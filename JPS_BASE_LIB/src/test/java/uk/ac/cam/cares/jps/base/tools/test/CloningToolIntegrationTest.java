package uk.ac.cam.cares.jps.base.tools.test;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.jena.arq.querybuilder.AskBuilder;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.tools.CloningTool;

@Testcontainers
class CloningToolIntegrationTest {
 
	static final int N_TEST_TRIPLES = 1000;
	
	static final String BLAZEGRAPH_IMAGE = "docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"; 
	static final int BLAZEGRAPH_INTERNAL_PORT = 9999;
	
	//Test triple template
	static final String s = "<http://www.example.com/s%s>";
	static final String p = "<http://www.example.com/p%s>";
	static final String o = "<http://www.example.com/o%s>";

	String testData;
	
	@Container
	GenericContainer<?> sourceContainer = new GenericContainer<>(DockerImageName.parse(BLAZEGRAPH_IMAGE))
												 .withExposedPorts(BLAZEGRAPH_INTERNAL_PORT);
	
	GenericContainer<?> targetContainer = new GenericContainer<>(DockerImageName.parse(BLAZEGRAPH_IMAGE))
			 .withExposedPorts(BLAZEGRAPH_INTERNAL_PORT);

	RemoteStoreClient sourceStoreClient;
	RemoteStoreClient targetStoreClient;	
	
	
	@BeforeEach
	void setup() {
		try {
			sourceContainer.start();
			targetContainer.start();	
		} catch (Exception e) {
			throw new JPSRuntimeException("CloningToolIntegrationTest: Docker container startup failed. Please try running tests again");
		}
		
		String sourceEndpoint = "http://" + sourceContainer.getHost() 
		+ ":" + sourceContainer.getFirstMappedPort()
		+ "/blazegraph/namespace/kb/sparql";
		
		sourceStoreClient = new RemoteStoreClient(sourceEndpoint,sourceEndpoint);
		
		String targetEndpoint = "http://" + targetContainer.getHost() 
		+ ":" + targetContainer.getFirstMappedPort()
		+ "/blazegraph/namespace/kb/sparql";
		
		targetStoreClient = new RemoteStoreClient(targetEndpoint,targetEndpoint);
		
		//Load test data 		
		testData = createTriples(N_TEST_TRIPLES);		
		sourceStoreClient.executeUpdate(createInsertData(testData));
		//check test data loaded
		assertEquals(N_TEST_TRIPLES,sourceStoreClient.getTotalNumberOfTriples()); 
	}
	
	static String createInsertData(String triples) {
		StringBuilder stringBuilder = new StringBuilder()
									.append("INSERT DATA {")
									.append(triples)
									.append("}");
		return stringBuilder.toString();
	}
	
	static String createTriples(int N) {
		
		StringBuilder stringBuilder = new StringBuilder();
	
		for(int i = 0; i < N; i++) {
			String si = String.format(s, Integer.toString(i)); 
			String pi = String.format(p, Integer.toString(i));
			String oi = String.format(o, Integer.toString(i));
			stringBuilder.append(si+pi+oi+".\n");
		}
		return stringBuilder.toString();
	}
	
	@AfterEach
	void stopContainers() {
		if (sourceContainer.isRunning()) {
			sourceContainer.stop();
		}
		if (targetContainer.isRunning()) {
			targetContainer.stop();
		}
	}
	
	@Test
	void test() {
		
		CloningTool cloningTool = new CloningTool(200,20);
		cloningTool.clone(sourceStoreClient, targetStoreClient);
		
		assertEquals(N_TEST_TRIPLES,targetStoreClient.getTotalNumberOfTriples());
		assertTrue(checkTriples(N_TEST_TRIPLES));
	}

	@Test
	void testWithBlanks() {
		
	}
	
	boolean checkTriples(int N) {
		
		boolean check = true;
		
		for(int i = 0; i < N; i++) {
		
			String si = String.format(s, Integer.toString(i));
			String pi = String.format(p, Integer.toString(i));
			String oi = String.format(o, Integer.toString(i));
			
	    	AskBuilder builder = new AskBuilder();
			builder.addWhere(si, pi, oi);
			String askQuery = builder.build().toString();
			String result = targetStoreClient.execute(askQuery);
			JSONObject obj =  new JSONArray(result).getJSONObject(0);
			check = (boolean) obj.get("ASK");
		}
		return check;
	}
	
}
