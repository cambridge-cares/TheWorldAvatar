package uk.ac.cam.cares.jps.base.tools.test;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.jena.arq.querybuilder.AskBuilder;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
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

	static final String BLAZEGRAPH_IMAGE = "docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"; 
	static final int BLAZEGRAPH_INTERNAL_PORT = 9999;
	
	@Container
	static final GenericContainer<?> SOURCE_CONTAINER = new GenericContainer<>(DockerImageName.parse(BLAZEGRAPH_IMAGE))
												 .withExposedPorts(BLAZEGRAPH_INTERNAL_PORT);
	
	static final GenericContainer<?> TARGET_CONTAINER = new GenericContainer<>(DockerImageName.parse(BLAZEGRAPH_IMAGE))
			 .withExposedPorts(BLAZEGRAPH_INTERNAL_PORT);

	static RemoteStoreClient sourceStoreClient;
	static RemoteStoreClient targetStoreClient;
	
	static int expectedCount;
	static String testData;
	
	@BeforeAll
	static void setup() {
		
		try {
			SOURCE_CONTAINER.start();
			TARGET_CONTAINER.start();	
		} catch (Exception e) {
			throw new JPSRuntimeException("CloningToolIntegrationTest: Docker container startup failed. Please try running tests again");
		}
		
		String sourceEndpoint = "http://" + SOURCE_CONTAINER.getHost() 
		+ ":" + SOURCE_CONTAINER.getFirstMappedPort()
		+ "/blazegraph/namespace/kb/sparql";
		
		sourceStoreClient = new RemoteStoreClient(sourceEndpoint,sourceEndpoint);
		
		String targetEndpoint = "http://" + TARGET_CONTAINER.getHost() 
		+ ":" + TARGET_CONTAINER.getFirstMappedPort()
		+ "/blazegraph/namespace/kb/sparql";
		
		targetStoreClient = new RemoteStoreClient(targetEndpoint,targetEndpoint);
		
		//Load test data 
		int i = 0;
		int j = 100;
		expectedCount = j-i;
		
		testData = createTriples(i,j);
		String testDataUpdate = createInsertData(testData);
		
		sourceStoreClient.executeUpdate(testDataUpdate);
		
		assertEquals(expectedCount,sourceStoreClient.getTotalNumberOfTriples()); //check test data loaded
	}
	
	static String createInsertData(String triples) {
		StringBuilder stringBuilder = new StringBuilder()
									.append("INSERT DATA {")
									.append(triples)
									.append("}");
		return stringBuilder.toString();
	}
	
	static String createTriples(int i, int j) {
		
		StringBuilder stringBuilder = new StringBuilder();
		
		for(int k = i; k < j; k++) {
			String s = "<http://www.example.com/s"+Integer.toString(k)+"> "; 
			String p = "<http://www.example.com/p"+Integer.toString(k)+"> ";
			String o = "<http://www.example.com/o"+Integer.toString(k)+">.\n";
			stringBuilder.append(s+p+o);
		}
		
		return stringBuilder.toString();
	}
	
	@AfterAll
	static void stopContainers() {
		if (SOURCE_CONTAINER.isRunning()) {
			SOURCE_CONTAINER.stop();
		}
		if (TARGET_CONTAINER.isRunning()) {
			TARGET_CONTAINER.stop();
		}
	}
	
	@Test
	void test() {
		
		CloningTool cloningTool = new CloningTool(20,5);
		cloningTool.clone(sourceStoreClient, targetStoreClient);
		
		assertEquals(expectedCount,targetStoreClient.getTotalNumberOfTriples());
		assertTrue(checkTriples(0,10));
	}

	boolean checkTriples(int i , int j) {
		
		boolean check = true;
		
		for(int k = i; k < j; k++) {
		
			String s = "<http://www.example.com/s"+Integer.toString(k)+">";
			String p = "<http://www.example.com/p"+Integer.toString(k)+">";
			String o = "<http://www.example.com/o"+Integer.toString(k)+">";
			
	    	AskBuilder builder = new AskBuilder();
			builder.addWhere(s, p, o);
			String askQuery = builder.build().toString();
			String result = targetStoreClient.execute(askQuery);
			JSONObject obj =  new JSONArray(result).getJSONObject(0);
			check = (boolean) obj.get("ASK");
		}
		
		return check;
	}
	
}
