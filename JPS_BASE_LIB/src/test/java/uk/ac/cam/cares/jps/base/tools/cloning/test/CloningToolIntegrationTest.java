package uk.ac.cam.cares.jps.base.tools.cloning.test;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.tools.cloning.CloningTool;

@Disabled("These integration tests are quite slow") 
@Testcontainers
class CloningToolIntegrationTest {
 
	static final int N_TEST_TRIPLES = 1000;
	
	static final String BLAZEGRAPH_IMAGE = "docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"; 
	static final int BLAZEGRAPH_INTERNAL_PORT = 9999;
	
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
		testData = CloningToolTestHelper.createTriples(N_TEST_TRIPLES);		
		sourceStoreClient.executeUpdate(CloningToolTestHelper.createInsertData(testData));
		//check test data loaded
		assertEquals(N_TEST_TRIPLES,sourceStoreClient.getTotalNumberOfTriples()); 
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
	void testClone() {
		
		CloningTool cloningTool = new CloningTool(400,40);
		cloningTool.clone(sourceStoreClient, targetStoreClient);
		
		assertEquals(N_TEST_TRIPLES,targetStoreClient.getTotalNumberOfTriples());
		assertTrue(CloningToolTestHelper.checkTriples(N_TEST_TRIPLES, targetStoreClient));
	}

	@Test
	void testCloneWithBlanks() {
		
		int nBlankTriples = 4;
		String insertBlanks = "INSERT DATA{"
				+ "<sblank0> <pblank0> _:b0 .\n"
				+ "_:b0 <pblank1> <oblank1> .\n"
				+ "_:b2 <pblank2> <oblank2> .\n"
				+ "<sblank3> <pblank3> _:b3 .}";
		sourceStoreClient.executeUpdate(insertBlanks);
		
		WhereBuilder where = new WhereBuilder()
				.addWhere("<sblank0>", "<pblank0>", "_:b0")
				.addWhere("_:b0", "<pblank1>", "<oblank1>")
				.addWhere("_:b2", "<pblank2>", "<oblank2>")
				.addWhere("<sblank3>", "<pblank3>", "_:b3");
				
		CloningTool cloningTool = new CloningTool(400,40);
		cloningTool.clone(sourceStoreClient, targetStoreClient);
		
		assertEquals(N_TEST_TRIPLES+nBlankTriples,targetStoreClient.getTotalNumberOfTriples());
		assertTrue(CloningToolTestHelper.checkTriples(N_TEST_TRIPLES, targetStoreClient));
		assertTrue(CloningToolTestHelper.checkSingleTriple(targetStoreClient, where));
	}

	@Test
	void testStoreClientCloneToFail() {
		assertThrows(JPSRuntimeException.class, ()->{targetStoreClient.cloneTo(sourceStoreClient);});
	}
	
	@Test
	void testStoreClientCloneTo() {
		
		sourceStoreClient.cloneTo(targetStoreClient);
		
		assertEquals(N_TEST_TRIPLES,targetStoreClient.getTotalNumberOfTriples());
		assertTrue(CloningToolTestHelper.checkTriples(N_TEST_TRIPLES, targetStoreClient));
	}

	@Test
	void testStoreClientCloneFromFail() {
		assertThrows(JPSRuntimeException.class, ()->{sourceStoreClient.cloneFrom(targetStoreClient);});	
	}
	
	@Test
	void testStoreClientCloneFrom() {
		
		targetStoreClient.cloneFrom(sourceStoreClient);	
		
		assertEquals(N_TEST_TRIPLES,targetStoreClient.getTotalNumberOfTriples());
		assertTrue(CloningToolTestHelper.checkTriples(N_TEST_TRIPLES, targetStoreClient));
	}
}
