package uk.ac.cam.cares.jps.accessagent.integrationtest;

import static org.junit.Assert.*;

import java.io.IOException;

import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.testcontainers.containers.FixedHostPortGenericContainer;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.Wait;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import uk.ac.cam.cares.jps.accessagent.AccessAgent;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.StoreRouter;

/**
 * Integration tests for the AccessAgentCaller, AccessAgent, StoreRouter and RemoteStoreClient.
 * <p> These tests require the AccessAgent and Blazegraph to be running on the localhost.
 * The local Blazegraph needs to have the test triple store accessible at <i>sparqlEndpoint</i>.  
 * The OntoKGRouter triple store must be accessible on the www.theworldavatar.com and contain 
 * routing information consistent with the variables <i>datasetIRI</i> and <i>sparqlEndpoint</i>. 
 * <p> The tests can be run by commenting out the @Ignore annotation
 * @author csl37
 *
 */
//@Ignore("Requires the AccessAgent and Blazegraph with the namespace teststorelocal to be deployed and running on localhost. Requires Internet access to OntoKGRouter on www.theworldavatar.com.")
@Testcontainers
public class AccessAgentRemoteStoreIntegrationTest {

	// Test content and content type, n-triples
	static String rdfcontent = "<http://www.example.com/test/s> <http://www.example.com/test/p>	<http://www.example.com/test/o>.";
	static String contentType = MediaType.APPLICATION_N_TRIPLES.type;
	
	// Test query
	static String query = "SELECT ?o WHERE {<http://www.example.com/test/s> <http://www.example.com/test/p> ?o.}";

	// Target dataset IRI and corresponding sparql endpoint returned by the StoreRouter
	static String datasetIRI = "http://localhost:48080/kb/teststorelocal";
	static String sparqlEndpoint = "http://localhost:39888/blazegraph/namespace/kb/sparql";
	static String dockerSparqlEndpoint = "http://172.17.0.1:39888/blazegraph/namespace/kb/sparql";
	RemoteStoreClient storeClient = null;
	
	// Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
	// For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
	// The endpoint url port must be fixed in the ontokgrouter triple store so we use a fixed host port container.
	@Container
	@SuppressWarnings("deprecation")	  
	private GenericContainer<?> blazegraph = new FixedHostPortGenericContainer<>("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0")
												.withFixedExposedPort(39888, 9999);
	
	///////////////////////////////////////////////////////////////
	// Initial checks to ensure that the testing environment 
	// is set up correctly.
	///////////////////////////////////////////////////////////////
	
	@BeforeAll
	public static void initialChecks() {
		checkOntoKGRouter();
		checkStoreRouter();
	}
	
	/**
	 * Perform initial check to ensure that the StoreRouter is able to connect 
	 * to the OntoKGRouter triple store and get the correct test endpoint.
	 */
	public static void checkOntoKGRouter() {
		
		String queryQueryEndpoint =  "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/ontokgrouter/teststorelocal> <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint> ?o.}";
		String updateQueryEndpoint =  "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/ontokgrouter/teststorelocal> <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint> ?o.}";
		
		// Get the OntoKGRouter endpoint from the StoreRouter class
		String ontokgrouterEndpoint = TestHelper.getRouterEndpoint();
		assertNotNull("Failed to get OntoKGRouter endpoint!",ontokgrouterEndpoint);
		
		RemoteStoreClient ontokgrouter = new RemoteStoreClient(ontokgrouterEndpoint);
		try {
			// Query the OntoKGRouter to check that the store contains the correct sparql endpoints 
			JSONObject result1 = ontokgrouter.executeQuery(queryQueryEndpoint).getJSONObject(0);
			assertEquals("OntoKGRouter did not return the test sparql endpoint!",dockerSparqlEndpoint,result1.get("o").toString()); 
			JSONObject result2 = ontokgrouter.executeQuery(updateQueryEndpoint).getJSONObject(0);
			assertEquals("OntoKGRouter did not return the test sparql endpoint!",dockerSparqlEndpoint,result2.get("o").toString());
		}catch(RuntimeException e) {
			fail("Failed to connect to OntoKGRouter triple store!");
			e.printStackTrace();
		}
	}
	
	/**
	 * Perform initial check to ensure the StoreRouter instantiates a RemoteStoreClient 
	 * with the correct endpoint.
	 */
	public static void checkStoreRouter() {
		
		String shortIRI = AccessAgent.getShortIRI(datasetIRI);
		StoreClientInterface storeClient = StoreRouter.getStoreClient(shortIRI, true, true);
		
		// Is a RemoteStoreClient
		assertEquals(RemoteStoreClient.class, storeClient.getClass());

		// Check endpoints
		assertEquals(dockerSparqlEndpoint,storeClient.getQueryEndpoint());
		assertEquals(dockerSparqlEndpoint,storeClient.getUpdateEndpoint());
	}
	
	///////////////////////////////////////////////////////////////
	// Set up and clean up test environment 
	///////////////////////////////////////////////////////////////
	
	/**
	 * Clear test store client and add test triples
	 */
	@BeforeEach
	public synchronized void setupTestStore() {
		
		try {
			// Start Blazegraph container
			blazegraph.start();
		} catch (Exception e) {
			throw new JPSRuntimeException("AccessAgentRemoteStoreIntegrationTest: Docker container startup failed. Please try running tests again");
		}
		
		//TODO loop
		//wait for container start up
		try {
			wait(10000);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	
		storeClient = new RemoteStoreClient(sparqlEndpoint, sparqlEndpoint);
		try {
			storeClient.insert(null, rdfcontent, contentType);
		}catch(RuntimeException e) {
			fail("AccessAgentRemoteStoreIntegrationTest: Failed to connect to test remote store client: "+sparqlEndpoint);
		}
	}
	
	@AfterEach
	public void closeTestStore() {
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
	}
				
	///////////////////////////////////////////////////////////////
	// Integration tests
	///////////////////////////////////////////////////////////////
	
	@Test
	public void testGet() {
		
		String result = AccessAgentCaller.get(datasetIRI, null, contentType);
		JSONObject jo = new JSONObject(result);
		String result2 = jo.getString("result");
		assertEquals(TestHelper.removeWhiteSpace(rdfcontent), TestHelper.removeWhiteSpace(result2));
	}
	
	@Test
	public void testGetWithQuery() {
				
		String result = AccessAgentCaller.query(datasetIRI, null, query);
		JSONObject jo = new JSONObject(result);
		String result2 = jo.getString("result");
				
		JSONArray ja = new JSONArray(result2); 
		jo = ja.getJSONObject(0); 
		assertEquals("http://www.example.com/test/o",jo.get("o").toString());
	}
	
	@Test
	public void testPut() {

		String rdfcontentnew = "<http://www.example.com/test/s1>	<http://www.example.com/test/p1>	<http://www.example.com/test/o1>.";
		
        AccessAgentCaller.put(datasetIRI, null, rdfcontentnew, contentType);
        
        String result = storeClient.get(null, contentType);
		assertTrue(TestHelper.removeWhiteSpace(result).contains(TestHelper.removeWhiteSpace(rdfcontent)));
	}
	
	@Test
	public void testPost() throws ParseException, IOException {
				
		//Update
		AccessAgentCaller.update(datasetIRI, null, TestHelper.getUpdateRequest());
		
        JSONArray ja = storeClient.executeQuery(query);
        JSONObject result = ja.getJSONObject(0); 
		assertEquals("TEST",result.get("o").toString()); 
	}
}
