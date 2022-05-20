package uk.ac.cam.cares.jps.accessagent.integrationtest;

import static org.junit.Assert.*;

import java.io.IOException;

import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.testcontainers.containers.FixedHostPortGenericContainer;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.StoreRouter;

/**
 * Integration tests for the AccessAgentCaller, AccessAgent, StoreRouter and RemoteStoreClient.
 * <p> These tests require the AccessAgent to be running locally on Docker and built with the correct 
 * url.storerouter.endpoint in accessagent.properties
 * Testcontainers is used create a Blazegraph test store.  
 * <p> The tests can be run by commenting out the @Disabled annotation.
 * @author csl37
 *
 */
@Disabled("Requires the AccessAgent to be running locally on Docker and internet access to OntoKGRouter on www.theworldavatar.com.")
@Testcontainers
public class AccessAgentRemoteStoreIntegrationTest {

	///////////////////////////////////////////////////////////////
	// IMPORTANT VARIABLES
	///////////////////////////////////////////////////////////////
	
	//Access agent address. Assume Docker container exposes port 48080
	static final String accessagentHost = "http://localhost:48080/";
	
	//Test store router endpoint. 
	//NOTE: the routerEndpoint must match url.storerouter.endpoint in accessagent.properties
	//http://host.docker.internal:39889/blazegraph/namespace/kb/sparql
	static final int routerPort = 39889;
	static final String routerEndpoint = "http://localhost:"+String.valueOf(routerPort)+"/blazegraph/namespace/kb/sparql";
	
	///////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////
	
	// Test Store Container, StoreClient and endpoints
	// Create a Docker container with Blazegraph image from CMCL registry (image uses port 9999)
	// For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
	// The endpoint url port must be fixed in the ontokgrouter triple store so we use a fixed host port container.
	@Container
	@SuppressWarnings("deprecation")	  
	private GenericContainer<?> blazegraph = new FixedHostPortGenericContainer<>("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0")
												.withFixedExposedPort(39888, 9999);
	RemoteStoreClient testStoreClient = null;
	static final String testStoreEndpoint = "http://localhost:39888/blazegraph/namespace/kb/sparql";
	static final String dockerTestStoreEndpoint = testStoreEndpoint.replace("localhost","host.docker.internal");
	
	// Router container, StoreClient and Docker endpoint
	@Container
	@SuppressWarnings("deprecation")	  
	private GenericContainer<?> blazegraphRouter = new FixedHostPortGenericContainer<>("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0")
												.withFixedExposedPort(routerPort, 9999);
	RemoteStoreClient routerStoreClient = null;
	static final String dockerRouterEndpoint = routerEndpoint.replace("localhost","host.docker.internal");
	
	///////////////////////////////////////////////////////////////
	
	//Test store target IRI
	static final String targetNamespace = "teststorelocal";
	static final String targetIRI = accessagentHost + targetNamespace;
	
	// Test content and content type, n-triples
	static final String contentType = MediaType.APPLICATION_N_TRIPLES.type;
	static final String rdfcontent = "<http://www.example.com/test/s> <http://www.example.com/test/p>	<http://www.example.com/test/o>.";
	static final String routerContent = getRouterContent();
	
	// Test query
	static final String query = "SELECT ?o WHERE {<http://www.example.com/test/s> <http://www.example.com/test/p> ?o.}";
	
	private static String getRouterContent() {
		return  "<http://www.theworldavatar.com/kb/ontokgrouter/teststorelocal>	<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint>	\""+dockerTestStoreEndpoint+"\".\n"+
				"<http://www.theworldavatar.com/kb/ontokgrouter/teststorelocal>	<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint> \""+dockerTestStoreEndpoint+"\".\n"+
				"<http://www.theworldavatar.com/kb/ontokgrouter/teststorelocal>	<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>	<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#TargetResource>.\n"+
				"<http://www.theworldavatar.com/kb/ontokgrouter/teststorelocal>	<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>	<http://www.w3.org/2002/07/owl#NamedIndividual>.\n"+
				"<http://www.theworldavatar.com/kb/ontokgrouter/teststorelocal>	<http://www.w3.org/2000/01/rdf-schema#label>	\""+targetNamespace+"\".\n";		
	}
		
	///////////////////////////////////////////////////////////////
	// Set up and clean up test environment 
	///////////////////////////////////////////////////////////////
	
	/**
	 * Clear test store client and add test triples
	 */	
	@BeforeEach
	public synchronized void setup() {
		testStoreClient = setupStore(blazegraph, testStoreEndpoint, rdfcontent, contentType);
		routerStoreClient = setupStore(blazegraphRouter, routerEndpoint, routerContent, contentType);
		
		// check the Docker containers are working
		checkOntoKGRouter();
		checkStoreRouter();
	}

	@AfterEach
	public void closeTestStore() {
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
		if(blazegraphRouter.isRunning()) {
			blazegraphRouter.stop();
		}
	}
	
	private synchronized RemoteStoreClient setupStore(GenericContainer<?> container, String endpoint, String content, String contentType) {
		
		try {
			// Start Blazegraph container
			container.start();
		} catch (Exception e) {
			throw new JPSRuntimeException("AccessAgentRemoteStoreIntegrationTest: Docker container startup failed. Please try running tests again");
		}
		
		RemoteStoreClient storeClient = new RemoteStoreClient(endpoint, endpoint);
		
		// The Blazegraph container can take some time to start up in Docker. 
		// We make 5 attempts to connect, waiting 5s between attempts.  
		int maxi = 5;
		for (int i = 1; i<=maxi; i++) {
			//wait 5s for container start up
			try {
				wait(5000);
			} catch (InterruptedException e) {
				throw new JPSRuntimeException(e);
			}
			
			//make up to 5 attempts to connect
			try {
				storeClient.insert(null, content, contentType);
				System.out.println("Attempt:"+i+"/"+maxi+". Successfully connected to test store.");
				break;
			}catch(RuntimeException e) {
				if(i<maxi) {
					System.out.println("Attempt:"+i+"/"+maxi+". Failed to connect to test store. Trying again.");
				}else {
					fail("AccessAgentRemoteStoreIntegrationTest: Failed to connect to test store: "+ endpoint);
				}
			}
		}
		
		return storeClient;
	}
		
	///////////////////////////////////////////////////////////////
	// Initial checks to ensure that the testing environment 
	// is set up correctly.
	///////////////////////////////////////////////////////////////
		
	/**
	* Perform initial check to ensure that the StoreRouter is able to connect 
	* to the OntoKGRouter triple store and get the correct test endpoint.
	*/
	public static void checkOntoKGRouter() {
	
		String queryQueryEndpoint =  "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/ontokgrouter/teststorelocal> <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint> ?o.}";
		String updateQueryEndpoint =  "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/ontokgrouter/teststorelocal> <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint> ?o.}";
		
		RemoteStoreClient ontokgrouter = new RemoteStoreClient(routerEndpoint);
		try {
			// Query the OntoKGRouter to check that the store contains the correct sparql endpoints 
			JSONObject result1 = ontokgrouter.executeQuery(queryQueryEndpoint).getJSONObject(0);
			assertEquals("OntoKGRouter did not return the test sparql endpoint!",dockerTestStoreEndpoint,result1.get("o").toString()); 
			JSONObject result2 = ontokgrouter.executeQuery(updateQueryEndpoint).getJSONObject(0);
			assertEquals("OntoKGRouter did not return the test sparql endpoint!",dockerTestStoreEndpoint,result2.get("o").toString());
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
	
		StoreRouter.setRouterEndpoint(routerEndpoint);
		StoreClientInterface storeClient = StoreRouter.getStoreClient(targetIRI, true, true);
		
		// Is a RemoteStoreClient
		assertEquals(RemoteStoreClient.class, storeClient.getClass());
		
		// Check endpoints
		assertEquals(dockerTestStoreEndpoint,storeClient.getQueryEndpoint());
		assertEquals(dockerTestStoreEndpoint,storeClient.getUpdateEndpoint());
	}
	
	///////////////////////////////////////////////////////////////
	// Integration tests
	///////////////////////////////////////////////////////////////
	
	@Test
	public void testGet() {
		
		String result = AccessAgentCaller.get(targetIRI, null, contentType);
		JSONObject jo = new JSONObject(result);
		String result2 = jo.getString("result");
		assertEquals(TestHelper.removeWhiteSpace(rdfcontent), TestHelper.removeWhiteSpace(result2));
	}
		
	@Test
	public void testPut() {

		String rdfcontentnew = "<http://www.example.com/test/s1>	<http://www.example.com/test/p1>	<http://www.example.com/test/o1>.";
		
        AccessAgentCaller.put(targetIRI, null, rdfcontentnew, contentType);
        
        String result = testStoreClient.get(null, contentType);
		assertTrue(TestHelper.removeWhiteSpace(result).contains(TestHelper.removeWhiteSpace(rdfcontent)));
	}
	
	@Test
	public void testPostWithUpdate() throws ParseException, IOException {
				
		//Update
		AccessAgentCaller.updateStore(targetIRI, TestHelper.getUpdateRequest());
		
        JSONArray ja = testStoreClient.executeQuery(query);
        JSONObject result = ja.getJSONObject(0); 
		assertEquals("TEST",result.get("o").toString()); 
	}
	
	@Test
	public void testPostWithQuery() {
				
		JSONArray ja = AccessAgentCaller.queryStore(targetIRI, query);
		 
		JSONObject jo = ja.getJSONObject(0); 
		assertEquals("http://www.example.com/test/o",jo.get("o").toString());
	}
}
