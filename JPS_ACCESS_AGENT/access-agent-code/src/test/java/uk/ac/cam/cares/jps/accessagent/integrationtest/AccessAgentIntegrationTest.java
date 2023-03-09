package uk.ac.cam.cares.jps.accessagent.integrationtest;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.StoreRouter;

/**
 * Integration tests for the (triple store) Access Agent using Test containers.
 *  
 * To test a new version of the AccessAgent:
 * 1. Build the new AccessAgent Docker image 
 * 	  if it doesn't already exist in the registry (see README) 
 * 2. Update the ACCESS_AGENT_VERSION variable to the new version number
 * 3. Run the integration tests.
 * 
 * @author csl37
 *
 */
@Disabled("Requires Testcontainers and Docker to run. The AccessAgent Docker image must be built.")
@Testcontainers
class AccessAgentIntegrationTest {

	////////////////////////////////////////////////
	
	//User defined variables
	//set the desired access agent version number here
	static final String ACCESS_AGENT_VERSION = "1.7.0";
	
	//////////////////////////////////////////////////
	
	static final String ACCESS_AGENT_IMAGE ="ghcr.io/cambridge-cares/access-agent:"+ACCESS_AGENT_VERSION; 
	static final String BLAZEGRAPH_IMAGE = "docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"; 
	static final int BLAZEGRAPH_INTERNAL_PORT = 9999;
	static final String TEST_NAMESPACE = "kb";
	
	//Put all containers on the same network
	static final Network NETWORK = Network.newNetwork();
	static final String STORE_ROUTER_CONTAINER_ALIAS = "store-router-container";
	static final String TARGET_STORE_CONTAINER_ALIAS = "target-store-container";
	
	static final String STORE_ROUTER_ENDPOINT = "http://" + STORE_ROUTER_CONTAINER_ALIAS  
			+ ":"+Integer.toString(BLAZEGRAPH_INTERNAL_PORT)+"/blazegraph/namespace/kb/sparql";
	
	//Create only one store router triple store and Access Agent for the entire test
	@Container
	static final GenericContainer<?> STORE_ROUTER_CONTAINER = new GenericContainer<>(DockerImageName.parse(BLAZEGRAPH_IMAGE))
												 .withExposedPorts(BLAZEGRAPH_INTERNAL_PORT)
												 .withNetwork(NETWORK)
												 .withNetworkAliases(STORE_ROUTER_CONTAINER_ALIAS);
	@Container
	static final GenericContainer<?> ACCESS_AGENT_CONTAINER = new GenericContainer<>(DockerImageName.parse(ACCESS_AGENT_IMAGE))
												.withExposedPorts(8080)
												.withEnv(StoreRouter.STOREROUTER_ENDPOINT_NAME,STORE_ROUTER_ENDPOINT)
												.withNetwork(NETWORK)
												.dependsOn(STORE_ROUTER_CONTAINER);
	
	//Create a new target store for each test
	@Container
	GenericContainer<?> targetStoreContainer = new GenericContainer<>(DockerImageName.parse(BLAZEGRAPH_IMAGE))
												 .withExposedPorts(BLAZEGRAPH_INTERNAL_PORT)
												 .withNetwork(NETWORK)
												 .withNetworkAliases(TARGET_STORE_CONTAINER_ALIAS);;
	
	RemoteStoreClient targetStoreClient;
	String targetStoreLabel;
	String targetResourceID;
	String targetStoreEndpointInternal;
	
	//////////////////////////////////////////////////
	
	@BeforeAll 
	static void setupAll() {
		try {
			STORE_ROUTER_CONTAINER.start();
			ACCESS_AGENT_CONTAINER.start();	
		} catch (Exception e) {
			throw new JPSRuntimeException("AccessAgentIntegrationTest: Docker container startup failed. Please try running tests again");
		}
	}
	
	@BeforeEach
	void setupEach() {
				
		try {	
			targetStoreContainer.start();					
		} catch (Exception e) {
			throw new JPSRuntimeException("AccessAgentIntegrationTest: Docker container startup failed. Please try running tests again");
		}
		
		//Upload routing information
		targetStoreEndpointInternal = "http://" + TARGET_STORE_CONTAINER_ALIAS 
				+ ":" + BLAZEGRAPH_INTERNAL_PORT
				+ "/blazegraph/namespace/"+TEST_NAMESPACE+"/sparql";
		
		String targetStoreEndpointExternal = "http://" + targetStoreContainer.getHost() 
		+ ":" + targetStoreContainer.getFirstMappedPort()
		+ "/blazegraph/namespace/"+TEST_NAMESPACE+"/sparql";
		
		targetStoreLabel = "kb"+targetStoreContainer.getFirstMappedPort();
		
		String uploadUrl = "http://" + ACCESS_AGENT_CONTAINER.getHost() 
		+ ":" + ACCESS_AGENT_CONTAINER.getFirstMappedPort()
		+ "/access-agent/upload";
		
		String uploaded = IntegrationTestHelper.uploadRoutingData(targetStoreLabel, targetStoreEndpointInternal, uploadUrl);
		assertTrue("AccessAgentIntegrationTest: Routing data upload failed.", uploaded.equals("1 endpoint(s) uploaded."));
				
		//Connect to target store and set targetResourceID
		targetStoreClient = new RemoteStoreClient(targetStoreEndpointExternal,targetStoreEndpointExternal);
		
		targetResourceID = "http://" + ACCESS_AGENT_CONTAINER.getHost() 
		+ ":" + ACCESS_AGENT_CONTAINER.getFirstMappedPort()
		+ "/" + targetStoreLabel;
		
	}
	
	@AfterEach
	public void stopEach() {
		if (targetStoreContainer.isRunning()) {
			targetStoreContainer.stop();
		}
	} 
	
	@AfterAll
	public static void stopAll(){
		if (STORE_ROUTER_CONTAINER.isRunning()) {
			STORE_ROUTER_CONTAINER.stop();
		}
		if (ACCESS_AGENT_CONTAINER.isRunning()) {
			ACCESS_AGENT_CONTAINER.stop();
		}
	}

	//////////////////////////////////////////////////
	// Integration tests
	
	// Test data 
	static final String testContentType = MediaType.APPLICATION_N_TRIPLES.type;
	static final String testContent = "<http://www.example.com/test/s> <http://www.example.com/test/p>	<http://www.example.com/test/o>.";
	static final String query = "SELECT ?o WHERE {<http://www.example.com/test/s> <http://www.example.com/test/p> ?o.}";
	
	@Test
	void testPostSparqlQuery() {
		//insert test data 
		targetStoreClient.insert(null, testContent, testContentType);
				
		JSONArray ja = AccessAgentCaller.queryStore(targetResourceID, query);
		 
		JSONObject jo = ja.getJSONObject(0); 
		assertEquals("http://www.example.com/test/o",jo.get("o").toString());
	}

	@Test
	void testPostSparqlUpdate() throws ParseException {
		
		//insert test data 
		targetStoreClient.insert(null, testContent, testContentType);
		
		AccessAgentCaller.updateStore(targetResourceID, IntegrationTestHelper.getUpdateRequest());
		JSONArray ja = targetStoreClient.executeQuery(query);
        JSONObject result = ja.getJSONObject(0); 
		assertEquals("TEST",result.get("o").toString());
	}
	
	@Test
	void testGet() {
		JSONObject result = AccessAgentCaller.getEndpoints(targetResourceID);
		String query = result.getString(JPSConstants.QUERY_ENDPOINT);
		String update = result.getString(JPSConstants.UPDATE_ENDPOINT);
		
		assertEquals(IntegrationTestHelper.removeWhiteSpace(targetStoreEndpointInternal),IntegrationTestHelper.removeWhiteSpace(query));
		assertEquals(IntegrationTestHelper.removeWhiteSpace(targetStoreEndpointInternal),IntegrationTestHelper.removeWhiteSpace(update));
	}
	
	@Test
	void testPut() {
		
		String newContent = "<http://www.example.com/test/s1>	<http://www.example.com/test/p1>	<http://www.example.com/test/o1>.";
		
        AccessAgentCaller.put(targetResourceID, null, newContent, testContentType);
        
        String result = targetStoreClient.get(null, testContentType);
		assertTrue(IntegrationTestHelper.removeWhiteSpace(result).contains(IntegrationTestHelper.removeWhiteSpace(newContent)));
	}
	
	@Test
	void testClearCache() {
		
		//insert test data 
		targetStoreClient.insert(null, testContent, testContentType);
		
		JSONArray ja = AccessAgentCaller.queryStore(targetResourceID, query);
		JSONObject jo = ja.getJSONObject(0); 
		assertEquals("http://www.example.com/test/o",jo.get("o").toString());
			
		String url = "http://" + ACCESS_AGENT_CONTAINER.getHost() 
			+ ":" + ACCESS_AGENT_CONTAINER.getFirstMappedPort() + "/access-agent/clearcache";
		JSONObject response = new JSONObject(Http.execute(Http.get(url,null)));		
		assertEquals("Cache cleared.",response.getString(JPSConstants.RESULT_KEY));
		
		ja = AccessAgentCaller.queryStore(targetResourceID, query);
		jo = ja.getJSONObject(0); 
		assertEquals("http://www.example.com/test/o",jo.get("o").toString());
	}
		
}
