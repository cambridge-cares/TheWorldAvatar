package uk.ac.cam.cares.jps.scenarios.test;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.json.JSONObject;
import org.junit.Test;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.scenario.ScenarioAccessAgent;
import uk.ac.cam.cares.jps.scenario.ScenarioLog;
import uk.ac.cam.cares.jps.scenario.ScenarioStoreClient;

public class ScenarioAccessAgentTest {

	
	String scenarioName = "testScenario";
	String scenarioResource = "http://example.com/test/scenarioResource.owl";
	String accept = "application/n-triples";
	
	String scenarioUrl;			
	
	private JSONObject createRequestParams() {
		
		scenarioUrl = ScenarioAccessAgent.getScenarioUrl(scenarioName);
		
		JSONObject requestParams;
		requestParams = new JSONObject()
				.put(JPSConstants.METHOD, HttpGet.METHOD_NAME)
				.put(JPSConstants.PATH, "/"+scenarioName)
				.put(JPSConstants.REQUESTURL, scenarioUrl)
				.put(JPSConstants.SCENARIO_RESOURCE, scenarioResource)
				.put(JPSConstants.SCENARIO_DATASET, "test")
				.put(JPSConstants.CONTENTTYPE, accept)
				.put(JPSConstants.HEADERS, accept)
				.put(JPSConstants.TARGETIRI, "test");
		
		return requestParams;
	}
	
	@Test
	 public void testScenarioAccessAgent() {
		ScenarioAccessAgent agent = null;
        try {
            agent = new ScenarioAccessAgent();
        } finally {
            assertNotNull(agent);
        }
    }
	
	@Test
	public void testProcessRequestParameters() {
		
		ScenarioAccessAgent agent = Mockito.spy(ScenarioAccessAgent.class);
		Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
		Mockito.doReturn(null).when(agent).get(any(JSONObject.class));
		Mockito.doNothing().when(agent).put(any(JSONObject.class));
		Mockito.doNothing().when(agent).post(any(JSONObject.class));
		
		JSONObject requestParams;
		
		//test http get
		requestParams = new JSONObject();
		requestParams.put(JPSConstants.METHOD, HttpGet.METHOD_NAME);
		agent.processRequestParameters(requestParams, null);
		verify(agent).validateInput(requestParams);
		verify(agent).get(requestParams);
		
		//test http put
		requestParams = new JSONObject();
		requestParams.put(JPSConstants.METHOD, HttpPut.METHOD_NAME);
		agent.processRequestParameters(requestParams, null);
		verify(agent).validateInput(requestParams);
		verify(agent).put(requestParams);
		
		//test http post
		requestParams = new JSONObject();
		requestParams.put(JPSConstants.METHOD, HttpPost.METHOD_NAME);
		agent.processRequestParameters(requestParams, null);
		verify(agent).validateInput(requestParams);
		verify(agent).post(requestParams);
	}
	
	
	
	@Test
	public void testGetScenarioUrl() {
		String scenarioUrl = ScenarioAccessAgent.getScenarioUrl(scenarioName);
		assertNotNull(scenarioUrl);
		assertTrue(scenarioUrl.contains(scenarioName));
	}

	@Test 
	public void testGet() {
		
		String getOrQuery = "getOrQueryCalled";
		String call = "callCalled";
		
		ScenarioAccessAgent agent = Mockito.spy(ScenarioAccessAgent.class);
		Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
		Mockito.doReturn(getOrQuery).when(agent).getOrQuery(any(JSONObject.class), any(String.class), any(boolean.class));
		Mockito.doReturn(call).when(agent).call(any(JSONObject.class),any(String.class),any(ScenarioLog.class));
		
		JSONObject requestParams = createRequestParams();
		JSONObject result;
		
		//No operation
		result = agent.get(requestParams);
		verify(agent).get(requestParams);
		verify(agent).getOrQuery(any(JSONObject.class), any(String.class), any(boolean.class));
		assertEquals(getOrQuery,result.getString("result"));
		
		//Operations
		requestParams.put(JPSConstants.PATH, "/"+scenarioName+"/call");
		requestParams.put(JPSConstants.REQUESTURL, scenarioUrl+"/call");
		result = agent.get(requestParams);
		verify(agent,Mockito.times(2)).get(requestParams);
		verify(agent).call(any(JSONObject.class),any(String.class),any(ScenarioLog.class));
		assertEquals(call,result.getString("result"));
	}
	
	/**
	 * Test getFromKnowledgeBase is called when no sparql query is provided.
	 */
	@Test
	public void testGetOrQuery() {
		
		String getKG = "getFromKG";
		String queryKG = "queryKG";
		
		ScenarioAccessAgent agent = Mockito.spy(ScenarioAccessAgent.class);
		Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
		Mockito.doReturn(getKG).when(agent).getFromKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class),any(String.class));
		Mockito.doReturn(queryKG).when(agent).queryKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class));
		
		JSONObject requestParams = createRequestParams();
		
		String result = agent.getOrQuery(requestParams,scenarioName,false);
		
		verify(agent).getFromKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class),any(String.class));
		verify(agent,Mockito.times(0)).queryKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class));
		assertEquals(result,getKG);
	}
	
	/**
	 * Test queryKnowledgeBase is called when a sparql query is provided. 
	 */
	@Test
	public void testGetOrQueryWithSparqlQuery() {
		
		String getKG = "getFromKG";
		String queryKG = "queryKG";
		
		ScenarioAccessAgent agent = Mockito.spy(ScenarioAccessAgent.class);
		Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
		Mockito.doReturn(getKG).when(agent).getFromKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class),any(String.class));
		Mockito.doReturn(queryKG).when(agent).queryKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class));
		
		JSONObject requestParams = createRequestParams();	
		requestParams.put(JPSConstants.QUERY_SPARQL_QUERY, "test");
		
		String result;
		result = agent.getOrQuery(requestParams,scenarioName,false);
		
		verify(agent,Mockito.times(0)).getFromKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class),any(String.class));
		verify(agent).queryKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class));
		assertEquals(result,queryKG);
	}
	
	/**
	 * Test exception is thrown if a sparql update is given in Http Get request. 
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testGetOrQueryWithSparqlUpdate() {
				
		ScenarioAccessAgent agent = Mockito.spy(ScenarioAccessAgent.class);
		Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
		
		JSONObject requestParams  = createRequestParams();
		requestParams.put(JPSConstants.QUERY_SPARQL_UPDATE, "test");
		
		agent.getOrQuery(requestParams,scenarioName,false);		
	}
	
	/**
	 * Test get from existing file in scenario folder
	 */
	@Test
	public void testGetFromKnowledgeBaseFileExists() {
		
		String getReturn = "getCalled!";
		ScenarioStoreClient storeClient = Mockito.mock(ScenarioStoreClient.class);
		Mockito.when(storeClient.exists(scenarioResource)).thenReturn(true);
		Mockito.when(storeClient.get(scenarioResource, accept)).thenReturn(getReturn);
		 
		ScenarioAccessAgent agent = new ScenarioAccessAgent();
		String result = agent.getFromKnowledgeBase(storeClient, (String) null, scenarioResource, false, accept);
		 
		assertEquals(getReturn,result);
		verify(storeClient).exists(scenarioResource);
		verify(storeClient).get(scenarioResource, accept);
		verify(storeClient, Mockito.times(0)).put(any(String.class),any(String.class),any(String.class));
	}

	@Test
	public void testGetDatasetUrl() {
		String testUrl = "http://localhost:8080/kb/testDataset/test.owl";
		String result = ScenarioAccessAgent.getDatasetUrl(testUrl);
		assertEquals("http://localhost:8080/kb/testDataset",result);
		
		testUrl = "http://localhost:8080/kb/testDataset";
		result = ScenarioAccessAgent.getDatasetUrl(testUrl);
		assertEquals("http://localhost:8080/kb/testDataset",result);
		
		testUrl = "http://localhost:8080/testDataset";
		result = ScenarioAccessAgent.getDatasetUrl(testUrl);
		assertEquals("http://localhost:8080/testDataset",result);
	}
}
