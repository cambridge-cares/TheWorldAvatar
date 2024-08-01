package uk.ac.cam.cares.jps.accessagent.utils.test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.accessagent.utils.RouterUploadAgent;
import uk.ac.cam.cares.jps.accessagent.utils.RouterUploadTool;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.MockStoreClient;
import uk.ac.cam.cares.jps.base.query.StoreRouter;

class RouterUploadAgentTest {

	final String label1 = "test1";
	final String label2 = "test2";
	final String expectedAskQuery1 = "ASK WHERE{<"
			+RouterUploadTool.ONTOKGROUTER+label1+"> ?p ?o}";

	final String jsonArray = RouterUploadTestHelper.getJsonString(label1,label2);
	
	RouterUploadAgent agent = new RouterUploadAgent();
	
	@Test
	void testAgent() {
					
		StoreClientInterface storeClient = new MockStoreClient();
		
		RouterUploadAgent agent = Mockito.spy(RouterUploadAgent.class);
		Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
		Mockito.doReturn(storeClient).when(agent).getStoreClient(any(String.class));
		
		JSONObject testObj = new JSONObject();
		testObj.put(JPSConstants.METHOD, HttpPost.METHOD_NAME);
		testObj.put(JPSConstants.CONTENTTYPE,MediaType.APPLICATION_JSON.type);		
		testObj.put(JPSConstants.CONTENT, jsonArray);
		
		JSONObject result = agent.processRequestParameters(testObj);
		
		// verify validate input called with JSON parameters
		verify(agent).validateInput(testObj);

		// Should use default endpoint from the storerouter
		verify(agent).getStoreClient(StoreRouter.storeRouterEndpoint);
		
		//returned JSON object
		assertEquals("2 endpoint(s) uploaded.",result.get("result"));
		
		//assert data uploaded 
		JSONArray storeResult = storeClient.executeQuery(RouterUploadTestHelper.getTestQuery(label1));
		String expected = RouterUploadTestHelper.getTestQueryExpected(label1);
		assertEquals(expected,storeResult.toString());
		
		storeResult = storeClient.executeQuery(RouterUploadTestHelper.getTestQuery(label2));
		expected = RouterUploadTestHelper.getTestQueryExpected(label2);
		assertEquals(expected,storeResult.toString());
	}
	
	@Test
	void testAgentWithSuppliedEndpoint() {
		
		String testEndpoint = "http://www.example.com/test/sparql";
		
		StoreClientInterface storeClient = new MockStoreClient();
		
		RouterUploadAgent agent = Mockito.spy(RouterUploadAgent.class);
		Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
		Mockito.doReturn(storeClient).when(agent).getStoreClient(any(String.class));
		Mockito.doReturn(0).when(agent).uploadTriples(any(JSONArray.class), any(TripleStoreClientInterface.class));
		
		JSONObject testObj = new JSONObject();
		testObj.put(JPSConstants.METHOD, HttpPost.METHOD_NAME);
		testObj.put(JPSConstants.CONTENTTYPE,MediaType.APPLICATION_JSON.type);		
		testObj.put(JPSConstants.CONTENT, jsonArray);
		testObj.put("routerEndpoint", testEndpoint);
		
		agent.processRequestParameters(testObj);
		
		// verify get store client is called with supplied endpoint
		verify(agent).getStoreClient(testEndpoint);
	}

	@Test
	void testValidateInputValid() {
		
		JSONObject testObj = new JSONObject();
		testObj.put(JPSConstants.METHOD, HttpPost.METHOD_NAME);
		testObj.put(JPSConstants.CONTENTTYPE,MediaType.APPLICATION_JSON.type);		
		testObj.put(JPSConstants.CONTENT, jsonArray);
				
		assertTrue(agent.validateInput(testObj));
	}
	
	@Test
	void testValidateInputInvalidMethod() {
		
		JSONObject testObj = new JSONObject();
		testObj.put(JPSConstants.METHOD, HttpGet.METHOD_NAME);
		testObj.put(JPSConstants.CONTENTTYPE,MediaType.APPLICATION_JSON.type);		
		testObj.put(JPSConstants.CONTENT, jsonArray);
		
		//HTTP GET
		assertFalse(agent.validateInput(testObj));
		
		//HTTP PUT
		testObj.put(JPSConstants.METHOD, HttpPut.METHOD_NAME);
		assertFalse(agent.validateInput(testObj));
	}
	
	@Test
	void testValidateInputInvalidContent() {
		
		JSONObject testObj = new JSONObject();
		testObj.put(JPSConstants.METHOD, HttpPost.METHOD_NAME);
		testObj.put(JPSConstants.CONTENTTYPE,MediaType.APPLICATION_JSON.type);		
		
		String invalidJsonArray = null;
		
		//not json array
		invalidJsonArray =  "{\n"+
				"	\"label\": \"label1\",\n"+
				"	\"queryEndpoint\": \"http://www.example.com/test/sparql\",\n"+
				"	\"updateEndpoint\": \"http://www.example.com/test/sparql\"\n"+
				"}";
		testObj.put(JPSConstants.CONTENT, invalidJsonArray);
		assertFalse(agent.validateInput(testObj));
		
		//label missing
		invalidJsonArray =  "[\n{\n"+
				"	\"queryEndpoint\": \"http://www.example.com/test/sparql\",\n"+
				"	\"updateEndpoint\": \"http://www.example.com/test/sparql\"\n"+
				"}\n]";
		testObj.put(JPSConstants.CONTENT, invalidJsonArray);
		assertFalse(agent.validateInput(testObj));
		
		//queryEndpooint missing
		invalidJsonArray =  "[\n{\n"+
				"	\"label\": \"label1\",\n"+
				"	\"updateEndpoint\": \"http://www.example.com/test/sparql\"\n"+
				"}\n]";
		testObj.put(JPSConstants.CONTENT, invalidJsonArray);
		assertFalse(agent.validateInput(testObj));
		
		//updateEndpooint missing
		invalidJsonArray =  "[\n{\n"+
				"	\"label\": \"label1\",\n"+
				"	\"queryEndpoint\": \"http://www.example.com/test/sparql\"\n"+
				"}\n]";
		testObj.put(JPSConstants.CONTENT, invalidJsonArray);
		assertFalse(agent.validateInput(testObj));
	}
	
}
