package uk.ac.cam.cares.jps.accessagent.utils.test;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.http.client.methods.HttpPost;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.accessagent.utils.RouterUploadAgent;
import uk.ac.cam.cares.jps.accessagent.utils.RouterUploadTool;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.MediaType;

class RouterUploadAgentTest {

	final String label1 = "test1";
	final String label2 = "test2";
	final String expectedAskQuery1 = "ASK WHERE{<"
			+RouterUploadTool.ONTOKGROUTER+label1+"> ?p ?o}";

	final JSONArray jsonArray = new JSONArray(RouterUploadTestHelper.getJsonString(label1,label2));
	
	RouterUploadAgent agent = new RouterUploadAgent();
	
	@Test
	void testAgent() {
					
		JSONObject testObj = new JSONObject();
		testObj.put("routerEndpoint", "http://localhost:48889/blazegraph/namespace/kb/sparql");
		testObj.put(JPSConstants.METHOD, HttpPost.METHOD_NAME);
		testObj.put(JPSConstants.CONTENTTYPE,MediaType.APPLICATION_JSON.type);		
		testObj.put(JPSConstants.CONTENT, jsonArray.toString());
		
		agent.processRequestParameters(testObj);
	}

	@Test
	void testValidateInput() {
		
		JSONObject testObj = new JSONObject();
		testObj.put(JPSConstants.METHOD, HttpPost.METHOD_NAME);
		testObj.put(JPSConstants.CONTENTTYPE,MediaType.APPLICATION_JSON.type);		
		testObj.put(JPSConstants.CONTENT, jsonArray.toString());
		
		
		agent.validateInput(null);
	}
	
	//TODO: post/not post, json array, improper json array
	
}
