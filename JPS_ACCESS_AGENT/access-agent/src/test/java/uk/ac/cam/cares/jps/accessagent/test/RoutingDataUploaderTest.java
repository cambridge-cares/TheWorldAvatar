package uk.ac.cam.cares.jps.accessagent.test;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.http.client.methods.HttpPost;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.accessagent.RoutingDataUploader;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.MediaType;

class RoutingDataUploaderTest {

	@Test
	void test() {
		
		JSONArray jsonArray = new JSONArray(jsonString());	
		JSONObject testObj = new JSONObject();
	
		testObj.put("routerEndpoint", "http://localhost:48889/blazegraph/namespace/kb/sparql");
		testObj.put(JPSConstants.METHOD, HttpPost.METHOD_NAME);
		testObj.put(JPSConstants.CONTENTTYPE,MediaType.APPLICATION_JSON.type);		
		testObj.put(JPSConstants.CONTENT, jsonArray);
		
		RoutingDataUploader uploader = new RoutingDataUploader();
		JSONObject result = uploader.processRequestParameters(testObj);
		
	}

	String jsonString() {
		return "[\n"+
					"{\n"+
					"	\"label\": \"ontokin\",\n"+
					"	\"queryEndpoint\": \"http://www.theworldavatar.com/blazegraph/namespace/ontokin/sparql\",\n"+
					"	\"updateEndpoint\": \"http://www.theworldavatar.com/blazegraph/namespace/ontokin/sparql\"\n"+
					"},\n"+
					"{\n"+
					"	\"label\": \"ontokin2\",\n"+
					"	\"queryEndpoint\": \"http://www.theworldavatar.com/blazegraph/namespace/ontokin2/sparql\",\n"+
					"	\"updateEndpoint\": \"http://www.theworldavatar.com/blazegraph/namespace/ontokin2/sparql\"\n"+
					"}\n"+
				"]";
	}
	
}
