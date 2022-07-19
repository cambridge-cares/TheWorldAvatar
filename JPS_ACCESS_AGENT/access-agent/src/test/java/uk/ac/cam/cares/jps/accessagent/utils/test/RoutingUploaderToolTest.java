package uk.ac.cam.cares.jps.accessagent.utils.test;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.http.client.methods.HttpPost;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.accessagent.utils.RoutingUploaderTool;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.query.MockStoreClient;

class RoutingUploaderToolTest {

	/*
	JSONObject testObj = new JSONObject();

	testObj.put("routerEndpoint", "http://localhost:48889/blazegraph/namespace/kb/sparql");
	testObj.put(JPSConstants.METHOD, HttpPost.METHOD_NAME);
	testObj.put(JPSConstants.CONTENTTYPE,MediaType.APPLICATION_JSON.type);		
	testObj.put(JPSConstants.CONTENT, jsonArray);
	*/
	
	@Test
	void testUploadRoutingData() {
		
		String label = "test1";
		String expected = "[{\"o\":\""+label+"\"}]";
		
		JSONArray jsonArray = new JSONArray(jsonString(label));	
		
		MockStoreClient mockStore = new MockStoreClient();
		
		RoutingUploaderTool uploader = new RoutingUploaderTool();
		uploader.uploadRoutingData(jsonArray, mockStore);
		
		JSONArray result = mockStore.executeQuery(testQuery(label));
		assertEquals(expected,result.toString());
	}

	String testQuery(String label) {		
		return "SELECT ?o\n WHERE{<http://www.theworldavatar.com/kb/ontokgrouter/"
				+label+"> <http://www.w3.org/2000/01/rdf-schema#label> ?o}";
	}
	
	String jsonString(String label) {
		return "[\n"+
					"{\n"+
					"	\"label\": \""+label+"\",\n"+
					"	\"queryEndpoint\": \"http://www.theworldavatar.com/blazegraph/namespace/"+label+"/sparql\",\n"+
					"	\"updateEndpoint\": \"http://www.theworldavatar.com/blazegraph/namespace/"+label+"/sparql\"\n"+
					"},\n"+
					"{\n"+
					"	\"label\": \"test123456\",\n"+
					"	\"queryEndpoint\": \"http://www.theworldavatar.com/blazegraph/namespace/test123456/sparql\",\n"+
					"	\"updateEndpoint\": \"http://www.theworldavatar.com/blazegraph/namespace/test123456/sparql\"\n"+
					"}\n"+
				"]";
	}
	
}
