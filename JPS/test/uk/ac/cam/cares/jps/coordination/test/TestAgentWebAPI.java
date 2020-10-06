package uk.ac.cam.cares.jps.coordination.test;

import java.io.IOException;
import java.net.URISyntaxException;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class TestAgentWebAPI extends TestCase {

	//public class TestAgentWebAPI {

	private static final String KEY_DIR_KB_AGENTS = "absdir.knowledgebase.agents";
	
	public void testSetLocalProperties() {
		
		// TODO-AE URGENT find a better solution for setting test properties,
		// e.g. is someone wants to test the GUI on his local computer then he should not force to run this test method each time he restarts tomcat
		
		KeyValueManager.set("host", "localhost");
		KeyValueManager.set("port", "8080");	
		String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
		KeyValueManager.set(KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithoutWasteProduct");
	}
	
	public void testComposeAndExecuteForTheHagueAgentCallWithoutWasteProduct() throws JsonParseException, JsonMappingException, JSONException, URISyntaxException, IOException, Exception {
		
		JSONWriter jsonInput = new JSONStringer().object()
				.key("agent").value("http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service")
				.key("region").object()
					.key("srsname").value("EPSG:28992")
					.key("lowercorner").object()
						.key("lowerx").value("79480")
						.key("lowery").value("454670").endObject()
					.key("uppercorner").object()
						.key("upperx").value("80000")
						.key("uppery").value("455190").endObject()
				.endObject()
				.key("plant").value("http://www.theworldavatar.com/kb/nld/thehague/powerplants/Plant-001.owl#Plant-001")
				.endObject();  
		
		System.out.println("jsonInput=\n" + jsonInput);
		
		String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
		
		KeyValueManager.set(KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithoutWasteProduct");
		//KeyValueManager.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithWasteProduct");
			
		String result = AgentCaller.executeGetWithJsonParameter("/JPS_COMPOSITION/execute", jsonInput.toString());
		// TODO: The result returned from this test is not in the form of a JSON Object 
		System.out.println("result=\n" + result);
		JSONObject jsonOutput = new JSONObject(result);
		//assertEquals(25, jsonOutput.getJSONArray("building").length());
	}
	
	//testing same as with composition
	public void testComposeAndExecuteForTheHagueAgentCallWithWasteProduct() throws JsonParseException, JsonMappingException, JSONException, URISyntaxException, IOException, Exception {
		
		JSONWriter jsonInput = new JSONStringer().object()
				.key("agent").value("http://www.theworldavatar.com/kb/agents/Service__ComposedADMS.owl#Service")
				.key("region").object()
					.key("srsname").value("EPSG:28992")
					.key("lowercorner").object()
						.key("lowerx").value("79480")
						.key("lowery").value("454670").endObject()
					.key("uppercorner").object()
						.key("upperx").value("80000")
						.key("uppery").value("455190").endObject()
				.endObject()
				.key("plant").value("http://www.theworldavatar.com/kb/nld/thehague/powerplants/Plant-001.owl#Plant-001")
				.key("reactionmechanism").value("https://como.cheng.cam.ac.uk/kb/Toluene.owl#ReactionMechanism_4631074216281807")
				.endObject();  
		
		System.out.println("jsonInput=\n" + jsonInput);
		
		String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
		
		testSetLocalProperties();
		KeyValueManager.set(KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithWasteProduct");
			
		String result = AgentCaller.executeGetWithJsonParameter("/JPS_COMPOSITION/execute", jsonInput.toString());
		// TODO: The result returned from this test is not in the form of a JSON Object 
		System.out.println("result=\n" + result);
	}
}
