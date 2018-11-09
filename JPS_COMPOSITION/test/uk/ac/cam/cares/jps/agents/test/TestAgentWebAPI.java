package uk.ac.cam.cares.jps.agents.test;

import java.io.IOException;
import java.net.URISyntaxException;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.json.JSONWriter;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.agents.api.AgentWebAPI;
import uk.ac.cam.cares.jps.agents.discovery.ServiceDiscovery;
import uk.ac.cam.cares.jps.agents.ontology.ServiceBuilder;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueServer;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class TestAgentWebAPI extends TestCase {
	
	public void testSetLocalProperties() {
		
		// TODO-AE URGENT find a better solution for setting test properties,
		// e.g. is someone wants to test the GUI on his local computer then he should not force to run this test method each time he restarts tomcat
		
		KeyValueServer.set("host", "localhost");
		KeyValueServer.set("port", "8080");	
		String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
		KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithoutWasteProduct");
	}
	
	private Service createADMSWithoutWasteProduct() {
		Service composedAgent = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/Composite_Service_ODsMpRv")
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType", "region")
				.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant", "plant")
				.output("https://www.w3.org/ns/csvw#Table", "dispersiongrid")
				.output("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType", true, "buildings", true)
				.build();
		return composedAgent;
	}

	private JSONObject composeAndExecuteForBerlinDirectCall() throws Exception {

		Service composedAgent = createADMSWithoutWasteProduct();
		//System.out.println("compositeAgent=" + new Gson().toJson(composedAgent));
		
		JSONWriter jsonInput = new JSONStringer().object().
				key("region").object()
					.key("srsname").value("EPSG:28992")
					.key("lowercorner").object()
						.key("lowerx").value("699182")
						.key("lowery").value("532537").endObject()
					.key("uppercorner").object()
						.key("upperx").value("699983")
						.key("uppery").value("533338").endObject()
				.endObject()
				.key("plant").value("http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002")
				.endObject(); 
		
		System.out.println("jsonInput=\n" + jsonInput);
		
		String result = new AgentWebAPI().composeAndExecute(composedAgent, jsonInput.toString());
		System.out.println("result=\n" + result);
		
		return new JSONObject(result);	
	}
	
	public void testComposeAndExecuteForBerlinDirectCallWithoutWasteProduct() throws Exception {
		String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
		KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithoutWasteProduct");
		JSONObject result = composeAndExecuteForBerlinDirectCall();
		assertEquals(25, result.getJSONArray("building").length());
	}
	
	public void testComposeAndExecuteForBerlinDirectCallWithWasteProduct() throws Exception {
		String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
		KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithWasteProduct");
		JSONObject result = composeAndExecuteForBerlinDirectCall();
		assertEquals(25, result.getJSONArray("building").length());
	}
	
	public void testComposeAndExecuteWithUnresolvableParameter() throws Exception {
		String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
		KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithoutWasteProduct");
		
		Service composedAgent = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/Composite_Service_ODsMpRv")
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType", "building")
				.output("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant", "plant")
				.build();
		
		JSONWriter jsonInput = new JSONStringer().object()
				.key("building").value("http://www.theworldavatar.com/kb/nld/thehague/buildings/10_buildings0.owl#BuildingGUID_E77C9F0F-554A-4986-8332-75EDFF2DCF07")
				.endObject(); 
		
		System.out.println("jsonInput=\n" + jsonInput);
		
		try {
			String result = new AgentWebAPI().composeAndExecute(composedAgent, jsonInput.toString());
			System.out.println("result=\n" + result);
			throw new RuntimeException("expected an error");
		} catch (JPSRuntimeException exc) {
			assertTrue(exc.getMessage().startsWith("no composition result"));
		}
	}
	
	public void testComposeAndExecuteForTheHagueAgentCallWithoutWasteProduct() throws JsonParseException, JsonMappingException, JSONException, URISyntaxException, IOException, Exception {

		Service compositeAgent = createADMSWithoutWasteProduct();
		System.out.println("compositeAgent=" + new Gson().toJson(compositeAgent));
		
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
		
		KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithoutWasteProduct");
		//KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithWasteProduct");
			
		String result = AgentCaller.executeGetWithJsonParameter("/JPS_COMPOSITION/execute", jsonInput.toString());
		// TODO: The result returned from this test is not in the form of a JSON Object 
		
		//String result = new AgentWebAPI().composeAndExecute(compositeAgent, host, jsonInput.toString());
		System.out.println("result=\n" + result);
		JSONObject jsonOutput = new JSONObject(result);
		assertEquals(25, jsonOutput.getJSONArray("building").length());
	}
}
