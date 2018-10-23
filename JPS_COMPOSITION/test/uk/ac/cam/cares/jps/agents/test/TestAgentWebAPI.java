package uk.ac.cam.cares.jps.agents.test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;

import org.json.JSONException;
import org.json.JSONStringer;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.google.gson.Gson;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.agents.api.AgentWebAPI;
import uk.ac.cam.cares.jps.agents.discovery.ServiceDiscovery;
import uk.ac.cam.cares.jps.agents.ontology.ServiceBuilder;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueServer;
import uk.ac.cam.cares.jps.composition.enginemodel.Graph;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

public class TestAgentWebAPI extends TestCase {
	
	public void testSetLocalProperties() {
		
		KeyValueServer.set("host", "localhost");
		KeyValueServer.set("port", "8080");
		
	}
	
	

	public void testCompositeAndExecuteADMS() throws JsonParseException, JsonMappingException, JSONException, URISyntaxException, IOException, Exception {

		Service compositeAgent = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/Composite_Service_ODsMpRv")
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType", "region")
				.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant", "plant")
				.output("https://www.w3.org/ns/csvw#Table", "dispersiongrid")
				.output("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType", true, "buildings", true)
				.build();
		
		System.out.println("compositeAgent=\n" + new Gson().toJson(compositeAgent));
			
		String host = "http://localhost:8080";
		
		
		
		String jsonInputValues = new JSONStringer().object().
				key("region").object()
					.key("srsname").value("EPSG:4326")
					.key("lowercorner").object()
						.key("lowerx").value("13.4074096")
						.key("lowery").value("52.5177665").endObject()
					.key("uppercorner").object()
						.key("upperx").value("13.409")
						.key("uppery").value("52.52").endObject()
				.endObject()
				.key("plant").value("http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002")
				.endObject().toString(); 
		
		System.out.println("jsonInputValues=\n" + jsonInputValues);
		
		String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
		
		// TODO-AE URGENT
		//KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservices");
		KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, "C:/Users/Andreas/TMP/newAgentsMSM");
			
		Object result = new AgentWebAPI().composeAndExecute(compositeAgent, host, jsonInputValues);
		System.out.println("result=\n" + result);
		
		//Object[] result = new AgentWebAPI().compose(compositeAgent, host);
		//showComposedAgents(result);
	}
	
	private void showComposedAgents(Object[] compositionResult) {
		
		Graph graph = (Graph) compositionResult[0];
		ArrayList<String> eliminationList = (ArrayList<String>) compositionResult[1];

		System.out.println("service pool:");
		for (Service current : graph.servicePool) {
			System.out.println(current.uri);
		}
		
		System.out.println("\nelimination list:");
		for (String current : eliminationList) {
			System.out.println(current);
		}
	}
	
}
