package uk.ac.cam.cares.jps.agents.test;

import java.io.IOException;
import java.net.URISyntaxException;

import org.json.JSONException;
import org.json.JSONObject;
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
import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;

public class TestAgentWebAPI extends TestCase {

	public void testCompositeAndExecuteADMS() throws JsonParseException, JsonMappingException, JSONException, URISyntaxException, IOException, Exception {

		Service compositeAgent = new ServiceBuilder()
				.operation(null, "http://www.theworldavatar.com/Composite_Service_ODsMpRv")
				.input("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType", "region")
				.input("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Plant", "plant")
				.output("https://www.w3.org/ns/csvw#Table", "dispersiongrid")
				.output("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType", true, "buildings", true)
				.build();
		
		
//		Service compositeAgent = getTestCompositeAgentRegionToADMS();
		
		System.out.println("compositeAgent=\n" + new Gson().toJson(compositeAgent));
			
		String host = "http://localhost:8080";
		
		
		
		String jsonInputValues = new JSONStringer().object().
				key("region").object()
					.key("lowercorner").object()
						.key("lowerx").value("13.4074096")
						.key("lowery").value("52.5177665").endObject()
					.key("uppercorner").object()
						.key("upperx").value("13.409")
						.key("uppery").value("52.52").endObject()
					.key("srsname").value("EPSG:4326")
				.endObject()
				.key("plant").value("http://www.theworldavatar.com/kb/deu/berlin/powerplants/Heizkraftwerk_Mitte.owl#Plant-002")
				.endObject().toString(); 
		
		System.out.println("jsonInputValues=\n" + jsonInputValues);
		
		String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
		KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservices");
			
		String result = new AgentWebAPI().composeAndExecute(compositeAgent, host, jsonInputValues);
		
		System.out.println("result=\n" + result);
	}
	
	private Service getTestCompositeAgentRegionToADMS() throws JSONException, JsonParseException, JsonMappingException, URISyntaxException, IOException {
		JSONObject compositeAgentInJSON = new JSONObject("{\"operations\":[{\"outputs\":[{\"optionalParts\":[],\"mandatoryParts\":[{\"type\":\"https://www.w3.org/ns/csvw#Table\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_15wGxcwo\",\"value\":\"\",\"datatypeValue\":\"\"}],\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC\"},{\"optionalParts\":[],\"mandatoryParts\":[{\"type\":\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_1515cwo\",\"value\":\"\",\"datatypeValue\":\"\",\"array\":\"true\"}],\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC\"}],\"inputs\":[{\"optionalParts\":[],\"mandatoryParts\":[{\"type\":\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_CghedAK\",\"value\":\"\",\"datatypeValue\":\"\"}],\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_xzbAvBW\"}],\"httpUrl\":\"http://www.theworldavatar.com/JPS_COMPOSITION/RegionToADMS\",\"uri\":\"http://www.theworldavatar.com/Operation_pexDwAC\"}],\"uri\":\"http://www.theworldavatar.com/Composite_Service_ODsMpRv\"}");
		return FormatTranslator.convertJSONTOJavaClass(compositeAgentInJSON.toString());
	}
}
