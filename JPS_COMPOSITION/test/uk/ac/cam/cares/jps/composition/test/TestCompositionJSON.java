package uk.ac.cam.cares.jps.composition.test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.junit.After;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

import uk.ac.cam.cares.jps.agents.discovery.ServiceDiscovery;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueServer;
import uk.ac.cam.cares.jps.composition.compositionengine.ServiceCompositionEngine;
import uk.ac.cam.cares.jps.composition.enginemodel.Graph;
import uk.ac.cam.cares.jps.composition.executor.ExecutionLayer;
import uk.ac.cam.cares.jps.composition.executor.ExecutorNew;
import uk.ac.cam.cares.jps.composition.executor.ExecutorProcessor;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;
import uk.ac.cam.cares.jps.composition.util.OptimalPathSearcher;

public class TestCompositionJSON {

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() throws JsonParseException, JsonMappingException, JSONException, URISyntaxException, IOException, Exception {
		// Load semantic description of CityToWeather .. and another one ..
		
		KeyValueServer.set("host", "localhost");
		KeyValueServer.set("port", "8080");	
		String compositionDir = AgentLocator.getCurrentJpsAppDirectory(this);
		KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, compositionDir + "/testres/admsservicesWithoutWasteProduct");
		
		
		ServiceCompositionEngine engine = new ServiceCompositionEngine(getDebugCase(), "http://localhost:8080");
		engine.start();
		Graph graph = engine.getGraph();
		ArrayList<Service> serviceEliminationList = OptimalPathSearcher.getAllServicesToBeDeleted(graph);
		ArrayList<String> eliminationList = new ArrayList<String>();
		for (Service service : serviceEliminationList) {
			eliminationList.add(service.getUri().toASCIIString());
		}
		
		System.out.println(eliminationList);
		
		ExecutorProcessor processor = new ExecutorProcessor(FormatTranslator.convertGraphJavaClassTOJSON(graph), eliminationList);
		ArrayList<ExecutionLayer> executionChain = processor.generateExecutionChain();
		
		//			ExecutorNew executor = FormatTranslator.convertJSONTOExecutor(executorInJSON.toString());

		ExecutorNew executor = new ExecutorNew(executionChain); 


//		String input = new JSONStringer().object(). //52.074902, 4.288283 den hague 
//				key("region").object()
//					.key("lowercorner").object()
//						.key("lowerx").value("4.285190465377747") //52.07686748548798 4.285190465377747
//						.key("lowery").value("52.07321150985498").endObject()
//					.key("uppercorner").object()
//						.key("upperx").value("4.2916469691125085") //52.07321150985498 4.2916469691125085
//						.key("uppery").value("52.07686748548798").endObject()
//					.key("srsname").value("EPSG:4326")
//				.endObject()
//				.endObject().toString(); 
		
		
		String input = new JSONStringer().object().
				key("region").object()
					.key("lowercorner").object()
						.key("lowerx").value("13.4074096")
						.key("lowery").value("52.5177665").endObject()
					.key("uppercorner").object()
						.key("upperx").value("13.409")
						.key("uppery").value("52.52").endObject()
					.key("srsname").value("EPSG:4326")
				.endObject()
				.endObject().toString(); 
		
		
		input = "{\"region\":{\"lowercorner\":{\"lowerx\":\"4.283710282240122\",\"lowery\":\"52.07126465153749\"},\"uppercorner\":{\"upperx\":\"4.2946107796766455\",\"uppery\":\"52.07983708183632\"},\"srsname\":\"EPSG:4326\"}}";
		
		
 		String result = executor.execute(new JSONObject(input));
 		System.out.println("============== result ==============");
 		System.out.println(result);
 		System.out.println("====================================");
		
//		assertEquals(new JSONObject(result).getJSONArray("buildings").length(),25);
		 
		
	}

	public Service getTestCompositeAgentCityToWindDirection() throws JSONException, JsonParseException, JsonMappingException, URISyntaxException, IOException {
		JSONObject compositeAgentInJSON = new JSONObject("{\"operations\":[{\"outputs\":[{\"optionalParts\":[],\"mandatoryParts\":[{\"type\":\"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasDirection\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_15wGxcwo\",\"value\":\"\",\"datatypeValue\":\"\"}],\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC\"}],\"inputs\":[{\"optionalParts\":[],\"mandatoryParts\":[{\"type\":\"http://dbpedia.org/ontology/city\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_CghedAK\",\"value\":\"\",\"datatypeValue\":\"\"}],\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_xzbAvBW\"}],\"httpUrl\":\"http://www.theworldavatar.com/JPS_COMPOSITION/CoordinateToWeather\",\"uri\":\"http://www.theworldavatar.com/Operation_pexDwAC\"}],\"uri\":\"http://www.theworldavatar.com/Composite_Service_yIVb8on\"}");
		return FormatTranslator.convertJSONTOJavaClass(compositeAgentInJSON.toString());
	}
	
	
	
	public Service getDebugCase() throws JSONException, JsonParseException, JsonMappingException, URISyntaxException, IOException{
		JSONObject agentInJSON = new JSONObject("{\"uri\":\"http://www.theworldavatar.com/Composite_Service_8PPv74p\",\"operations\":[{\"uri\":\"http://www.theworldavatar.com/Operation_pexDwAC\",\"inputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_xzbAvBW\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_CghedAK\",\"value\":\"\",\"datatypeValue\":\"\",\"type\":\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\"}],\"optionalParts\":[]}],\"outputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_15wGxcwo\",\"value\":\"\",\"datatypeValue\":\"\",\"type\":\"https://www.w3.org/ns/csvw#Table\"}],\"optionalParts\":[]},{\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_1gpms0DA\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_guVoBmm\",\"value\":\"\",\"datatypeValue\":\"\",\"type\":\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType\"}],\"optionalParts\":[]}],\"httpUrl\":\"http://www.theworldavatar.com/JPS_COMPOSITION/CoordinateToWeather\"}]}\r\n");
				return FormatTranslator.convertJSONTOJavaClass(agentInJSON.toString());

	}
	
	
	//https://www.w3.org/ns/csvw#Table
	public Service getTestCompositeAgentRegionToADMS() throws JSONException, JsonParseException, JsonMappingException, URISyntaxException, IOException {
		JSONObject compositeAgentInJSON = new JSONObject("{\"operations\":[{\"outputs\":[{\"optionalParts\":[],\"mandatoryParts\":[{\"type\":\"https://www.w3.org/ns/csvw#Table\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_15wGxcwo\",\"value\":\"\",\"datatypeValue\":\"\"}],\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC\"},{\"optionalParts\":[],\"mandatoryParts\":[{\"type\":\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_1515cwo\",\"value\":\"\",\"datatypeValue\":\"\",\"array\":\"true\"}],\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC\"}],\"inputs\":[{\"optionalParts\":[],\"mandatoryParts\":[{\"type\":\"http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_CghedAK\",\"value\":\"\",\"datatypeValue\":\"\"}],\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_xzbAvBW\"}],\"httpUrl\":\"http://www.theworldavatar.com/JPS_COMPOSITION/RegionToADMS\",\"uri\":\"http://www.theworldavatar.com/Operation_pexDwAC\"}],\"uri\":\"http://www.theworldavatar.com/Composite_Service_ODsMpRv\"}");
		return FormatTranslator.convertJSONTOJavaClass(compositeAgentInJSON.toString());
	}
	
	

	
	
}
