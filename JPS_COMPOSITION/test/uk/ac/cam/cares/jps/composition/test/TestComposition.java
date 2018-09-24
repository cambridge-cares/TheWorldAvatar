package uk.ac.cam.cares.jps.composition.test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONStringer;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

import uk.ac.cam.cares.jps.composition.compositionengine.ServiceCompositionEngine;
import uk.ac.cam.cares.jps.composition.enginemodel.Graph;
import uk.ac.cam.cares.jps.composition.executor.ExecutionLayer;
import uk.ac.cam.cares.jps.composition.executor.ExecutorNew;
import uk.ac.cam.cares.jps.composition.executor.ExecutorProcessor;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;
import uk.ac.cam.cares.jps.composition.util.FormatTranslator;
import uk.ac.cam.cares.jps.composition.util.OptimalPathSearcher;

public class TestComposition {

	@Test
	public void test() throws Exception {
		
		String input = new JSONStringer().object().
				key("region").object()
					.key("lowercorner").object()
						.key("lowerx").value("13.4074096")
						.key("lowery").value("52.5177665").endObject()
					.key("uppercorner").object()
						.key("upperx").value("13.4075")
						.key("uppery").value("52.5178").endObject()
					.key("srsname").value("EPSG:4326")
				.endObject()
				.endObject().toString(); 
		
		
		
//		JSONObject inputJSON = new JSONObject("{\"city\": \"http://dbpedia.org/resource/Berlin\"}"); 
//		String input = inputJSON.toString();
		
//		Service compositeAgent = null;
//		String fileDirectory =  AgentLocator.getCurrentJpsAppDirectory(this) + "/testres/serviceowlfiles";
		ServiceCompositionEngine engine = new ServiceCompositionEngine(getTestCompositeAgentRegionToWeather(), "http://localhost:8080");
		engine.start();
		
		Graph graph = engine.getGraph();
		// Now this graph needs the optimization process 
		ArrayList<Service> serviceEliminationList = OptimalPathSearcher.getAllServicesToBeDeleted(graph);
		ArrayList<String> eliminationList = new ArrayList<String>();
		for (Service service : serviceEliminationList) {
			eliminationList.add(service.getUri().toASCIIString());
		}
		
		System.out.println(FormatTranslator.convertGraphJavaClassTOJSON(graph));
		
		ExecutorProcessor processor = new ExecutorProcessor(FormatTranslator.convertGraphJavaClassTOJSON(graph), eliminationList);
		ArrayList<ExecutionLayer> executionChain = processor.generateExecutionChain();
		ExecutorNew executor = new ExecutorNew(executionChain);
		System.out.println("Final Result ! \n"  + executor.execute(new JSONObject(input)));
		
		
		
 		
//		ArrayList<Service> serviceEliminationList = OptimalPathSearcher.getAllServicesToBeDeleted(graph);
//		ArrayList<String> eliminationList = new ArrayList<String>();
//		for (Service service : serviceEliminationList) {
//			eliminationList.add(service.getUri().toASCIIString());
//		}
//		
//		JSONObject graphInJSON = FormatTranslator.convertGraphJavaClassTOJSON(engine.getGraph());
//		
//		ExecutorProcessor processor = new ExecutorProcessor(graphInJSON, eliminationList);
//		ArrayList<ExecutionLayer> executionChain = processor.generateExecutionChain();
//		Executor executor = new Executor(executionChain);
		
		
//		String result = executor.execute(json);


	}

	
	public Service getTestCompositeAgentCityToWeather() throws JSONException, JsonParseException, JsonMappingException, URISyntaxException, IOException {
		JSONObject compositeAgentInJSON = new JSONObject("{\"uri\":\"http://www.theworldavatar.com/Composite_Service_yIVb8on\",\"operations\":[{\"uri\":\"http://www.theworldavatar.com/Operation_pexDwAC\",\"inputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_xzbAvBW\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_CghedAK\",\"value\":\"\",\"datatypeValue\":\"\",\"modelReference\":\"http://www.theworldavatar.com/OntoEIP/supporting_concepts/space_and_time_v1.owl#City\"}],\"optionalParts\":[]}],\"outputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_15wGxcwo\",\"value\":\"\",\"datatypeValue\":\"\",\"modelReference\":\"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\"}],\"optionalParts\":[]}],\"httpUrl\":\"http://www.theworldavatar.com/JPS_COMPOSITION/CoordinateToWeather\"}]}");
		return FormatTranslator.convertJSONTOJavaClass(compositeAgentInJSON.toString());
	}
	
	public Service getTestCompositeAgentRegionToWeather() throws JSONException, JsonParseException, JsonMappingException, URISyntaxException, IOException {
		JSONObject compositeAgentInJSON = new JSONObject("{\"uri\":\"http://www.theworldavatar.com/Composite_Service_16qPmgMK\",\"operations\":[{\"uri\":\"http://www.theworldavatar.com/Operation_pexDwAC\",\"inputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_xzbAvBW\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_CghedAK\",\"value\":\"\",\"datatypeValue\":\"\",\"modelReference\":\"http://test.com/ontology/Region\"}],\"optionalParts\":[]}],\"outputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_15wGxcwo\",\"value\":\"\",\"datatypeValue\":\"\",\"modelReference\":\"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\"}],\"optionalParts\":[]}],\"httpUrl\":\"http://www.theworldavatar.com/JPS_COMPOSITION/CoordinateToWeather\"}]}");
		return FormatTranslator.convertJSONTOJavaClass(compositeAgentInJSON.toString());
	}
	
	public Service getTestCompositeAgentRegionToADMS() throws JSONException, JsonParseException, JsonMappingException, URISyntaxException, IOException {
		JSONObject compositeAgentInJSON = new JSONObject("{\"uri\":\"http://www.theworldavatar.com/Composite_Service_ODsMpRv\",\"operations\":[{\"uri\":\"http://www.theworldavatar.com/Operation_pexDwAC\",\"inputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_xzbAvBW\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_CghedAK\",\"value\":\"\",\"datatypeValue\":\"\",\"modelReference\":\"http://test.com/ontology/Region\"}],\"optionalParts\":[]}],\"outputs\":[{\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_15wGxcwo\",\"value\":\"\",\"datatypeValue\":\"\",\"modelReference\":\"http://test.com/ontology/ADMSSimulation\"}],\"optionalParts\":[]},{\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_1gpms0DA\",\"mandatoryParts\":[{\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_guVoBmm\",\"value\":\"\",\"datatypeValue\":\"\",\"modelReference\":\"http://test.com/ontology/BuildingList\"}],\"optionalParts\":[]}],\"httpUrl\":\"http://www.theworldavatar.com/JPS_COMPOSITION/CoordinateToWeather\"}]}");
		return FormatTranslator.convertJSONTOJavaClass(compositeAgentInJSON.toString());

	}
	
}
