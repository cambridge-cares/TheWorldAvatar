package uk.ac.cam.cares.jps.composition.test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.After;
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

public class TestCompositionJSON {

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() throws JsonParseException, JsonMappingException, JSONException, URISyntaxException, IOException, Exception {
		// Load semantic description of CityToWeather .. and another one ..
		 
		ServiceCompositionEngine engine = new ServiceCompositionEngine(getTestCompositeAgentCityToWindDirection(), "http://localhost:8080");
		engine.start();
		Graph graph = engine.getGraph();
		ArrayList<Service> serviceEliminationList = OptimalPathSearcher.getAllServicesToBeDeleted(graph);
		ArrayList<String> eliminationList = new ArrayList<String>();
		for (Service service : serviceEliminationList) {
			eliminationList.add(service.getUri().toASCIIString());
		}
		
		//System.out.println(FormatTranslator.convertGraphJavaClassTOJSON(graph));
		
		ExecutorProcessor processor = new ExecutorProcessor(FormatTranslator.convertGraphJavaClassTOJSON(graph), eliminationList);
		ArrayList<ExecutionLayer> executionChain = processor.generateExecutionChain();
		ExecutorNew executor = new ExecutorNew(executionChain); 
		System.out.println("-------------------------------- ExecutionChain --------------------------------");
		System.out.println(FormatTranslator.convertExectorToJSON(executor).toString());
		System.out.println("--------------------------------------------------------------------------------");
		JSONObject inputJSON = new JSONObject("{\"city\": \"http://dbpedia.org/resource/Berlin\"}"); 
		executor.execute(inputJSON);
		//System.out.println("Final Result ! \n"  + executor.execute(inputJSON));

		
	}

	public Service getTestCompositeAgentCityToWindDirection() throws JSONException, JsonParseException, JsonMappingException, URISyntaxException, IOException {
		JSONObject compositeAgentInJSON = new JSONObject("{\"operations\":[{\"outputs\":[{\"optionalParts\":[],\"mandatoryParts\":[{\"type\":\"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasDirection\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_15wGxcwo\",\"value\":\"\",\"datatypeValue\":\"\"}],\"uri\":\"http://www.theworldavatar.com/MessageContent_Output_18YRk5SC\"}],\"inputs\":[{\"optionalParts\":[],\"mandatoryParts\":[{\"type\":\"http://dbpedia.org/ontology/city\",\"uri\":\"http://www.theworldavatar.com/Mandatory_MessagePart_CghedAK\",\"value\":\"\",\"datatypeValue\":\"\"}],\"uri\":\"http://www.theworldavatar.com/MessageContent_Input_xzbAvBW\"}],\"httpUrl\":\"http://www.theworldavatar.com/JPS_COMPOSITION/CoordinateToWeather\",\"uri\":\"http://www.theworldavatar.com/Operation_pexDwAC\"}],\"uri\":\"http://www.theworldavatar.com/Composite_Service_yIVb8on\"}");
		return FormatTranslator.convertJSONTOJavaClass(compositeAgentInJSON.toString());
	}
}
