package com.cmclinnovations.jps.agent.json.parser;

import java.util.List;
import java.util.Map;

import com.jayway.jsonpath.JsonPath;

public class JSonRequestParser {

	public static String getLevelOfTheory(String jsonString){
		return JsonPath.read(jsonString, "$.job.levelOfTheory");
	}
	
	public static String getJobKeyword(String jsonString){
		return JsonPath.read(jsonString, "$.job.keyword");
	}
	
	public static String getAlgorithmChoice(String jsonString){
		return JsonPath.read(jsonString, "$.job.algorithmChoice");
	}

	public static String getSpeciesIRI(String jsonString){
		
		return JsonPath.read(jsonString, "$.speciesIRI");
	}	
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @param jsonString input json content
	 * @return all species IRIs and ontocomcpchem IRIs given in json input file.
	 */
	public static List<Map<String, Object>> getAllSpeciesIRI(String jsonString){
		
		return JsonPath.parse(jsonString).read("$.job[*]");
	}
}
