package com.cmclinnovations.jps.agent.json.parser;

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
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @param jsonString the input json string (content)
	 * @return the value of ontospeciesIRI. 
	 * 
	 */
	public static String getOntoSpeciesIRI(String jsonString) {
		
		return JsonPath.read(jsonString, "$.job.ontospeciesIRI");
	}
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @param jsonString the input json string (content)
	 * @return the value of ontocompchemIRI.
	 */
	public static String getOntoCompChemIRI(String jsonString) {
		
		return JsonPath.read(jsonString, "$.job.ontocompchemIRI");
	}
}
