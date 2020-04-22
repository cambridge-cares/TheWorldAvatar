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
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param jsonString input json content
	 * @return all species IRIs and ontocomcpchem IRIs given in json input file.
	 * 
	 */
	public static List<Map<String, Object>> getAllTargetSpeciesIRI(String jsonString){
		
		return JsonPath.parse(jsonString).read("$.job.targetSpecies[*]");
	}
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param jsonString input json content
	 * @return all ontospecies IRIs and ontocompchem IRIs that are reference species given in Json input file.
	 *  
	 */
	public static List<Map<String, Object>> getAllReferenceSpeciesIRI(String jsonString){
		
		boolean booleanIndicator = JsonPath.parse(jsonString).read("$.isTargetSpeciesSameAsReferenceSpecies");
		
		/**
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * 
		 * Checks whether target species list is the same as reference species list		
		 */
		if(booleanIndicator==true) {
			
			return JsonPath.parse(jsonString).read("$.job.targetSpecies[*]");
			
		} else {
		
		return JsonPath.parse(jsonString).read("$.job.referenceSpecies[*]");
		}
	}
	
	public static String getSrcRefPool(String jsonString){
		
		return JsonPath.read(jsonString, "$.srcRefPool");
	}
	
public static String getInputZipFile(String jsonString){
		
		return JsonPath.read(jsonString, "$.inputZipFile");
		
	}

/**
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * @param jsonString the input JSON string
 * @return folder path where Gaussian files are copied.
 */
public static String getGaussianFolderPath(String jsonString) {
	
	return JsonPath.read(jsonString, "$.srcCompoundsRef");
}
}