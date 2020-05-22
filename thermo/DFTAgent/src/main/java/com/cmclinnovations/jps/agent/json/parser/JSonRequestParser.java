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
	
	public static boolean isInvokingThermoAgentRequired(String jsonString){
		return JsonPath.read(jsonString, "$.isInvokingThermoAgentRequired");
	}
	
	public static boolean isApplyingThermoUpdateToMechanismRequired(String jsonString){
		return JsonPath.read(jsonString, "$.isApplyingThermoUpdateToMechanismRequired");
	}

	public static String getOntoCompChemIRI(String jsonString){
		return JsonPath.read(jsonString, "$.gaussian");
	}

	public static String getHighTemperatureCoefficient(String jsonString){
		return JsonPath.read(jsonString, "$.highTcoeff");
	}

	public static String getLowTemperatureCoefficient(String jsonString){
		return JsonPath.read(jsonString, "$.LowTcoeff");
	}
}
