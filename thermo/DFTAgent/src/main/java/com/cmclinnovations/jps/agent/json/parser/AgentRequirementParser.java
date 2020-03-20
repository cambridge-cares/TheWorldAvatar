package com.cmclinnovations.jps.agent.json.parser;

import com.jayway.jsonpath.JsonPath;

public class AgentRequirementParser {
	public static String getNumberOfCores(String jsonString){
		return JsonPath.read(jsonString, "$.results.bindings[0].core.value");
	}
	
	public static String getRAMSize(String jsonString){
		return JsonPath.read(jsonString, "$.results.bindings[0].memReq.value");
	}

}
