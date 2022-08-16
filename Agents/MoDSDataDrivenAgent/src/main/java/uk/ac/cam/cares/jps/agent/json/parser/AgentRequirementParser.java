package uk.ac.cam.cares.jps.agent.json.parser;

import com.jayway.jsonpath.JsonPath;

public class AgentRequirementParser {
	public static String getNumberOfCores(String jsonString) {
		return JsonPath.read(jsonString, "$.ontoagent.core.value");
	}
	
	public static String getRAMSize(String jsonString) {
		return JsonPath.read(jsonString, "$.ontoagent.memReq.value");
	}
}
