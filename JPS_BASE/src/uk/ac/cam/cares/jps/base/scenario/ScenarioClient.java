package uk.ac.cam.cares.jps.base.scenario;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class ScenarioClient {
	
	public String call(String scenarioName, String scenarioAgentOperation, String jsonInputParams) {
		
		String path = ScenarioHelper.getScenarioPath(scenarioName) + "/call";
				
		JSONObject jo = new JSONObject(jsonInputParams);
		jo.put(JPSConstants.SCENARIO_AGENT_OPERATION, scenarioAgentOperation);
		String json = jo.toString();
		
		return AgentCaller.executeGetWithJsonParameter(path, json);
	}
	
	public void mock(String scenarioName, String scenarioAgent) {
		String path = ScenarioHelper.getScenarioPath(scenarioName) + "/mock";
		
//		JSONObject jo = new JSONObject(jsonInputParams);
//		jo.put(ScenarioKeys.SCENARIO_AGENT, scenarioAgent);
//		jo.put(ScenarioKeys.SCENARIO_AGENT_OPERATION, scenarioAgentOperation);
//		String json = jo.toString();
//		
//		
//		// TODO-AE SC 20190218 return subscenario hash key?
//		String result = AgentCaller.executeGetWithJsonParameter(path, json);
	}
	
	// TODO-AE SC 20190218 remove prepareRecordering if not needed
	public void prepareRecording(String scenarioName, String scenarioAgent, String scenarioAgentOperation, String jsonInputParams) {
		
		String path = ScenarioHelper.getScenarioPath(scenarioName) + "/preparerecording";
		
		JSONObject jo = new JSONObject(jsonInputParams);
		jo.put(JPSConstants.SCENARIO_AGENT, scenarioAgent);
		jo.put(JPSConstants.SCENARIO_AGENT_OPERATION, scenarioAgentOperation);
		String json = jo.toString();
		
		
		// TODO-AE SC 20190218 return subscenario hash key?
		String result = AgentCaller.executeGetWithJsonParameter(path, json);
		
	}
}
