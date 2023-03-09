package uk.ac.cam.cares.jps.scenario;

import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.scenario.ScenarioLog.ScenarioLogEntry;

public class ScenarioMockManager {
	
	public void mock(JSONObject jo, String scenarioName, ScenarioLog log) {
		
		if (jo.isNull(JPSConstants.SCENARIO_AGENT)) {
			throw new JPSRuntimeException("missing input parameter " + JPSConstants.SCENARIO_AGENT);
		}
		
		String agent = jo.getString(JPSConstants.SCENARIO_AGENT);
		if (agent.isEmpty()) {
			return;
		}

		JSONObject message = new JSONObject().put("operation", "mock").put("agent", agent);
		log.logMessage(scenarioName, message);
	}

	public String execute(JSONObject jo, String scenarioName, String operation, ScenarioLog log) {
		
		String httpUrl = findHttpUrlForOperationOfMockedAgent(jo, scenarioName, operation, log);
		if (httpUrl == null) {
			throw new JPSRuntimeException("unknown operation for scenario agent, scenarioName=" + scenarioName + ", operation=" + operation);
		}
		
		JSONObject joresult = findResultInScenarioLog(jo, scenarioName, operation, log);
		if (joresult != null) {
			// don't call the agent again, just return the mocked result
			return joresult.toString();
		}
		
//		jo.put(JPSConstants.SCENARIO_URL, ScenarioManagementAgent.getScenarioUrl(scenarioName));
//		String result = AgentCaller.executeGetWithURLAndJSON(httpUrl, jo.toString());
		
		String result = ScenarioManagementAgent.execute(scenarioName, httpUrl, jo);
		
		joresult = new JSONObject();
		if ((result != null) && !result.isEmpty()) {
			joresult = new JSONObject(result);
		}
		
		JSONObject message = new JSONObject();
		String agent = getLatestMockedAgent(log);
		message.put("output", joresult);
		message.put("input", jo);
		message.put("operation", operation);
		message.put("agent", agent);

		log.logMessage(scenarioName, message);
		
		return result;
	}
	
	private String findHttpUrlForOperationOfMockedAgent(JSONObject jo, String scenarioName, String operation, ScenarioLog log) {
		
		String agent = getLatestMockedAgent(log);
		if (agent != null ) {
			JSONObject input = new JSONObject().put("agent", agent); 
			String jsondescr = AgentCaller.executeGetWithJsonParameter("/JPS_COMPOSITION/describe", input.toString());
			JSONArray joservice = new JSONObject(jsondescr).getJSONArray("service");
			int size = joservice.length();
			for (int i=0; i<size; i++) {
				JSONObject jooperation = joservice.getJSONObject(i).getJSONObject("hasOperation");
				String httpUrl = jooperation.getString("hasHttpUrl");
				int index = httpUrl.lastIndexOf("/");
				if (operation.equals(httpUrl.substring(index))) {
					return httpUrl;
				}
			}
		}
		
		return null;
	}
	
	public static String getLatestMockedAgent(ScenarioLog log) {
		List<ScenarioLogEntry> entries = log.search("operation", "mock");
		if (entries.size() > 0) {
			ScenarioLogEntry latestEntry = entries.get(entries.size()-1);
			return latestEntry.message.getString("agent");
		}
		return null;
	}
	
	private JSONObject findResultInScenarioLog(JSONObject jo, String scenarioName, String operation, ScenarioLog log) {
		
		// TODO-AE SC URGENT 20190913 dummy mock is commented
		// This is a dummy for getting the mocking result and returns only the last result
//		List<ScenarioLogEntry> entries = log.search("operation", operation);
//		if (entries.size() > 0) {
//			ScenarioLogEntry latestEntry = entries.get(entries.size()-1);
//			return latestEntry.message.getJSONObject("output");
//		}
		return null;
	}
}
