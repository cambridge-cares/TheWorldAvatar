package uk.ac.cam.cares.jps.misc.http;

import java.io.IOException;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

public class HttpGet {
	
	public void printHelp() {
		System.out.println("\nHttpGet <url> <scenario name>");
		System.out.println("url - any url, e.g. http://www.theworldavatar.com/JPS_SHIP");
		System.out.println("scenario name - optional, default is base scenario");
	}
	
	public void start(String[] args) throws IOException {
		
		if (args.length == 0) {
			printHelp();
			return;
		}
		
		String url = args[0];
		String scenarioName = null;
		if (args.length == 2) {
			scenarioName = args[1];
		}
		
		performHttpGet(url, scenarioName);
	}
	
	private void performHttpGet(String url, String scenarioName) {
		System.out.println("requesting " + url + " with scenario name = " + scenarioName);
		JSONObject jo = new JSONObject();
		//String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		//jo.put(JPSConstants.SCENARIO_URL, scenarioUrl);
		jo.put("scenarioname", scenarioName);
		
		String result = AgentCaller.executeGetWithURLAndJSON(url, jo.toString());
		System.out.println("response:");
		System.out.println(result);
	}
}
