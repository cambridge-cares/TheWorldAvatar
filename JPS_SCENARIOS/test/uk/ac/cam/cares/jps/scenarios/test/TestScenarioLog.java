package uk.ac.cam.cares.jps.scenarios.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.scenario.ScenarioLog;

public class TestScenarioLog extends TestCase {

	public void testToJson() {
		
		ScenarioLog log = new ScenarioLog("test789xyz");
		JSONObject message = new JSONObject();
		message.put("operation", "mock").put("input", "some input parameter values");
		log.logMessage("test789xyz", message);
		message = new JSONObject();
		message.put("operation", "query").put("input", "some other input parameter values");
		log.logMessage("test789xyz", message);
		
		JSONObject jo = log.toJson();
		
		System.out.println(jo.toString());
	}
	
}
