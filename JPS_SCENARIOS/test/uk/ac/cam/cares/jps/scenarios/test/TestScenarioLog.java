package uk.ac.cam.cares.jps.scenarios.test;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.scenario.ScenarioLog;

public class TestScenarioLog extends TestCase {

	public void testToJson() {
		
		ScenarioLog log = new ScenarioLog();
		JSONObject message = new JSONObject();
		message.put("operation", "mock").put("input", "some input parameter values");
		log.logMessage(message);
		message = new JSONObject();
		message.put("operation", "query").put("input", "some other input parameter values");
		log.logMessage(message);
		
		JSONObject jo = log.toJson();
		
		System.out.println(jo.toString());
	}
	
}
