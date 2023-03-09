package uk.ac.cam.cares.jps.scenarios.test;

import java.util.List;

import org.json.JSONObject;
import org.junit.Ignore;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.scenario.ScenarioLog;
import uk.ac.cam.cares.jps.scenario.ScenarioLog.ScenarioLogEntry;
import uk.ac.cam.cares.jps.scenario.ScenarioManagementAgent;
import uk.ac.cam.cares.jps.scenario.ScenarioMockManager;

@Ignore("This test sends HTTP requests. Cannot run without correct environment setup.")
public class TestScenarioMockManager extends TestCase {
	
	public void testCallEmissionAgentTwice() {
		
		// prepare the test
		String scenarioName = "testCallEmissionAgentTwice";
		//ScenarioLog log = new ScenarioLog(scenarioName);
		
		// the following method will request /setemission via /call from the scenario agent
		// this will copy the plant OWL to the scenario bucket and /setemission will set its emission value to 178.14
		// however, /call never writes the response results to the scenario log !!!
		double emissionValue = 178.14;
		TestScenarios.setEmissionValue(scenarioName, TestScenarios.PLANT, emissionValue);
	
		// the test itself is executed in several steps
		
		// first step: add the functionality of EmissionTestAgent to the scenario agent 
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.SCENARIO_AGENT, "http://www.theworldavatar.com/kb/agents/Service__EmissionTestAgent.owl#Service");
		ScenarioLog log = ScenarioManagementAgent.getScenarioLog(scenarioName);
		ScenarioMockManager manager = new ScenarioMockManager();
		manager.mock(jo, scenarioName, log);
		
		// second step: execute /getemission via scenario agent
		// since EmissionAgent is mocked, this time the result 178.14 will be written to the scenario log 
		jo = new JSONObject().put("plant", TestScenarios.PLANT);
		String result = manager.execute(jo, scenarioName, "/getemission", log);
		System.out.println("Result from first call of getemission = " + result);
		
		
		
		
		// check that the result 178.14 was really written to the scenario log
		List<ScenarioLogEntry> entries = log.search("output", null);
		assertEquals(1, entries.size());
		double loggedEmissionValue = entries.get(0).message.getJSONObject("output").getJSONObject("hasemission").getJSONObject("hasvalue").getDouble("hasnumericalvalue");
		assertEquals(emissionValue, loggedEmissionValue);

		// third step: overwrite the emission value in the plant OWL file in the scenario bucket
		// as in the first step, /setemission is requested via /call 
		// and the result is only written to the OWL file in the scenario bucket
		double newEmissionValue = 17.59;
		TestScenarios.setEmissionValue(scenarioName, TestScenarios.PLANT, newEmissionValue);
		
		// check that 17.59 it is really written to the OWL file
		TestScenarios.assertEmissionValue(scenarioName, TestScenarios.PLANT, newEmissionValue);
		// reload the log and check that not further "output" entries were added
		log = ScenarioManagementAgent.getScenarioLog(scenarioName);
		System.out.println(log.toJson());
		entries = log.search("output", null);
		assertEquals(1, entries.size());
		
		// fourth step: request /getemission via scenario agent 
		// since EmissionAgent is mocked, the request is not performed
		// instead, the result is read from the scenario log
		result = manager.execute(jo, scenarioName, "/getemission", log);
		System.out.println("result emissionValue = " + result);	
		double actual = new JSONObject(result).getJSONObject("hasemission").getJSONObject("hasvalue").getDouble("hasnumericalvalue");
		assertEquals(emissionValue, actual);
	}

}
