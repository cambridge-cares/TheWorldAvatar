package uk.ac.cam.cares.jps.scenarios.test;

import java.util.List;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.scenario.ScenarioManagementAgent;

public class TestScenarioManagement extends TestCase {
	
	public void testCreateScenarioDescription() {
		
		String scenarioName = "test1234567";
		String agent = "http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl";
		
		new ScenarioManagementAgent().createScenarioDescription(scenarioName, agent);
		
		
	}
	
	public void testListScenarios() {
		
		for (int i=1; i<=10; i++) {
			String scenarioName = "sc" + i + "emissionagenttest";
			TestScenarios.createScenarioAndCallEmissionAgentCreatingScenarioDescription(scenarioName);
		}
		
		String json = new ScenarioManagementAgent().listScenariosAndAgentsAsJson();
		
		System.out.println(json);
		
		// TODO-AE SC assert list test
	}
	
	public void testTmp() {
		List<String> list = new ScenarioManagementAgent().getScenarioIRIs();
		for (String current : list) {
			System.out.println(current);
		}
	}
	
	public void testTmp2() {
		
		//String json = new ScenarioManagementAgent().listScenariosAndAgentsAsJson();
		//System.out.println(json);
		
		new ScenarioManagementAgent().writeListToFile();
	}

	
}
