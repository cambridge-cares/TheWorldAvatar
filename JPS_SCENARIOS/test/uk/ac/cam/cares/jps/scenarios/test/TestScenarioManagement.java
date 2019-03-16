package uk.ac.cam.cares.jps.scenarios.test;

import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.scenario.ScenarioLog;
import uk.ac.cam.cares.jps.scenario.ScenarioManagementAgent;

public class TestScenarioManagement extends TestCase {
	
//	public void testCreateScenarioDescription() {
//		
//		String scenarioName = "test1234567";
//		String agent = "http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl";
//		
//		new ScenarioAgent().createScenarioDescription(scenarioName, agent);
//	}
	
	public void testCreateScenarioAgent() {
	
		String scenarioName = "testscenarioabc";
		ScenarioLog log = new ScenarioLog(scenarioName);
		JSONObject message = new JSONObject().put("agent", "http://www.theworldavatar.com/kb/agents/Service__OpenWeatherMap.owl#Service");
		message.put("operation", "mock");
		log.logMessage(scenarioName, message);
		
		JSONObject jo = new ScenarioManagementAgent().createScenarioAgent(scenarioName, log);
		
		System.out.println(jo);
		
		JSONArray joarray = jo.getJSONArray("service");
		// operations: mock, call, read, query, delete and one from OpenWeatherMap
		assertEquals(7, joarray.length());
	}
	
	
	public void testTmp() {
		List<String> list = new ScenarioManagementAgent().getScenarioIRIsOLD();
		for (String current : list) {
			System.out.println(current);
		}
	}
	
	public void testTmp3() {
		
		//String json = new ScenarioManagementAgent().listScenariosAndAgentsAsJson();
		//System.out.println(json);
//		String dir = "C:/Users/Andreas/my/JPSWorkspace/JParkSimulator-git/JPS_COMPOSITION/testres/admsservicesWithWasteProduct";
//		KeyValueServer.set(ServiceDiscovery.KEY_DIR_KB_AGENTS, dir);
//		System.out.println("dir=" + KeyValueServer.get(ServiceDiscovery.KEY_DIR_KB_AGENTS));
//		
//		ArrayList<Service> services = ServiceDiscovery.getInstance().getServices();
//		for (Service current : services) {
//			System.out.println(current.getOperations().get(0).getHttpUrl());
//		}
	}
	
	public void testTmp4() {
		
		String result =  new ScenarioManagementAgent().listScenariosAndAgentsAsJson();
		
		//String result = new ScenarioManagementAgent().listAgentsAsJson();
		//String result = new ScenarioManagementAgent().listScenariosAsJson();
		System.out.println(result);
	}
	
//	public void testTmp5() {
//		
//		String scenarioName = "aaaa";
//		String agent = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";
//		ScenarioHelper.getScenarioBucket(scenarioName);
//		new ScenarioAgent().createScenarioDescription(scenarioName, agent);
//	}
	
	public void testTmp7() {

		//String plant = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		String plant = "C://JPS_DATA/workingdir/JPS_SCENARIO/and1/-2043594514_Northwest_Kabul_Power_Plant_Afghanistan.owl#Northwest_Kabul_Power_Plant_Afghanistan.owl";
		
		//String result = new QueryBroker().readFile("C://JPS_DATA/workingdir/JPS_SCENARIO/and1/-2043594514_Northwest_Kabul_Power_Plant_Afghanistan.owl#Northwest_Kabul_Power_Plant_Afghanistan");
		
		String sparqlquery = "PREFIX : <http://www.theworldavatar.com/kb/powerplants/> PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> PREFIX system_v1: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#> PREFIX spacetimeext: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#> PREFIX system_performance: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#> PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> SELECT *  WHERE { ?generation system_performance:hasEmission ?emission . ?emission system:hasValue ?emissionvalue .  ?emissionvalue system:numericalValue ?emissionvaluenum . } LIMIT 100";
		//String sparqlquery = EmissionTestAgent.SPARQL_EMISSION;
		
		String result = new QueryBroker().queryFile(plant , sparqlquery);
		
		System.out.println(result);
	}
	

}
