package uk.ac.cam.cares.jps.scenarios.test;

import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
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
	/** test no longer available due to JPS COMPOSITION not running
	 * 
	 */
	public void xxtestCreateScenarioAgent() {
	
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
	/** test no longer available due to JPS COMPOSITION not running
	 * 
	 */
	public void xxtestListScenariosAndAgentsAsJson() {
		
		JSONObject result =  new ScenarioManagementAgent().listScenariosAndAgentsAsJson();		
		//String result = new ScenarioManagementAgent().listAgentsAsJson();
		//String result = new ScenarioManagementAgent().listScenariosAsJson();
		System.out.println(result);
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
	

	
//	public void testTmp5() {
//		
//		String scenarioName = "aaaa";
//		String agent = "http://www.theworldavatar.com/kb/agents/Service__ADMS.owl#Service";
//		ScenarioHelper.getScenarioBucket(scenarioName);
//		new ScenarioAgent().createScenarioDescription(scenarioName, agent);
//	}
	

	
//	public void testTmp6() {
//		
//		String path = "/kb/bd1c6d1d-f875-4c50-a7e1-cc28919f1fe7/nuclearpowerplants/NucGenerator_1_B0.owl";
//	
//		String cutPath = ScenarioHelper.cutHash(path);;
//		String bucket = ScenarioHelper.getScenarioBucket(JPSConstants.SCENARIO_NAME_BASE);
//		String hostport = KeyValueManager.get(IKeys.HOST) + "_" + KeyValueManager.get(IKeys.PORT);
//		String localFile = bucket + "/" + hostport + cutPath;				
//		String result =  new QueryBroker().readFile(localFile); 
//		
//		System.out.println(result);
//	}
	

	public void testTmp7() {
		String s = "{\"entries\":[{\"scenario\":\"aasc4\",\"message\":{\"extendsagent\":\"http://www.theworldavatar.com/kb/agents/Service__ScenarioAgent.owl#Service\"},\"timestamp\":\"2019-05-23 19:07:41.045-SGT\"},{\"scenario\":\"aasc4\",\"message\":{\"agent\":\"http://www.theworldavatar.com/kb/agents/Service__Nuclear.owl#Service\",\"operation\":\"mock\"},\"timestamp\":\"2019-09-10 10:31:53.378-SGT\"}]}";
		JSONObject resultjo = new JSONObject(s);
		
		System.out.println(resultjo.toString(2));
	}
	
	public void testTmp8() {
		
		String s = BucketHelper.getUsecaseUrl();
		System.out.println(s);
	}
	
	public void testTmp9() {
		JPSContext.putScenarioUrl("http://localhost:8080/jps/scenario/13sctax");
		JPSContext.putUsecaseUrl("http://localhost:8080/jps/scenario/13sctax/kb/bd1c6d1d-f875-4c50-a7e1-cc28919f1fe7");
		
		String dataPath = QueryBroker.getLocalDataPath();
		
		System.out.println("path=" + dataPath);
	}
}
