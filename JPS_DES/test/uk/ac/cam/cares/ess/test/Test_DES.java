package uk.ac.cam.cares.ess.test;

import java.io.IOException;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.des.DistributedEnergySystem;


public class Test_DES extends TestCase{
	
	private String ENIRI="http://www.theworldavatar.com/kb/sgp/singapore/singaporepowernetwork/SingaporePowerNetwork.owl#SingaporePowerNetwork";
	
	public void testrunpython() throws IOException {
		DistributedEnergySystem a = new DistributedEnergySystem();
		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_DES";
		a.runOptimization(baseUrl);
	}

	public void testStartDESScenario() throws IOException  {
		

		JSONObject jo = new JSONObject();
	
		jo.put("electricalnetwork", ENIRI);
		
//		String scenarioUrl = BucketHelper.getScenarioUrl("testtest");
//		
//		
//		JPSContext.putScenarioUrl(jo, scenarioUrl);
//		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
//		JPSContext.putUsecaseUrl(jo, usecaseUrl);
//		JPSHttpServlet.enableScenario(scenarioUrl,usecaseUrl);
		//new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_DES/DESAgent", jo.toString());
		System.out.println(resultStart);
		System.out.println("finished execute");

	}
	
	
}
