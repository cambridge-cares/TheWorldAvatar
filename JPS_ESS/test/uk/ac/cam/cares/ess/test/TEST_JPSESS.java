package uk.ac.cam.cares.ess.test;

import java.io.IOException;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.ess.JPS_ESS;


public class TEST_JPSESS extends TestCase {
	
	public static String ELECTRICAL_NETWORK = "http://www.jparksimulator.com/kb/sgp/pvsingaporenetwork/PVSingaporeNetwork.owl#PVSingaporeNetwork";
//	String dataPath = QueryBroker.getLocalDataPath();
//	String baseUrl=dataPath+"/JPS_ESS";
	

	
		

	public void testStartSimulationPFAgentCallBaseScenario() throws IOException  {

		JSONObject jo = new JSONObject();
		
		jo.put("PVNetwork", ELECTRICAL_NETWORK);
		
		//String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSENSimulationPFCallAgent");
		//JPSHttpServlet.enableScenario(scenarioUrl);	
		//new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);		
		//jo.put(JPSConstants.SCENARIO_URL, scenarioUrl);
		
		//String usecaseUrl = BucketHelper.getUsecaseUrl();
		//JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		//jo.put(JPSConstants.SCENARIO_USE_CASE_URL,  usecaseUrl);
		System.out.println(jo.toString());
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_ESS/ESSAgent", jo.toString());
		System.out.println(resultStart);
		System.out.println("finished execute");
	}
	
	public void testCreateCSV() throws IOException  {

		String dataPath = QueryBroker.getLocalDataPath();
		String baseUrl = dataPath + "/JPS_ESS";
		new JPS_ESS().prepareCSV(ELECTRICAL_NETWORK, baseUrl);	
	}
	public void testModifyTemplate() throws IOException{
		JPS_ESS a = new JPS_ESS();
		a.modifyTemplate("D:/", "NESS.gms") ;
	}

}
