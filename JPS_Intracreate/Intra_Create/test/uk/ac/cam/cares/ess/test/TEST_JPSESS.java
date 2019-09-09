package uk.ac.cam.cares.ess.test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
//import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;


public class TEST_JPSESS extends TestCase {
	
	public static String ELECTRICAL_NETWORK = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	String dataPath = QueryBroker.getLocalDataPath();
	String baseUrl=dataPath+"/JPS_POWSYS_EN";
	

	
		

	public void testStartSimulationPFAgentCallNonBaseScenario() throws IOException  {

		JSONObject jo = new JSONObject();
		
		jo.put("electricalnetwork", ELECTRICAL_NETWORK);
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSENSimulationPFCallAgent");
		JPSHttpServlet.enableScenario(scenarioUrl);	
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);		
		jo.put(JPSConstants.SCENARIO_URL, scenarioUrl);
		
		String usecaseUrl = BucketHelper.getUsecaseUrl();
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		jo.put(JPSConstants.SCENARIO_USE_CASE_URL,  usecaseUrl);
		
		String resultStart = AgentCaller.executeGetWithJsonParameter("Intra_Create/ESSAgent", jo.toString());
	}
	
//	public void testStartSimulationPFDirectCallBaseScenario() throws IOException  {
//
//		String dataPath = QueryBroker.getLocalDataPath();
//		String baseUrl = dataPath + "/JPS_POWSYS_EN";
//		new ENAgent().startSimulation(ELECTRICAL_NETWORK, baseUrl, "PF");	
//	}
	

}
