package uk.ac.cam.cares.jps.powsys.nuclear.test;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.powsys.nuclear.NuclearAgent;

public class TestNuclear extends TestCase {

	public void testStartSimulationAndProcessResultDirectCallForBaseScenario() throws NumberFormatException, IOException, URISyntaxException, InterruptedException {
		NuclearAgent agent = new NuclearAgent();
		
		String lotiri = "http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl";
		String iriofnetwork = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		String dataPath = QueryBroker.getLocalDataPath();
		agent.startSimulation(lotiri, iriofnetwork, dataPath, false);
		
		// copy existing result file from a previous simulation to the data bucket 
		String source = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" + "/results.csv";
		File file = new File(source);
		String destinationUrl = dataPath + "/" + NuclearAgent.AGENT_TAG + "/results.csv";
		new QueryBroker().put(destinationUrl, file);
		
		List<String> result = agent.processSimulationResult(dataPath);
		System.out.println(result);
		assertEquals(4, result.size());
	}
	
	public void testStartSimulationAndProcessResultAgentCallForTestScenario() throws NumberFormatException, IOException, URISyntaxException, InterruptedException {
		
		JSONObject jo = new JSONObject();
		jo.put("landlot", "http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl");
		jo.put("electricalnetwork", "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario"); 
		JPSHttpServlet.enableScenario(scenarioUrl);	
		
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		String usecaseUrl = BucketHelper.getUsecaseUrl();
		//usecaseUrl = "http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/testStartSimulationAndProcessResultAgentCallForTestScenario/kb/d9fbd6f4-9e2f-4c63-9995-9ff88ab8900e";
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put(JPSConstants.RUN_SIMULATION, false);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		
		System.out.println("json input parameter=" + jo);
		// start simulation (since parameter JPSConstants.SCENARIO_USE_CASE_URL is set, GAMS is not started)
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/NuclearAgent/startsimulation", jo.toString());
		System.out.println("result from startsimulation=" + resultStart);
		
		// copy existing result file from a previous simulation to the data bucket 
		String source = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" + "/results.csv";
		File file = new File(source);
		String destinationUrl = QueryBroker.getLocalDataPath() + "/" + NuclearAgent.AGENT_TAG + "/results.csv";
		new QueryBroker().put(destinationUrl, file);
		
		// process the simulation result
		jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		String resultProcess = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/NuclearAgent/processresult", jo.toString());
		System.out.println("result from processsimulationresult=" + resultProcess);
	}
}
