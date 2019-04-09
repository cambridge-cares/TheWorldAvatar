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
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.powsys.nuclear.NuclearAgentScenarioCapable;

public class TestNuclearAgentScenarioCapable extends TestCase {

	public void testStartSimulationAndProcessResultDirectCallForBaseScenario() throws NumberFormatException, IOException, URISyntaxException, InterruptedException {

		NuclearAgentScenarioCapable agent = new NuclearAgentScenarioCapable();
		
		String lotiri = "http://www.theworldavatar.com/kb/sgp/jurongisland/JurongIslandLandlots.owl";
		String iriofnetwork = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		String baseUrl = QueryBroker.getUniqueTaggedDataScenarioUrl("test_JPS_POWSYS_npp");
		agent.startSimulation(lotiri, iriofnetwork, baseUrl, false);
		
		// copy existing result file from a previous simulation to the data bucket 
		String source = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres/results.csv";
		File file = new File(source);
		String destinationUrl = baseUrl + "/results.csv";
		new QueryBroker().put(destinationUrl, file);
		
		List<String> result = agent.processSimulationResult(baseUrl);
		System.out.println(result);
	}
	
	public void testStartSimulationAndProcessResultAgentCallForTestScenario() throws NumberFormatException, IOException, URISyntaxException, InterruptedException {
		
		JSONObject jo = new JSONObject();
		jo.put("landlot", "http://www.theworldavatar.com/kb/sgp/jurongisland/JurongIslandLandlots.owl");
		jo.put("electricalnetwork", "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork");
		String scenarioUrl = BucketHelper.getScenarioUrl("scenariotest");
		jo.put(JPSConstants.SCENARIO_URL, scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl);	
		String baseUrl = QueryBroker.getUniqueTaggedDataScenarioUrl("test_JPS_POWSYS_npp");
		jo.put("baseUrl",  baseUrl);
		
		// start simulation (since parameter "baseUrl" is set, GAMS is not started)
		String resultStart = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/NuclearAgent/startsimulation", jo.toString());
		System.out.println("result from startsimulation=" + resultStart);
		
		// copy existing result file from a previous simulation to the data bucket 
		String source = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres/results.csv";
		File file = new File(source);
		String destinationUrl = baseUrl + "/results.csv";
		new QueryBroker().put(destinationUrl, file);
		
		// process the simulation result
		jo = new JSONObject();
		jo.put(JPSConstants.SCENARIO_URL, scenarioUrl);
		jo.put("baseUrl",  baseUrl);	
		String resultProcess = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/NuclearAgent/processresult", jo.toString());
		System.out.println("result from processsimulationresult=" + resultProcess);
	}
}
