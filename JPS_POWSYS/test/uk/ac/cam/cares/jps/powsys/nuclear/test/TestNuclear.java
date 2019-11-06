package uk.ac.cam.cares.jps.powsys.nuclear.test;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
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
import uk.ac.cam.cares.jps.powsys.electricalnetwork.test.TestEN;
import uk.ac.cam.cares.jps.powsys.nuclear.NuclearAgent;

public class TestNuclear extends TestCase {

	public void testStartSimulationAndProcessResultDirectCallForBaseScenario() throws NumberFormatException, IOException, URISyntaxException, InterruptedException { //not tested yet
		NuclearAgent agent = new NuclearAgent();
		JSONObject jofornuc = new JSONObject();
		JSONArray ja = new JSONArray();
		//ja.put("http://www.theworldavatar.com/kb/powerplants/Keppel_Merlimau_Cogen_Power_Plant_Singapore.owl#Keppel_Merlimau_Cogen_Power_Plant_Singapore");
		//ja.put("http://www.theworldavatar.com/kb/powerplants/SembCorp_Pulau_Sakra_CCGT_Cogen_Power_Station_Singapore.owl#SembCorp_Pulau_Sakra_CCGT_Cogen_Power_Station_Singapore");
		//ja.put("http://www.theworldavatar.com/kb/powerplants/Jurong_Island_-_PLP_CCGT_Power_Plant_Singapore.owl#Jurong_Island_-_PLP_CCGT_Power_Plant_Singapore");
		ja.put("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-006.owl#EGen-006");
		jofornuc.put("substitutionalpowerplants", ja);
		
		String lotiri = "http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl";
		String iriofnetwork = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		String dataPath = QueryBroker.getLocalDataPath();
		ArrayList<String> listofplant= new ArrayList<String>();
		
		for (int c=0;c<jofornuc.getJSONArray("substitutionalpowerplants").length();c++) {
			listofplant.add(jofornuc.getJSONArray("substitutionalpowerplants").getString(c));
		}
		
		agent.startSimulation(lotiri, iriofnetwork,listofplant, dataPath, false);
		
		// copy existing result file from a previous simulation to the data bucket 
		String source = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" + "/results.csv";
		File file = new File(source);
		String destinationUrl = dataPath + "/" + NuclearAgent.AGENT_TAG + "/results.csv";
		new QueryBroker().put(destinationUrl, file);
		
		List<String> result = agent.processSimulationResult(dataPath);
		System.out.println("result from processsimulationresult=" + result);
		assertEquals(4, result.size());
	}
	
	public void xxxtestProcessResultDirectCallForScenarioaasc5() throws NumberFormatException, IOException, URISyntaxException, InterruptedException {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("aasc5"); 
		
		String usecaseUrl = scenarioUrl + "/kb/bd1c6d1d-f875-4c50-a7e1-cc28919f1fe7";
		
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);
		
		NuclearAgent agent = new NuclearAgent();
		String dataPath = QueryBroker.getLocalDataPath();
		List<String> result = agent.processSimulationResult(dataPath);
		System.out.println("result from processsimulationresult=" + result);
		assertEquals(4, result.size());
	}
	
public void testStartSimulationAndProcessResultAgentCallForTestScenario() throws NumberFormatException, IOException, URISyntaxException, InterruptedException {
		JSONArray ja = new JSONArray();
		ja.put("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-024.owl#EGen-024");
		ja.put("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-025.owl#EGen-025");
		ja.put("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-026.owl#EGen-026");
		ja.put("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-027.owl#EGen-027");
		ja.put("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-028.owl#EGen-028");
		ja.put("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-029.owl#EGen-029");
		
		JSONObject jo = new JSONObject();
		jo.put("landlot", "http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl");
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		jo.put("substitutionalpowerplants", ja);
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSNuclearStartSimulationAndProcessResultAgentCallForTestScenario"); 
		JPSHttpServlet.enableScenario(scenarioUrl);	
		
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		String usecaseUrl = BucketHelper.getUsecaseUrl();
		System.out.println(usecaseUrl);
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
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		String resultProcess = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/NuclearAgent/processresult", jo.toString());
		System.out.println("result from processsimulationresult=" + resultProcess);
		jo = new JSONObject(resultProcess);
		assertEquals(4, jo.getJSONArray("plants").length());
	}
	
	public void testcallNewNuclearAgentCSVInput() throws IOException, InterruptedException, NumberFormatException, URISyntaxException {
		JSONObject result = new JSONObject();
		JSONArray ja = new JSONArray();
		ja.put("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-006.owl#EGen-006");
		ja.put("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-007.owl#EGen-007");
		ja.put("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-016.owl#EGen-016");
		ja.put("http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-017.owl#EGen-017");
		result.put("substitutionalpowerplants", ja);
		NuclearAgent agent = new NuclearAgent();
		
		String lotiri = "http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl";
		String iriofnetwork = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
		
		ArrayList<String> listofplant= new ArrayList<String>();
		for (int c=0;c<result.getJSONArray("substitutionalpowerplants").length();c++) {
			listofplant.add(result.getJSONArray("substitutionalpowerplants").getString(c));
		}
		
		String dataPath = QueryBroker.getLocalDataPath();
		System.out.println("what is dataPath="+dataPath);
		//agent.startSimulation(lotiri, iriofnetwork,listofplant, dataPath, false);
		agent.prepareCSVPartialRemaining(listofplant,iriofnetwork,dataPath);
		File file = new File(dataPath+"/inputgeneratorselection.csv");
		assertTrue(file.exists());

	}
	
	
}
