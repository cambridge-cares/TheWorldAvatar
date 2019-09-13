package uk.ac.cam.cares.jps.powsys.coordination.test;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import org.apache.commons.io.FileUtils;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.powsys.coordination.CoordinationAgent;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.test.TestEN;
import uk.ac.cam.cares.jps.powsys.retrofit.RetrofitAgent;

public class TestCoordinationAgent extends TestCase implements Prefixes, Paths {
	
	
	private void copy(String sourceScenarioName, String destinationScenarioName) throws IOException {
		
		String src = ScenarioHelper.getScenarioBucket(sourceScenarioName);
		String dest =  ScenarioHelper.getScenarioBucket(destinationScenarioName);
		File srcDir = new File(src);
		File destDir = new File(dest);
		
		FileUtils.deleteDirectory(destDir);
		FileUtils.copyDirectory(srcDir, destDir);
	}

	public void testCoordinatePFDirectCall() throws URISyntaxException, IOException {
		
		String scenarioName = "testPOWSYSCoordinatePF";
		copy("aasc5", scenarioName);
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		String scenarioUrlOfMockedAgent = "http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/aasc5";
		jo.put("mergescenariourl", scenarioUrlOfMockedAgent);
			
		new RetrofitAgent().retrofit(jo);
		AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENAgent/startsimulationPF", jo.toString());
	}
	
	public void testCoordinateOPFDirectCall() throws URISyntaxException {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinateOPF");
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		String scenarioUrlOfMockedAgent = "http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/aasc5";
		jo.put("mergescenariourl", scenarioUrlOfMockedAgent);
		
		new RetrofitAgent().retrofit(jo);
		AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENAgent/startsimulationOPF", jo.toString());
	}
	
	public void testCoordinateOPFAgentCall() throws URISyntaxException {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinateOPFAgentCall");
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		String scenarioUrlOfMockedAgent = "http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/aasc5";
		jo.put("mergescenariourl", scenarioUrlOfMockedAgent);
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/processresultwithopf", jo.toString());
		System.out.println("result = " + result);
	}
	
	public void testCoordinateStartSimulationDirectCall() {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinateStartSimulationDirectCall");
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put("carbontax", 52.0);
		jo.put("landlot", "http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl");
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		jo.put(JPSConstants.RUN_SIMULATION, false);
		
		new CoordinationAgent().startSimulation(jo);
	}
	
	public void testCoordinateStartSimulation() {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinateStartSimulation");
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put("carbontax", 64.0);
		jo.put("landlot", "http://www.jparksimulator.com/kb/sgp/jurongisland/JurongIslandLandlots.owl");
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/startsimulation", jo.toString());
		System.out.println(result);
	}
} 