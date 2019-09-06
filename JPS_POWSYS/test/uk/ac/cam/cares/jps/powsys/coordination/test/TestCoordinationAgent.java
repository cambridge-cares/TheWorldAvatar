package uk.ac.cam.cares.jps.powsys.coordination.test;

import java.io.File;
import java.net.URISyntaxException;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.RDFNode;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.sparql.JenaModelWrapper;
import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;
import uk.ac.cam.cares.jps.powsys.coordination.CoordinationAgent;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.test.TestEN;

public class TestCoordinationAgent extends TestCase implements Prefixes, Paths {

	private void assertPropertyValue(double expected, String url, String... path) {
		OntModel model = JenaHelper.createModel(url);
		JenaModelWrapper w = new JenaModelWrapper(model, null);
		RDFNode o = w.getPropertyValue(url, path);
		double actual = o.asLiteral().getDouble();
		assertEquals(expected, actual);
	}
	
	public void testGetNuclearPowerPlantsFromMockedScenarioAgent() {
		
		String scenarioUrlOfMockedAgent = "http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/aasc5";
		List<String> result = new CoordinationAgent().getNuclearPowerPlantsFromMockedScenarioAgent(scenarioUrlOfMockedAgent);
		assertEquals(4, result.size());
		for (String current : result) {
			assertTrue(current.contains("NucPP"));
		}
	}
	
	public void testCompleteOnePowerGenerator() {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinateCompleteOnePowerGenerator"); 
		JPSHttpServlet.enableScenario(scenarioUrl);	
		//new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		// copy NPP generator OWL test file into the scenario bucket 
		String source = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" + "/NucGenerator_1_B0.owl";
		File file = new File(source);
		String powerGenerator = "http://localhost:8080/jps/kb/bd1c6d1d-f875-4c50-a7e1-cc28919f1fe7/nuclearpowerplants/NucGenerator_1_B0.owl#NucGenerator_1_B0";
		new QueryBroker().put(powerGenerator, file);

		assertPropertyValue(1.270333, powerGenerator, PGISCOORDX);
		
		OntModel model = JenaHelper.createModel(powerGenerator);
		new CoordinationAgent().completePowerGenerator(model, powerGenerator);
		
		assertPropertyValue(1.270333, powerGenerator, PGISCOORDX);
		assertPropertyValue(103.719167, powerGenerator, PGISCOORDY);
		//String[] pathActivePowerGenerated = new String[] {OPSBEHA, "hasActivePowerGenerated", OCPSYST, "hasValue", OCPSYST, "numericalValue"};
		//assertPropertyValue(225.0, powerGenerator, pathActivePowerGenerated);
		String pgIri = PrefixToUrlMap.getPrefixUrl(OPSMODE) + "Pg";
		String[] pathPg = new String[] {OCPSYST, "isModeledBy", OCPMATH, "hasModelVariable", pgIri, OCPSYST, "hasValue", OCPSYST, "numericalValue"};
		assertPropertyValue(225.0, powerGenerator, pathPg);
	}
	
	public void testCoordinatePFDirectCall() throws URISyntaxException {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinatePF");
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JSONObject jo = new JSONObject();
		JPSContext.putScenarioUrl(jo, scenarioUrl);
		JPSContext.putUsecaseUrl(jo, usecaseUrl);
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		String scenarioUrlOfMockedAgent = "http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/aasc5";
		new CoordinationAgent().coordinate(scenarioUrlOfMockedAgent, jo, "PF");
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
		new CoordinationAgent().coordinate(scenarioUrlOfMockedAgent, jo, "OPF");
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
		jo.put("mergescenariourl", "http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/aasc5");
		
		String result = AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/startcombinedsimulationOPF", jo.toString());
		System.out.println("result = " + result);
	}
	
//	public void testENSimulationWithExistingNPPs() throws IOException  {
//
//		//String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinate");
//		//String usecaseUrl = "http://localhost:808" + ScenarioHelper.SCENARIO_COMP_URL + "/testPOWSYSCoordinate/kb/cd66f823-17b3-414b-a5c7-070f760f27cb";
//		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinateTemp");
//		String usecaseUrl = "http://localhost:8080" + ScenarioHelper.SCENARIO_COMP_URL + "/testPOWSYSCoordinate/kb/98d018c8-7ccf-468d-be5b-fad9c9f3b605";
//		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);
//		//function to copy all the owl file involved ???
//		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
//			
//		String dataPath = QueryBroker.getLocalDataPath();
//		String baseUrl = dataPath + "/JPS_POWSYS_EN";
//		new ENAgent().startSimulation(TestEN.ELECTRICAL_NETWORK, baseUrl, "PF");
//	}
} 