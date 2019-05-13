package uk.ac.cam.cares.jps.powsys.coordination.test;

import java.io.File;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.RDFNode;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.sparql.JenaModelWrapper;
import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.powsys.coordination.CoordinationAgent;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
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
		
		String scenarioUrlOfMockedAgent = "http://localhost:8080/JPS_SCENARIO/scenario/aasc4";
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
		String powerGenerator = "http://localhost:8080/jps/kb/3faa7da1-d1e3-407c-a1c8-86641233f205/nuclearpowerplants/NucGenerator_1_B0.owl#NucGenerator_1_B0";
		new QueryBroker().put(powerGenerator, file);

		assertPropertyValue(1.270333, powerGenerator, PGISCOORDX);
		
		OntModel model = JenaHelper.createModel(powerGenerator);
		new CoordinationAgent().completePowerGenerator(model, powerGenerator);
		
		assertPropertyValue(1.270333, powerGenerator, PGISCOORDX);
		assertPropertyValue(103.719167, powerGenerator, PGISCOORDY);
		String[] pathActivePowerGenerated = new String[] {OPSBEHA, "hasActivePowerGenerated", OCPSYST, "hasValue", OCPSYST, "numericalValue"};
		assertPropertyValue(225.0, powerGenerator, pathActivePowerGenerated);
	}
	
	public void testConnectNuclearPowerPlantsToElectricalNetwork() {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinate");
		JPSHttpServlet.enableScenario(scenarioUrl);	
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		new CoordinationAgent().deletePowerGeneratorsFromElectricalNetwork(TestEN.ELECTRICAL_NETWORK);
		
		// check that the electrical network top node doesn't refer any power generators any more
		String sparqlQuery = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";
		
		String result = new QueryBroker().queryFile(TestEN.ELECTRICAL_NETWORK, sparqlQuery);
		List<String[]> list = JenaResultSetFormatter.convertToListofStringArrays(result, JenaResultSetFormatter.getKeys(result));
		int count = 0;
		for (String[] current : list) {
			String s = current[0];
			if (s.contains("EGen")) {
				count++;
			}
		}
		assertEquals(0, count);
	}
	
	public void testFindOptimalBusNumberForNuclearPowerGenerator() {
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinate"); 
		JPSHttpServlet.enableScenario(scenarioUrl);	
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		OntModel model = ENAgent.readModelGreedy(TestEN.ELECTRICAL_NETWORK);
		List<String> generators = new ArrayList<String>();
		generators.add("http://localhost:8080/jps/kb/371818be-7abb-4b25-9c7f-3e069b499d26/nuclearpowerplants/NucGenerator_2_B0.owl#NucGenerator_2_B0");
		new CoordinationAgent().connectNuclearPowerGeneratorsToOptimalBus(model, generators);
	}
	
	public void testCoordinate() throws URISyntaxException {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSCoordinate"); 
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);	
		System.out.println("MY usecaseUrl=" + usecaseUrl);
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.SCENARIO_URL, scenarioUrl);
		jo.put(JPSConstants.SCENARIO_USE_CASE_URL,  usecaseUrl);
		jo.put("electricalnetwork", TestEN.ELECTRICAL_NETWORK);
		String scenarioUrlOfMockedAgent = "http://localhost:8080/JPS_SCENARIO/scenario/aasc4";
		new CoordinationAgent().coordinate(scenarioUrlOfMockedAgent, jo);
	}
} 