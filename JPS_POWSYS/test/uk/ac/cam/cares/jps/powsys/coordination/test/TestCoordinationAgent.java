package uk.ac.cam.cares.jps.powsys.coordination.test;

import java.io.File;
import java.net.URISyntaxException;
import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.RDFNode;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.sparql.JenaModelWrapper;
import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.powsys.coordination.CoordinationAgent;

public class TestCoordinationAgent extends TestCase implements Prefixes, Paths {

	private void assertPropertyValue(double expected, String url, String... path) {
		OntModel model = JenaHelper.createModel(url);
		JenaModelWrapper w = new JenaModelWrapper(model, null);
		RDFNode o = w.getPropertyValue(url, path);
		double actual = o.asLiteral().getDouble();
		assertEquals(expected, actual);
	}
	
	public void testPOWSYSgetNuclearPowerPlantsFromMockedScenarioAgent() {
		
		//String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYSgetNuclearPowerPlantsFromMockedScenarioAgent"); 
		//JPSHttpServlet.enableScenario(scenarioUrl);	
		
		String scenarioUrlOfMockedAgent = "http://localhost:8080/JPS_SCENARIO/scenario/aasc4";
		JSONObject jo = new JSONObject();
		List<String> result = new CoordinationAgent().getNuclearPowerPlantsFromMockedScenarioAgent(scenarioUrlOfMockedAgent, jo);
		assertEquals(4, result.size());
		for (String current : result) {
			assertTrue(current.contains("NucPP"));
		}
	}
	
	public void testPOWSYScompletePowerGenerator() {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testCompletePowerGenerator"); 
		JPSHttpServlet.enableScenario(scenarioUrl);	
		
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
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
	
	public void testPOWSYScoordinate() throws URISyntaxException {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testPOWSYScoordinate"); 
		JPSHttpServlet.enableScenario(scenarioUrl);	
		
		String scenarioUrlOfMockedAgent = "http://localhost:8080/JPS_SCENARIO/scenario/aasc4";
		new CoordinationAgent().coordinate(scenarioUrlOfMockedAgent);
	}
} 
