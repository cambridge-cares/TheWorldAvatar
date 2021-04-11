package uk.ac.cam.cares.jps.base.scenario.test;

import org.apache.jena.ontology.OntModel;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;

public class TestScenario extends TestCase {

	public void testDividePath() {
		
		// auto generate a scenario id
		String path = "/";
		String[] actual= ScenarioHelper.dividePath(path);
		assertNotNull(actual[0]);
		assertNull(actual[1]);
		
		// throws an exception because path (without /) does not contain at least 10 characters
		path = "/123456789";
		try {
			ScenarioHelper.dividePath(path);
		} catch (Exception e) {
		}
	
		path = "/1234567890";
		actual= ScenarioHelper.dividePath(path);
		assertEquals("1234567890", actual[0]);
		assertNull(actual[1]);
		
		path = "/1234567890/any/fancy/operation";
		actual= ScenarioHelper.dividePath(path);
		assertEquals("1234567890", actual[0]);
		assertEquals("/any/fancy/operation", actual[1]);
	}
	
	public void testGetLocalPathBaseScenarioForKB() {
		
		String root = KeyValueManager.get("absdir.root");
		
		String url = "http://www.theworldavatar.com/kb/sgp/jurongisland/something.owl";
		String scenarioUrl = null;
		String path = BucketHelper.getLocalPath(url, scenarioUrl);
		assertEquals(root + "/kb/sgp/jurongisland/something.owl", path);
		
		url = "http://www.jparksimulator.com/data/something.csv";
		scenarioUrl = null;
		path = BucketHelper.getLocalPath(url, scenarioUrl);
		assertEquals(root + "/data/something.csv", path);
		
		try {
			url = "http://www.theworldavatar.com/kb/sgp/jurongisland/something.owl";
			scenarioUrl = BucketHelper.getScenarioUrl(JPSConstants.SCENARIO_NAME_BASE);
			JPSHttpServlet.enableScenario(scenarioUrl);
			path = BucketHelper.getLocalPath(url, scenarioUrl);
			assertEquals(root + "/kb/sgp/jurongisland/something.owl", path);
		} finally {
			JPSHttpServlet.disableScenario();
		}
	}
	
	public void testGetLocalPathBaseScenarioForJPSKB() {
		
		String root = ScenarioHelper.getScenarioWorkingDir() + "/" + JPSConstants.SCENARIO_NAME_BASE;		
		String url = "http://www.theworldavatar.com" + ScenarioHelper.SCENARIO_COMP_URL + "/base/kb/sgp/jurongisland/something.owl";
		String scenarioUrl = null;
		String path = BucketHelper.getLocalPath(url, scenarioUrl);
		assertEquals(root + "/www_theworldavatar_com/kb/sgp/jurongisland/something.owl", path);
		
		url = "http://www.jparksimulator.com" + ScenarioHelper.SCENARIO_COMP_URL + "/base/data/something.csv";
		scenarioUrl = null;
		path = BucketHelper.getLocalPath(url, scenarioUrl);
		assertEquals(root + "/www_jparksimulator_com/data/something.csv", path);
		
		try {
			url = "http://www.theworldavatar.com" + ScenarioHelper.SCENARIO_COMP_URL + "/base/kb/sgp/jurongisland/something.owl";
			scenarioUrl = BucketHelper.getScenarioUrl(JPSConstants.SCENARIO_NAME_BASE);
			JPSHttpServlet.enableScenario(scenarioUrl);
			path = BucketHelper.getLocalPath(url, scenarioUrl);
			assertEquals(root + "/www_theworldavatar_com/kb/sgp/jurongisland/something.owl", path);
		} finally {
			JPSHttpServlet.disableScenario();
		}
	}
	
	public void testGetLocalPathNonBaseScenario() {
		
		String scenarioName = "testmy123";
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		//String root = AgentLocator.getPathToJpsWorkingDir() + ScenarioHelper.SCENARIO_COMP_URL + "/" + scenarioName;
		String root = ScenarioHelper.getScenarioBucket(scenarioName);
		
		String url = "http://www.theworldavatar.com/jps/kb/sgp/jurongisland/something.owl";
		String path = BucketHelper.getLocalPath(url, scenarioUrl);
		assertEquals(root + "/www_theworldavatar_com/kb/sgp/jurongisland/something.owl", path);
		
		url = "http://www.jparksimulator.com/jps/data/something.csv";
		path = BucketHelper.getLocalPath(url, scenarioUrl);
		assertEquals(root + "/www_jparksimulator_com/data/something.csv", path);
		
		try {
			url = "http://www.theworldavatar.com/jps/kb/sgp/jurongisland/something.owl";
			JPSHttpServlet.enableScenario(scenarioUrl);
			path = BucketHelper.getLocalPath(url, scenarioUrl);
			assertEquals(root + "/www_theworldavatar_com/kb/sgp/jurongisland/something.owl", path);
		} finally {
			JPSHttpServlet.disableScenario();
		}
	}
	
	public void testGetLocalDataPathBaseScenario() {
		String root = ScenarioHelper.getScenarioWorkingDir() + "/" + JPSConstants.SCENARIO_NAME_BASE;	
		String path = BucketHelper.getLocalDataPath();	
		System.out.println(root);
		System.out.println(path);
		assertTrue(path.startsWith(root));
		assertTrue(path.contains(JPSConstants.SCENARIO_SUBDIR_DATA));
	}
	
	public void testGetLocalDataPathNonBaseScenario() {

		String scenarioName = "testmy123";
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);

		try {
			JPSHttpServlet.enableScenario(scenarioUrl);
			String root = ScenarioHelper.getScenarioWorkingDir() + "/" + scenarioName;	
			String path = BucketHelper.getLocalDataPath();
			System.out.println(root);
			System.out.println(path);
			assertTrue(path.startsWith(root));
			assertTrue(path.contains(JPSConstants.SCENARIO_SUBDIR_DATA));
		} finally {
			JPSHttpServlet.disableScenario();
		}
	}
	
	public void testGetLocalPathOnLocalhost() {
		JPSHttpServlet.disableScenario();
		String resource = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EBus-174.owl#V_Pd_EBus-174";
		String path = BucketHelper.getLocalPath(resource);
		System.out.println(path);
		assertTrue(path.contains("ROOT"));
	}
	
	public void testGetLocalPathFancyBaseScenario() {
		JPSHttpServlet.disableScenario();
		String resource = "http://localhost:9090/fancy/path/some.owl";
		String path = BucketHelper.getLocalPath(resource);
		System.out.println(path);
	}
	
	public void testGetLocalDataPathForAnyDatasetUrl() {

		JPSHttpServlet.disableScenario();
		String datasetName = "testdataset123";
		String url = "http://www.example.com:6000/some/path/fancy.csv";
		
		try {
			
			String datasetUrl = KeyValueManager.getServerAddress() + JPSConstants.KNOWLEDGE_BASE_PATH_JPS_DATASET + "/" + datasetName;	
			String expected = ScenarioHelper.getJpsWorkingDir() + "/JPS_SCENARIO/dataset/" + datasetName + "/www_example_com_6000/some/path/fancy.csv"; 
			String path = BucketHelper.getLocalPath(url, datasetUrl);
			System.out.println(datasetUrl);
			System.out.println(expected);
			System.out.println(path);
			assertEquals(expected, path);
		} finally {
			JPSHttpServlet.disableScenario();
		}
	}
	
	public void testGetLocalDataPathForAnyDatasetUrl2() {

		JPSHttpServlet.disableScenario();
		String datasetUrl = "http://localhost:8081/jps/dataset/testfilebased";
		String url = "http://www.example.com:3001/testScenariosWithKbcput/testE-303load.owl";
		
		String path = BucketHelper.getLocalPath(url, datasetUrl);
		String expected = "C:/JPS_DATA/workingdir/JPS_SCENARIO/dataset/testfilebased/www_example_com_3001/testScenariosWithKbcput/testE-303load.owl";
		System.out.println(datasetUrl);
		System.out.println(expected);
		System.out.println(path);
		assertEquals(expected, path.replace("\\", "/"));
	}

	public void testGetIriPrefixBaseScenario() {
		String prefix = BucketHelper.getIriPrefix();
		System.out.println(prefix);
		assertTrue(prefix.contains("/jps/kb"));
	}
	
	public void testGetIriPrefixNonBaseScenario() {
		String scenarioName = "testGetIriPrefixNonBaseScenario";
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		System.out.println("scenarioUrl=" + scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl);		
		try {
			String prefix = BucketHelper.getIriPrefix();
			System.out.println(prefix);
			assertTrue(prefix.contains("/jps/kb"));
		} finally {
			JPSHttpServlet.disableScenario();
		}
	}
	
	public void testGetHashedResource() {
		String resource = "https://www.jparksimulator.com:8080/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#abctest";
		String hashedResource = ScenarioHelper.getHashedResource(resource);
		assertEquals("-1820947590/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl", hashedResource);
	}
	
	public void testJenaReadHook() {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testJenaReadHook");
		JPSHttpServlet.enableScenario(scenarioUrl);	
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		// the URL for the OWL file for EBus-174 is transformed into a read call to the scenario agent within 
		// the call JenaHelper.createMode(url) below
		String url = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EBus-174.owl#V_Pd_EBus-174";
		
		long start = System.currentTimeMillis();
		OntModel model = JenaHelper.createModel(url);
		long diff = System.currentTimeMillis() - start;	
		System.out.println("diff=" + diff);
	}
	
	public static void main(String[] args){
		new TestScenario().xxxtestReadScenarioAgentPerformance();
	}
	
	public void xxxtestReadScenarioAgentPerformance() {
		
		String scenarioUrl = BucketHelper.getScenarioUrl("testReadScenarioPerformance");
		JPSHttpServlet.enableScenario(scenarioUrl);	
		//new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		String url = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EBus-034.owl";
		//String url = "http://localhost:8080/kb/sgp/jurongisland/jurongislandpowernetwork/EBus-034.owl";

		long start = System.currentTimeMillis();
		for (int i=0; i<10; i++) {
			String testResult = new QueryBroker().readFile(url);
			System.out.println(testResult);
		}

		long diff = System.currentTimeMillis() - start;	
		System.out.println("diff=" + diff);
	}
	
	public void testPingScenarioAgentPerformance() {
		
		String url = KeyValueManager.getServerAddress() + ScenarioHelper.SCENARIO_COMP_URL + "/testPingScenarioAgentPerformance/ping";
		long start = System.currentTimeMillis();
		for (int i=0; i<10; i++) {
			String result = AgentCaller.executeGetWithURL(url);
		}
		
		long diff = System.currentTimeMillis() - start;	
		System.out.println("diff=" + diff);
	}
	
	public void testJPSContext() {
		
		JSONObject jo = new JSONObject();
		jo.put("key1", "value1");
		jo.put("key2", "value2");
		JPSContext.putScenarioUrl(jo, "scenarioabc");
		jo.put("key3", "value3");
		JPSContext.putScenarioUrl(jo, "scenarioxyz");
		jo.put("key4", "value4");
		
		String actual = JPSContext.getScenarioUrl(jo);
		assertEquals("scenarioxyz", actual);
		System.out.println(jo);
	}
}
