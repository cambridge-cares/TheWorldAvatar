package uk.ac.cam.cares.jps.scenarios.test;

import java.io.File;
import java.util.UUID;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.scenario.kb.test.TestKnowledgeBaseClient;

public class TestScenariosWithKnowledgeBaseClient extends TestKnowledgeBaseClient {

	protected void setUpFileBasedRemote() {
		if (client() == null) {
			String datasetUrl = KeyValueManager.getServerAddress() + "/jps/dataset/testfilebased";
			System.out.println("creating client for datasetUrl=" + datasetUrl);
			createClient(datasetUrl, false);
		}	
	}
	
	public void setUp() {
		JPSHttpServlet.disableScenario();
		setUpFileBasedRemote();
	}
	
	private String enableScenario(String scenarioName) {
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		JPSHttpServlet.enableScenario(scenarioUrl);	
		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		return scenarioUrl;
	}
	
	public void testGetWithDirectResource() {
		
		// put without scenario, the targetUrl does start with datasetUrl
		String marker = "30.00";
		String targetUrl = datasetUrl + "/some/path/testE-303load.owl";
		putE303LoadRemoteKBCOnly(null, targetUrl, marker);
		
		// get within scenario
		String scenarioUrl = enableScenario("testScenariosWithKbcput");
		String content = KnowledgeBaseClient.get(null, targetUrl, null);
		assertMarkerInE303Load(content, marker);
	}
	
	public void testGetWithIndirectResourceInDataset() {
		
		// put without scenario, the targetUrl does not start with datasetUrl
		String marker = "30.01";
		String targetUrl = "http://www.example.com:3001/some/path/testE-303load.owl";
		putE303Load(targetUrl, marker);
		
		// get within scenario
		String scenarioUrl = enableScenario("testScenariosWithKbcput");
		String content = KnowledgeBaseClient.get(datasetUrl, targetUrl, null);
		assertMarkerInE303Load(content, marker);
	}	
	
	public void testPutWithDirectResource() {
		
		String targetUrl = datasetUrl + "/putwithdirectresource/testE-303load.owl";
		String pathWithoutScenario = BucketHelper.getLocalPath(targetUrl, datasetUrl);
		
		// put with scenario, the targetUrl does start with datasetUrl
		String scenarioUrl = enableScenario("testScenariosWithKbcput");
		String marker = "30.05";
		putE303LoadRemoteKBCOnly(null, targetUrl, marker);
		
		// check that the file was not written to the dataset bucket
		boolean exists = new File(pathWithoutScenario).exists();
		assertFalse(exists);
		
		// check that the file as written to the scenario bucjet
		String content = KnowledgeBaseClient.get(null, targetUrl, null);
		assertMarkerInE303Load(content, marker);
	}
	
	public void testPutWithIndirectResourceInDataset() {
		
		String targetUrl = "http://www.example.com:3001/some6/path/testE-303load.owl";
		String pathWithoutScenario = BucketHelper.getLocalPath(targetUrl, datasetUrl);
		
		// put with scenario, the targetUrl does not start with datasetUrl
		String scenarioUrl = enableScenario("testScenariosWithKbcputIndirect");
		String marker = "30.06";
		putE303LoadRemoteKBCOnly(datasetUrl, targetUrl, marker);
		
		// check that the file was not written to the dataset bucket
		boolean exists = new File(pathWithoutScenario).exists();
		assertFalse(exists);
		
		// check that the file as written to the scenario bucjet
		String content = KnowledgeBaseClient.get(datasetUrl, targetUrl, null);
		assertMarkerInE303Load(content, marker);
	}
	
	public void testUpdateAndQueryWithDirectResource() {
		
		String targetUrl = datasetUrl + "/querywithdirectresource/testE-303load.owl";
		
		// put with scenario, the targetUrl does start with datasetUrl
		String scenarioUrl = enableScenario("testScenariosWithKbcUpdateAndQuery");
		String marker = "30.07";
		putE303LoadRemoteKBCOnly(null, targetUrl, marker);
		
		// assert
		String content = KnowledgeBaseClient.get(null, targetUrl, null);
		assertMarkerInE303Load(content, marker);
		
		// update
		String name = UUID.randomUUID().toString();
		
		String sparqlupdate = "PREFIX dcterms:<http://purl.org/dc/terms/> " + 
				"PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> " + 
				"INSERT DATA { " + 
				"<http://example.com/" + name + "> dcterms:created \"2019-10-20T13:25:13.857\"^^xsd:dateTime . " + 
				"}";
		KnowledgeBaseClient.update(null, targetUrl, sparqlupdate);
		
		// query
		String sparqlquery = "PREFIX dcterms:<http://purl.org/dc/terms/> " + 
				"SELECT ?s ?p ?o WHERE { ?s dcterms:created ?o } ";
		String result = KnowledgeBaseClient.query(null, targetUrl, sparqlquery);
		JSONObject simplified = JenaResultSetFormatter.convertToSimplifiedList(result);
		System.out.println(simplified);
		String subject = simplified.getJSONArray("results").getJSONObject(0).getString("s");
		assertEquals("http://example.com/"+name, subject);
	}
}
