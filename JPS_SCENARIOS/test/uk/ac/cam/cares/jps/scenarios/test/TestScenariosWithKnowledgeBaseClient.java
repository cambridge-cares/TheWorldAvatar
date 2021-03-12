package uk.ac.cam.cares.jps.scenarios.test;

import java.io.File;
import java.util.List;
import java.util.UUID;

import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.scenario.kb.test.TestKnowledgeBaseClient;

public class TestScenariosWithKnowledgeBaseClient extends TestKnowledgeBaseClient {

	static final String ELECTRICAL_NETWORK_IRI = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/JurongIslandPowerNetwork.owl#JurongIsland_PowerNetwork";
	
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
	
	/**
	 * This method will not work since KnowledgeBaseAbstract.query uses RDF4j API instead of Jena for loading and querying a file. 
	 * Since RDF4J is not loading automatically OWL imports - contrary to Jena -, it will not find the triples "?tech j9:hasEmissionFactor ?emm" etc.
	 * That is the result set will be 0 instead of 1. 
	 */
	public void xxxtestQueryFileWithImportStatements() {
		
		String targetUrl = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-008.owl";
		String sparqlQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
				+ "SELECT ?entity ?Pmaxvalue ?emissionfactor " // add the emission value as optional
				+ "WHERE {?entity  a  j1:PowerGenerator  ." 
				//+ "?entity   j2:isSubsystemOf ?plant ." // plant
				+ "?entity   j2:isModeledBy ?model ." 
				+ "?model   j5:hasModelVariable ?pmax ." 
				+ "?pmax  a  j3:PMax  ."
				+ "?pmax  j2:hasValue ?vpmax ." 
				+ "?vpmax   j2:numericalValue ?Pmaxvalue ." // pmax

				+ "?entity j4:realizes ?genprocess ." 
				+ "?genprocess j9:usesGenerationTechnology ?tech ."
				+ "?tech j9:hasEmissionFactor ?emm ."
				+ "?emm j2:hasValue ?valueemm ."
				+ "?valueemm j2:numericalValue ?emissionfactor ." 
				+ "}";		
		
		
		String scenarioName = "testScenariosQueryFileWithImportStatements";
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		System.out.println(scenarioUrl);
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);
		//System.out.println("Copy on read");
		//new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);	
		
		ResultSet resultSet = JenaHelper.queryUrl(targetUrl, sparqlQuery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		System.out.println("expected result: \n" + result);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> expectedResultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		assertEquals(1 , expectedResultList.size());
		assertEquals("0.181", expectedResultList.get(0)[2]);
		
		result = new QueryBroker().queryFile(targetUrl, sparqlQuery);
		keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> actualResultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		assertEquals(1 , actualResultList.size());
		assertEquals("0.181", actualResultList.get(0)[2]);
	}
	
	public void testQueryFileWithImportStatementsSplittedIntoTwoQueries() {
		
		String targetUrl = "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-008.owl";
		
		// first sparql query
		
		String sparqlQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
				+ "SELECT ?entity ?Pmaxvalue ?tech " // add the emission value as optional
				+ "WHERE {?entity  a  j1:PowerGenerator  ." 
				//+ "?entity   j2:isSubsystemOf ?plant ." // plant
				+ "?entity   j2:isModeledBy ?model ." 
				+ "?model   j5:hasModelVariable ?pmax ." 
				+ "?pmax  a  j3:PMax  ."
				+ "?pmax  j2:hasValue ?vpmax ." 
				+ "?vpmax   j2:numericalValue ?Pmaxvalue ." // pmax

				+ "?entity j4:realizes ?genprocess ." 
				+ "?genprocess j9:usesGenerationTechnology ?tech ."
//				+ "?tech j9:hasEmissionFactor ?emm ."
//				+ "?emm j2:hasValue ?valueemm ."
//				+ "?valueemm j2:numericalValue ?emissionfactor ." 
				+ "}";		
		
		
		String scenarioName = "testScenariosQueryFileWithImportStatements";
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		System.out.println(scenarioUrl);
		String usecaseUrl = BucketHelper.getUsecaseUrl(scenarioUrl);
		JPSHttpServlet.enableScenario(scenarioUrl, usecaseUrl);
		//System.out.println("Copy on read");
		//new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);	
		
		String result = new QueryBroker().queryFile(targetUrl, sparqlQuery);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		assertEquals(1 , resultList.size());
		String technology = resultList.get(0)[2];
		assertEquals("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CombinedCycleGasTurbine", technology);
		
		// second sparql query
		
		sparqlQuery = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
				+ "SELECT ?emissionfactor " // add the emission value as optional
				+ "WHERE { " 
				+ "<" + technology + "> j9:hasEmissionFactor ?emm ."
				+ "?emm j2:hasValue ?valueemm ."
				+ "?valueemm j2:numericalValue ?emissionfactor ." 
				+ "}";		
		
		result = new QueryBroker().queryFile(technology, sparqlQuery);
		keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> secondResultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		assertEquals(1 , secondResultList.size());
		assertEquals("0.181", secondResultList.get(0)[0]);
	}
	//Can't run the same test more than once in a setting. Restart the server
	//or restart the scenario with this folder because it deletes the data. 
	public void testQueryBrokerRemoteSparqlDeleteData() {
		
		String scenarioName = "testScenariosQueryBrokerRemoteSparqlDeleteData";
		String scenarioUrl = BucketHelper.getScenarioUrl(scenarioName);
		JPSHttpServlet.enableScenario(scenarioUrl);
		
		QueryBroker broker = new QueryBroker();
		String sparqlQuery = "PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n" + 
				"SELECT * \r\n" +
				"WHERE { ?s OCPSYST:hasSubsystem <%s> . } ";
		sparqlQuery = MiscUtil.format(sparqlQuery, "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-009.owl#EGen-009");
		System.out.println(sparqlQuery);
		String result = broker.queryFile(ELECTRICAL_NETWORK_IRI, sparqlQuery);
		System.out.println(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, "s");
		// we queried for a specific instance; thus we expect only one result row
		assertEquals(1, resultList.size());
		
		// delete instance
		String sparqlUpdate =  "PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n" + 
				"DELETE DATA { <%s> OCPSYST:hasSubsystem <%s> . } \r\n";
		sparqlUpdate = MiscUtil.format(sparqlUpdate, ELECTRICAL_NETWORK_IRI, "http://www.jparksimulator.com/kb/sgp/jurongisland/jurongislandpowernetwork/EGen-009.owl#EGen-009");
		System.out.println(sparqlUpdate);
		broker.updateFile(ELECTRICAL_NETWORK_IRI, sparqlUpdate);

		// check whether the triple was deleted
		result =  broker.queryFile(ELECTRICAL_NETWORK_IRI, sparqlQuery);
		resultList = JenaResultSetFormatter.convertToListofStringArrays(result, "s");
		// we queried for a specific instance that was deleted; thus we expect zero result rows
		assertEquals(0, resultList.size());
	}
}
