package uk.ac.cam.cares.jps.base.query.test;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.test.EmissionTestAgent;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

public class TestQuery extends TestCase implements ITestConstants{
	
	private void copy(String from, String to) {
		String sourceFile = ResourcePathConverter.convertToLocalPath(from);
		String targetFile = ResourcePathConverter.convertToLocalPath(to);
		String content = FileUtil.readFileLocally(sourceFile);
		FileUtil.writeFileLocally(targetFile, content);
	}
	
	public void setUp() {
		copy(POWER_PLANT_AFG_FILE_ORIGINAL, POWER_PLANT_AFG_FILE);
		copy(ELECTRICAL_NETWORK_FILE_ORIGINAL, ELECTRICAL_NETWORK_FILE);
	}
	
	public void testJenaResultSetFormatterConvertToCSV() {
		String plantFile = ResourcePathConverter.convertToLocalPath(POWER_PLANT_AFG_FILE);		
		ResultSet resultSet = JenaHelper.queryFile(plantFile, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		String result = JenaResultSetFormatter.convertToCSV(resultSet);
		System.out.println(result);
		assertTrue(result.contains("15.75"));
	}
	
	public void testJenaResultSetFormatterConvertToJSON() {
		String plantFile = ResourcePathConverter.convertToLocalPath(POWER_PLANT_AFG_FILE);		
		ResultSet resultSet = JenaHelper.queryFile(plantFile, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		System.out.println(result);
		JSONObject jo = new JSONObject(result);
		JSONObject getFirstResult = jo.getJSONObject("results").getJSONArray("bindings").getJSONObject(0);
		assertEquals("15.75", getFirstResult.getJSONObject("emissionvaluenum").getString("value"));
	}
	
	public void testJenaResultSetFormatterConvertToSimplifiedList() {
		String plantFile = ResourcePathConverter.convertToLocalPath(POWER_PLANT_AFG_FILE);		
		ResultSet resultSet = JenaHelper.queryFile(plantFile, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		JSONObject joOrig = JenaResultSetFormatter.convertToSimplifiedList(resultSet);
		System.out.println(joOrig.toString());
		JSONArray list = joOrig.getJSONArray("results");
		assertEquals(1, list.length());
		JSONObject jo = list.getJSONObject(0);
		assertEquals("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGasGeneration", jo.get("generation"));
		assertEquals("15.75", jo.get("emissionvaluenum"));
	}
	
	
	public void testJenaResultSetFormatterConvertToSimplifiedListResultOnly( ) {
		String json = "{\r\n" + 
				"  \"head\": {\r\n" + 
				"    \"vars\": [ \"generation\" , \"emission\" , \"emissionvalue\" , \"emissionvaluenum\" ]\r\n" + 
				"  } ,\r\n" + 
				"  \"results\": {\r\n" + 
				"    \"bindings\": [\r\n" + 
				"      {\r\n" + 
				"        \"generation\": { \"type\": \"uri\" , \"value\": \"http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGasGeneration\" } ,\r\n" + 
				"        \"emission\": { \"type\": \"uri\" , \"value\": \"http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#CO2Emission_of_Northwest_Kabul_Power_Plant_Afghanistan\" } ,\r\n" + 
				"        \"emissionvalue\": { \"type\": \"uri\" , \"value\": \"http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#v_CO2Emission_of_Northwest_Kabul_Power_Plant_Afghanistan\" } ,\r\n" + 
				"        \"emissionvaluenum\": { \"type\": \"literal\" , \"value\": \"15.75\" }\r\n" + 
				"      }\r\n" + 
				"    ]\r\n" + 
				"  }\r\n" + 
				"}";
		
		JSONArray list = JenaResultSetFormatter.convertToSimplifiedList(json).getJSONArray("results");
		assertEquals(1, list.length());
	}
	
	public void testJenaResultSetFormatterconvertToListofStringArrays() {
		String plantFile = ResourcePathConverter.convertToLocalPath(POWER_PLANT_AFG_FILE);		
		ResultSet resultSet = JenaHelper.queryFile(plantFile, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, "emissionvaluenum", "generation");
		for (String[] current : resultList) {
			System.out.println(current[0] + ", " + current[1]);
		}
		
		assertEquals(1, resultList.size());
	}
	
	public void testJenaReadHook() throws MalformedURLException {
		try {
			String scenarioUrl = BucketHelper.getScenarioUrl("testscenariojenareadhook");
			new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
			JPSHttpServlet.enableScenario(scenarioUrl);		
			
			OntModel model = JenaHelper.createModel();	
			String url = ELECTRICAL_NETWORK_IRI;
			JenaHelper.readFromUrl(new URL(url), model);
			long size = model.listImportedOntologyURIs().size();
			System.out.println("size imports=" + size);
			assertEquals(5, size);
			size = model.size();
			System.out.println("size triples=" + size);
			assertEquals(7812, size);
		} finally {
			JPSHttpServlet.disableScenario();
		}
	}
	
	public void testQueryBrokerReadWithConversion() {
		String result = new QueryBroker().readFile(POWER_PLANT_AFG_FILE);
		assertTrue(result.startsWith("<rdf:RDF"));
	}
	
	public void testQueryBrokerLocalSparqlQuery() {			
		//String plantFile = ResourcePathConverter.convertToLocalPath(POWER_PLANT_AFG_FILE);		
		String plantFile = POWER_PLANT_AFG_FILE;	
		String result = new QueryBroker().queryFile(plantFile, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		JSONArray list = JenaResultSetFormatter.convertToSimplifiedList(result).getJSONArray("results");
		assertEquals("15.75", list.getJSONObject(0).get("emissionvaluenum"));
	}
	
	public void testQueryBrokerRemoteSparqlQuery() {
		String result = new QueryBroker().queryFile(POWER_PLANT_AFG_FILE, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		JSONArray list = JenaResultSetFormatter.convertToSimplifiedList(result).getJSONArray("results");
		assertEquals("15.75", list.getJSONObject(0).get("emissionvaluenum"));
	}
	
	public void testQueryBrokerRemoteSparqlQueryIRIWithHash() {
		String result = new QueryBroker().queryFile(POWER_PLANT_AFG_FILE, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		JSONArray list = JenaResultSetFormatter.convertToSimplifiedList(result).getJSONArray("results");
		assertEquals("15.75", list.getJSONObject(0).get("emissionvaluenum"));
	}
	
	public void testQueryBrokerQueryFilesGreedy() {
		
//		String scenarioUrl = BucketHelper.getScenarioUrl("testGREEDY");
//		JPSHttpServlet.enableScenario(scenarioUrl);
//		new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
		
		String greedySparqlQuery = "PREFIX sys:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity a sys:CompositeSystem . " 
				+ "?entity sys:hasSubsystem ?component . "								
				+ "}";
		String secondSparqlQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?entity "
				+ "WHERE { ?entity a j1:BusNode . ?entity j2:isModeledBy ?model . }";
		
		String result = new QueryBroker().queryFilesGreedy(ELECTRICAL_NETWORK_IRI, greedySparqlQuery, secondSparqlQuery);
	
		System.out.println(result);
		
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, "entity");
		assertEquals(208, resultList.size());
	}
	
	public void xxxtestQueryBrokerQueryFilesGreedyForNonBaseScenarioWithCopyOnRead() {
		
		String greedySparqlQuery = "PREFIX sys:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity a sys:CompositeSystem . " 
				+ "?entity sys:hasSubsystem ?component . "								
				+ "}";
		String secondSparqlQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> " 
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?entity "
				+ "WHERE { ?entity a j1:BusNode . ?entity j2:isModeledBy ?model . }";
		
		try {
			String scenarioUrl = BucketHelper.getScenarioUrl("testgreedy");
			new ScenarioClient().setOptionCopyOnRead(scenarioUrl, true);
			JPSHttpServlet.enableScenario(scenarioUrl);		
			
			String result = new QueryBroker().queryFilesGreedy(ELECTRICAL_NETWORK_IRI, greedySparqlQuery, secondSparqlQuery);
			
			System.out.println(result);
			
			List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, "entity");
			assertEquals(208, resultList.size());
			
		} finally {
			JPSHttpServlet.disableScenario();
		}
	}
	
	private void assertEmissionValue(String path, double expected) {
		QueryBroker broker = new QueryBroker();
		String result = broker.queryFile(path, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		System.out.println(result);
		JSONArray list = JenaResultSetFormatter.convertToSimplifiedList(result).getJSONArray("results");
		double actual = list.getJSONObject(0).getDouble("emissionvaluenum");
		assertEquals(expected, actual);
	}
	
	public void testQueryBrokerRemoteSparqlUpdate() throws InterruptedException {
		// check that copy was successful with the correct initial value
		assertEmissionValue(POWER_PLANT_AFG_FILE, 15.75);

		// update
		String plantIri = POWER_PLANT_AFG_IRI;
		double newEmissionValue = 255.55;
		String query = MiscUtil.format(EmissionTestAgent.SPARQL_PLANT_UPDATE_EMISSION, newEmissionValue, plantIri);
		new QueryBroker().updateFile(POWER_PLANT_AFG_FILE, query);
		
		// check updated value
		//String localFile = ResourcePathConverter.convertToLocalPath(POWER_PLANT_AFG_FILE);
		//String localFile = ResourcePathConverter.convert(POWER_PLANT_AFG_FILE);	
		// For some reasons, we need a time delay here. Otherwise the test will fail. No idea why.
		TimeUnit.SECONDS.sleep(5);
		
		assertEmissionValue(POWER_PLANT_AFG_FILE, newEmissionValue);
	}
	
	public void testQueryBrokerGetLocalDataPath() {
		String path = QueryBroker.getLocalDataPath("ADMS");
		System.out.println(path);
		assertTrue(path.endsWith("_ADMS"));
	}
}
