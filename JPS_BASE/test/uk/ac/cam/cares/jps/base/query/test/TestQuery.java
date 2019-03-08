package uk.ac.cam.cares.jps.base.query.test;

import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.test.EmissionTestAgent;

public class TestQuery extends TestCase implements ITestConstants{
	
	private void copyPowerPlantAfg() {
		String sourceFile = ResourcePathConverter.convertToLocalPath(POWER_PLANT_AFG_FILE_ORIGINAL);
		String targetFile = ResourcePathConverter.convertToLocalPath(POWER_PLANT_AFG_FILE);
		QueryBroker broker = new QueryBroker();
		String content = broker.readFileLocally(sourceFile);
		QueryBroker.writeFileLocally(targetFile, content);
	}
	
	public void setUp() {
		System.out.println("MY HELLO ");
		
		String root = KeyValueManager.get("absdir.root");
		System.out.println("MY ROOT " + root);
		
	
		
		copyPowerPlantAfg();
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
	
	public void testQueryBrokerReadWithConversion() {
		String result = new QueryBroker().readFile(POWER_PLANT_AFG_FILE);
		assertTrue(result.startsWith("<rdf:RDF"));
	}
	
	public void testQueryBrokerLocalSparqlQuery() {			
		String plantFile = ResourcePathConverter.convertToLocalPath(POWER_PLANT_AFG_FILE);		
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
	
	private void assertEmissionValue(String path, double expected) {
		String result = new QueryBroker().queryFile(path, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		JSONArray list = JenaResultSetFormatter.convertToSimplifiedList(result).getJSONArray("results");
		double actual = list.getJSONObject(0).getDouble("emissionvaluenum");
		assertEquals(expected, actual);
	}
	
	public void testQueryBrokerRemoteSparqlUpdate() {
		// check that copy was successful with the correct initial value
		assertEmissionValue(POWER_PLANT_AFG_FILE, 15.75);

		// update
		String plantIri = POWER_PLANT_AFG_IRI;
		double newEmissionValue = 255.55;
		String query = String.format(EmissionTestAgent.SPARQL_PLANT_UPDATE_EMISSION, newEmissionValue, plantIri);
		new QueryBroker().updateFile(plantIri, query);
		
		// check updated value
		String localFile = ResourcePathConverter.convertToLocalPath(POWER_PLANT_AFG_FILE);
		assertEmissionValue(localFile, newEmissionValue);
	}
}
