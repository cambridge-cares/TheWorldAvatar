package uk.ac.cam.cares.jps.base.query.test;

import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateExecutionFactory;
import org.apache.jena.update.UpdateFactory;
import org.apache.jena.update.UpdateProcessor;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONObject;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.test.EmissionTestAgent;

public class TestQuery extends TestCase {
	
	public void testJenaResultSetFormatterConvertToCSV() {
		
		String localPlantFile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		ResultSet resultSet = JenaHelper.queryFile(localPlantFile, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		String result = JenaResultSetFormatter.convertToCSV(resultSet);
		System.out.println(result);
	}
	
	public void testJenaResultSetFormatterConvertToJSON() {
		String localPlantFile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		ResultSet resultSet = JenaHelper.queryFile(localPlantFile, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		System.out.println(result);
	}
	
	public void testJenaResultSetFormatterConvertToSimplifiedList() {
		String localPlantFile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		ResultSet resultSet = JenaHelper.queryFile(localPlantFile, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		List<JSONObject> list = JenaResultSetFormatter.convertToSimplifiedList(resultSet);
		assertEquals(1, list.size());
		JSONObject jo = list.get(0);
		System.out.println(jo.toString());
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
		
		List<JSONObject> list = JenaResultSetFormatter.convertToSimplifiedList(json);
		assertEquals(1, list.size());
	}
	
	public void testQueryBrokerLocalSparqlQuery() {
		String plant = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		String result = new QueryBroker().queryFile(plant, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		List<JSONObject> list = JenaResultSetFormatter.convertToSimplifiedList(result);
		assertEquals("15.75", list.get(0).get("emissionvaluenum"));
	}
	
	public void testQueryBrokerRemoteSparqlQuery() {
		String plant = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl";			
		String result = new QueryBroker().queryFile(plant, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		List<JSONObject> list = JenaResultSetFormatter.convertToSimplifiedList(result);
		assertEquals("15.75", list.get(0).get("emissionvaluenum"));
	}
	
	public void testQueryBrokerRemoteSparqlQueryIRIWithHash() {
		String plant = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#Northwest_Kabul_Power_Plant_Afghanistan";			
		String result = new QueryBroker().queryFile(plant, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		List<JSONObject> list = JenaResultSetFormatter.convertToSimplifiedList(result);
		assertEquals("15.75", list.get(0).get("emissionvaluenum"));
	}
	
	private void assertEmissionValue(String path, double expected) {
		String result = new QueryBroker().queryFile(path, EmissionTestAgent.SPARQL_PLANT_QUERY_EMISSION);
		List<JSONObject> list = JenaResultSetFormatter.convertToSimplifiedList(result);
		double actual = list.get(0).getDouble("emissionvaluenum");
		assertEquals(expected, actual);
	}
	
	public void testQueryBrokerLocalSparqlUpdate() {
		
		String plantFile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";
		String plantIri = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#Northwest_Kabul_Power_Plant_Afghanistan";
		String updatedFile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_updated.owl";
		String updatedIri = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_updated.owl";
		
		// copy plantFile to updateFile
		QueryBroker broker = new QueryBroker();
		String content = broker.readFileLocally(plantFile);
		broker.writeFileLocally(updatedFile, content);
		
		// check that copy was successful with the correct initial value
		assertEmissionValue(updatedFile, 15.75);

		// update
		double newEmissionValue = 255.55;
		String query = String.format(EmissionTestAgent.SPARQL_PLANT_UPDATE_EMISSION, newEmissionValue, plantIri);
		broker.updateFile(updatedIri, query);
		
		// check updated value
		assertEmissionValue(updatedIri, newEmissionValue);
	}
	
	public void testSparqlUpdate() {
		
		String plantFile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		assertEmissionValue(plantFile, 15.75);
		
		// update
		double newEmissionValue = 176.13;
		String plantUri = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#Northwest_Kabul_Power_Plant_Afghanistan";
		String query = String.format(EmissionTestAgent.SPARQL_PLANT_UPDATE_EMISSION, newEmissionValue, plantUri);
		UpdateRequest request = UpdateFactory.create(query);
		
		OntModel model = JenaHelper.createModel(plantFile);	
		Dataset dataset = DatasetFactory.wrap(model);
		UpdateProcessor processor = UpdateExecutionFactory.create(request, dataset);
		processor.execute();		
		Model updatedModel = dataset.getDefaultModel();
		
		String updatedFile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_updated.owl";
		JenaHelper.writeAsFile(updatedModel, updatedFile);
		
		// check updated value
		assertEmissionValue(updatedFile, newEmissionValue);
	}
	
	public void testSparqlUpdateSimplified() {

		String plantFile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		assertEmissionValue(plantFile, 15.75);
		
		// update
		double newEmissionValue = 135.98;
		String plantUri = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl#Northwest_Kabul_Power_Plant_Afghanistan";
		String query = String.format(EmissionTestAgent.SPARQL_PLANT_UPDATE_EMISSION, newEmissionValue, plantUri);
		UpdateRequest request = UpdateFactory.create(query);
		
		OntModel model = JenaHelper.createModel(plantFile);	
		UpdateAction.execute(request, model);
		
		String updatedFile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_updated.owl";
		JenaHelper.writeAsFile(model, updatedFile);
		
		// check updated value
		assertEmissionValue(updatedFile, newEmissionValue);
	}
}
