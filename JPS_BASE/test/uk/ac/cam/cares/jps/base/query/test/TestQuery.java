package uk.ac.cam.cares.jps.base.query.test;

import java.util.List;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
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

public class TestQuery extends TestCase {
	
	public static final String SPARQL_PREFIXES = "PREFIX : <http://www.theworldavatar.com/kb/powerplants/>\r\n"
			+ "PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>\r\n"
			+ "PREFIX system_v1: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#>\r\n"
			+ "PREFIX spacetimeext: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n"
			+ "PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n"
			+ "PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>\r\n"
			+ "PREFIX system_performance: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>\r\n"
			+ "PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>\r\n";
	
	
	// TODO-AE SC URGENT reuse other queries and prefixes, replace PREFIX : by keyword BASE
	public final static String SPARQL_PLANT_QUERY_EMISSION = SPARQL_PREFIXES + 
			"SELECT * \r\n" + 
			"WHERE {\r\n" + 
			"?generation system_performance:hasEmission ?emission .\r\n" + 
			"?emission system:hasValue ?emissionvalue . \r\n" + 
			"?emissionvalue system:numericalValue ?emissionvaluenum .\r\n" + 
			"}\r\n" + 
			"LIMIT 100";
	
	// TODO-AE SC URGENT reuse other queries and prefixes, replace PREFIX : by keyword BASE
//	public final static String SPARQL_EMISSION = "PREFIX : <http://www.theworldavatar.com/kb/powerplants/>\r\n" + 
//			"PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>\r\n" + 
//			"PREFIX system_v1: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#>\r\n" + 
//			"PREFIX spacetimeext: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
//			"PREFIX system:	<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n" + 
//			"PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>\r\n" + 
//			"PREFIX system_performance: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#>\r\n" + 
//			"PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>\r\n" + 
//			"SELECT * \r\n" + 
//			"WHERE {\r\n" + 
//			"?generation system_performance:hasEmission ?emission .\r\n" + 
//			"?emission system:hasValue ?emissionvalue . \r\n" + 
//			"?emissionvalue system:numericalValue ?emissionvaluenum .\r\n" + 
//			"}\r\n" + 
//			"LIMIT 100";
	
	private static final String SPARQL_PLANT_UPDATE_EMISSION = SPARQL_PREFIXES 
			+ "DELETE { ?emissionvalue system:numericalValue ?emissionvaluenum .} "
			+ "INSERT { ?emissionvalue system:numericalValue %f .} "
			+ "WHERE { <%s> technical_system:realizes ?generation . ?generation system_performance:hasEmission ?emission . ?emission system:hasValue ?emissionvalue . "
			+ "?emissionvalue system:numericalValue ?emissionvaluenum . }";
	
	
	private static final String SPARQL_PLANT_TEST = SPARQL_PREFIXES 
			+ "DELETE { ?emissionvalue system:numericalValue ?emissionvaluenum .} "
			+ "WHERE { ?emissionvalue system:numericalValue ?emissionvaluenum . }";
	
	
	public void testJenaResultSetFormatterConvertToCSV() {
		
		String localPlantFile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		ResultSet resultSet = JenaHelper.queryFile(localPlantFile, SPARQL_PLANT_QUERY_EMISSION);
		String result = JenaResultSetFormatter.convertToCSV(resultSet);
		System.out.println(result);
	}
	
	public void testJenaResultSetFormatterConvertToJSON() {
		String localPlantFile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		ResultSet resultSet = JenaHelper.queryFile(localPlantFile, SPARQL_PLANT_QUERY_EMISSION);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		System.out.println(result);
	}
	
	public void testJenaResultSetFormatterConvertToSimplifiedList() {
		String localPlantFile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		ResultSet resultSet = JenaHelper.queryFile(localPlantFile, SPARQL_PLANT_QUERY_EMISSION);
		List<JSONObject> list = JenaResultSetFormatter.convertToSimplifiedList(resultSet);
		assertEquals(1, list.size());
		JSONObject jo = list.get(0);
		System.out.println(jo.toString());
		assertEquals("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGasGeneration", jo.get("generation"));
		assertEquals("15.75", jo.get("emissionvaluenum"));
	}
	
	public void testQueryBrokerLocalSparqlQuery() {
		String plant = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";		
		String result = new QueryBroker().queryFile(plant, SPARQL_PLANT_QUERY_EMISSION);
		List<JSONObject> list = JenaResultSetFormatter.convertToSimplifiedList(result);
		assertEquals("15.75", list.get(0).get("emissionvaluenum"));
	}
	
	public void testQueryBrokerRemoteSparqlQuery() {
		String plant = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl";			
		String result = new QueryBroker().queryFile(plant, SPARQL_PLANT_QUERY_EMISSION);
		List<JSONObject> list = JenaResultSetFormatter.convertToSimplifiedList(result);
		assertEquals("15.75", list.get(0).get("emissionvaluenum"));
	}
	
	public void todotestQueryBrokerLocalSparqlUpdate() {
		// TODO-AE SC copy the file first since it is changed
		String plantfile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Kabul_Power_Plant_Afghanistan.owl";	
		String planturi = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl";	
		String query = String.format(SPARQL_PLANT_UPDATE_EMISSION, 200.05, planturi);
		String result = new QueryBroker().updateFile(plantfile, query);
		System.out.println("result = " + result);
		throw new RuntimeException("test not yet implemented");
	}
	
	public void testSparqlUpdate() {
		
		String plantfile = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_Update.owl";
		String planturi = "http://www.theworldavatar.com/kb/powerplants/Northwest_Kabul_Power_Plant_Afghanistan.owl";	
		OntModel model = JenaHelper.createModel(plantfile);	
		Dataset dataset = DatasetFactory.wrap(model);
		
		
		
		String query = String.format(SPARQL_PLANT_UPDATE_EMISSION, 200.05, planturi);
		
		query = SPARQL_PLANT_TEST;
		
		UpdateRequest request = UpdateFactory.create(query);
		
		UpdateProcessor processor = UpdateExecutionFactory.create(request, dataset);
		processor.execute();
		
		//dataset.commit();
		
		Model updatedModel = dataset.getDefaultModel();
		
		
		String output = AgentLocator.getCurrentJpsAppDirectory(this)+"/testres/Northwest_out.owl";
		JenaHelper.writeAsFile(updatedModel, output);
		
		
//		boolean done = false;
//		try {
//			UpdateExecutionFactory.createRemoteForm(
//					UpdateFactory.create(query), 
//					sparqlEndpointURI, 
//					context(),
//					auth(sparqlEndpointURI, user, password))
//				.execute();
//			done = true;
//		} catch (Exception exception) {
//			LOGGER.error(MessageCatalog._00035_SPARQL_FAILED, exception, query);
//		}
//		return done;
	
	}
}
