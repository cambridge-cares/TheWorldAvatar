package uk.ac.cam.cares.jps.base.query.test;

import static org.junit.Assert.*;

import java.sql.SQLException;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.ArgumentMatchers.anyString;


import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;

/**
 * This class covers both unit tests and regression tests on KnowledgeBaseClient,<p>
 * which is designed to perform query and update operations on virtually any<p>
 * SPARQL Endpoints.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class KnowledgeBaseClientTest {

	String queryEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
	String updateEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";

	/**
	 * Verifies if the KnowledgeBaseClient constructor that is designed to<p>
	 * set the query endpoint (URL) assigns the value to the corresponding<p>
	 * member variable. 
	 * 
	 * @throws SQLException
	 */
	@Test
	public void queryEndpointSetupTest() throws SQLException{
		KnowledgeBaseClient kbClient = new KnowledgeBaseClient(queryEndpoint);
		assertNotNull(kbClient.getQueryEndpoint());
		assertEquals(queryEndpoint, kbClient.getQueryEndpoint());
	}
	
//	@Test
//	public void performMechanismCountQueryTest() throws SQLException{
//		KnowledgeBaseClient kbClient = mock(KnowledgeBaseClient.class);
//		JSONArray jsonArray = new JSONArray();
//		JSONObject jsonObject = new JSONObject();
//		jsonObject.put("count", "1");
//		jsonArray.put(jsonObject);
//		kbClient.setQueryEndpoint(anyString());
//		when(kbClient.setQuery(formMechanismCountCountQuery())).thenReturn(jsonArray.toString());
//		kbClient.setQueryEndpoint(queryEndpoint);
//		kbClient.setQuery(formMechanismCountCountQuery());
//	}
//
//	@Test
//	public void performQueryTest2() throws SQLException{
//		KnowledgeBaseClient kbClient = new KnowledgeBaseClient();
//		kbClient.setQueryEndpoint(queryEndpoint);
//		kbClient.setQuery(formMechanismIRIsQuery());
//		JSONArray result = kbClient.executeQuery();
//		System.out.println(result.toString());
//		assertNotNull(result.toString());
//	}
//	
//	@Test
//	public void updateQuery() throws SQLException{
//		KnowledgeBaseClient kbClient = new KnowledgeBaseClient();
//		kbClient.setQueryEndpoint(queryEndpoint);
//		kbClient.setUpdateEndpoint(updateEndpoint);
//		kbClient.setQuery(formInsertQuery());
//		System.out.println("kbClient.executeUpdate():"+kbClient.executeUpdate());
//		
//	}

	/**
	 * A SPARQL query to count the total number of mechanisms in a repository.
	 * 
	 * @return
	 */
	private static String formMechanismCountCountQuery(){
		String query = "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>\n";
			query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
			query = query.concat("SELECT (COUNT(?x) AS ?count)\n");
			query = query.concat("WHERE\n");
			query = query.concat("{\n");
			query = query.concat("?x rdf:type ontokin:ReactionMechanism .\n");
			query = query.concat("}\n");
			return query;
	}
	
	/**
	 * A SPARQL query to retrieve the IRIs of all mechanisms in a repository.
	 * 
	 * @return
	 */
	private static String formMechanismIRIsQuery(){
		String query = "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>\n";
			query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
			query = query.concat("SELECT ?x ?y ?z \n");
			query = query.concat("WHERE\n");
			query = query.concat("{\n");
			query = query.concat("?x ?y ?z .\n");
			query = query.concat("} LIMIT 10\n");
			return query;
	}
	
	private static String formInsertQuery(){
		String query = "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>\n";
			query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
			query = query.concat("INSERT DATA { <http://www.theworldavatar.com/kb/ontokin/POLIMI_H2CO_1412.owl#ArrheniusCoefficient_182161099217501> ontokin:hasTemperatureExponent \"-0.6\" }");
			return query;
	}
	
	private static String formTempExponentQuery(){
		String query = "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>\n";
			query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
			query = query.concat("SELECT ?tempExponent\n");
			query = query.concat("WHERE\n");
			query = query.concat("{\n");
			query = query.concat("<http://www.theworldavatar.com/kb/ontokin/POLIMI_H2CO_1412.owl#ArrheniusCoefficient_182161099217501> ontokin:hasTemperatureExponent ?tempExponent .\n");
			query = query.concat("}");
			return query;
	}
	
	private static String formDeleteQuery(){
		String query = "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>\n";
			query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
			query = query.concat("INSERT DATA { <http://www.theworldavatar.com/kb/ontokin/POLIMI_H2CO_1412.owl#ArrheniusCoefficient_182161099217501> ontokin:hasTemperatureExponent \"-0.6\" }");
			return query;
	}
}
