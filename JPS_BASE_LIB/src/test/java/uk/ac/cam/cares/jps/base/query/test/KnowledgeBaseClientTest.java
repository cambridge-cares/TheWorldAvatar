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

	String queryEndpoint = anyString();
	String updateEndpoint = anyString();

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
	
	/**
	 * Verifies if the KnowledgeBaseClient constructor that is designed to<p>
	 * set both the query and update endpoints (URLs) assigns the values to<p>
	 * the corresponding member variables. 
	 * 
	 * @throws SQLException
	 */
	@Test
	public void queryAndUpdateEndpointsSetupTest() throws SQLException{
		KnowledgeBaseClient kbClient = new KnowledgeBaseClient(queryEndpoint, updateEndpoint);
		assertNotNull(kbClient.getQueryEndpoint());
		assertNotNull(kbClient.getUpdateEndpoint());
		assertEquals(updateEndpoint, kbClient.getQueryEndpoint());
		assertEquals(queryEndpoint, kbClient.getUpdateEndpoint());
	}
	
	/**
	 * Checks if the KnowledgeBaseClient constructor that is designed to<p>
	 * set the query and update endpoints (URLs) and query assigns the values<p>
	 * to the corresponding member variables. 
	 * 
	 * @throws SQLException
	 */
	@Test
	public void endpointsAndQuerySetupTest() throws SQLException{
		KnowledgeBaseClient kbClient = new KnowledgeBaseClient(queryEndpoint, updateEndpoint, formMechanismCountCountQuery());
		assertNotNull(kbClient.getQueryEndpoint());
		assertNotNull(kbClient.getUpdateEndpoint());
		assertNotNull(kbClient.getQuery());
		assertEquals(queryEndpoint, kbClient.getQueryEndpoint());
		assertEquals(updateEndpoint, kbClient.getUpdateEndpoint());
		assertEquals(formMechanismCountCountQuery(), kbClient.getQuery());
		queryEndpoint = "/test/Query/Endpoint";
		updateEndpoint = "/test/Update/Endpoint";
		kbClient = new KnowledgeBaseClient(queryEndpoint, updateEndpoint, formMechanismIRIsQuery());
		assertNotNull(kbClient.getQueryEndpoint());
		assertNotNull(kbClient.getUpdateEndpoint());
		assertNotNull(kbClient.getQuery());
		assertEquals(queryEndpoint, kbClient.getQueryEndpoint());
		assertEquals(updateEndpoint, kbClient.getUpdateEndpoint());
		assertEquals(formMechanismIRIsQuery(), kbClient.getQuery());
		queryEndpoint = "/extended/Test/QueryEndpoint";
		updateEndpoint = "/extended/Test/UpdateEndpoint";
		kbClient = new KnowledgeBaseClient(queryEndpoint, updateEndpoint, formInsertQuery());
		assertNotNull(kbClient.getQueryEndpoint());
		assertNotNull(kbClient.getUpdateEndpoint());
		assertNotNull(kbClient.getQuery());
		assertEquals(queryEndpoint, kbClient.getQueryEndpoint());
		assertEquals(updateEndpoint, kbClient.getUpdateEndpoint());
		assertEquals(formInsertQuery(), kbClient.getQuery());
	}
	
	/**
	 * Checks if the connection URL established for the query endpoint (URL)<p>
	 * is the expected one. 
	 * 
	 * @throws SQLException
	 */
	@Test
	public void connectionURLForQueryEndpointTest() throws SQLException{
		queryEndpoint = "http://localhost:8080/test";
		KnowledgeBaseClient kbClient = new KnowledgeBaseClient(queryEndpoint);
		assertNotNull(kbClient.getConnectionUrl());
		System.out.println("kbClient.getConnectionUrl():"+kbClient.getConnectionUrl());
		assertEquals("jdbc:jena:remote:query=".concat(queryEndpoint), kbClient.getConnectionUrl());
	}
	
	/**
	 * Checks if the connection URL established for the query and update<p> 
	 * endpoints (URLs) is the expected one. 
	 * 
	 * @throws SQLException
	 */
	@Test
	public void connectionURLForQueryAndUpdateEndpointsTest() throws SQLException{
		queryEndpoint = "http://localhost:8080/test";
		updateEndpoint = "http://localhost:8080/test";
		KnowledgeBaseClient kbClient = new KnowledgeBaseClient(queryEndpoint, updateEndpoint);
		assertNotNull(kbClient.getConnectionUrl());
		System.out.println("kbClient.getConnectionUrl():"+kbClient.getConnectionUrl());
		assertEquals("jdbc:jena:remote:query=".concat(queryEndpoint).concat("&update=").concat(updateEndpoint), kbClient.getConnectionUrl());
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
