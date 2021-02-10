package uk.ac.cam.cares.jps.base.query.test;

import static org.junit.Assert.*;

import java.sql.SQLException;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;

import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.RemoteKnowledgeBaseClient;

/**
 * This class covers both unit tests and regression tests on KnowledgeBaseClient,<p>
 * which is designed to perform query and update operations on virtually any<p>
 * SPARQL Endpoints.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class RemoteKnowledgeBaseClientTest {

	String queryEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
	String updateEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
	String userName = "user";
	String password = "password";
	
	/**
	 * Verifies if the KnowledgeBaseClient constructor that is designed to<p>
	 * set the query endpoint (URL) assigns the value to the corresponding<p>
	 * member variable. 
	 * 
	 * @throws SQLException
	 */
	@Test
	public void queryEndpointSetupTest() throws SQLException{
		RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient(queryEndpoint);
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
		RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint);
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
		userName = "user";
		password = "password";
		RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint, formMechanismCountQuery(), userName, password);
		assertNotNull(kbClient.getQueryEndpoint());
		assertNotNull(kbClient.getUpdateEndpoint());
		assertNotNull(kbClient.getQuery());
		assertEquals(queryEndpoint, kbClient.getQueryEndpoint());
		assertEquals(updateEndpoint, kbClient.getUpdateEndpoint());
		assertEquals(formMechanismCountQuery(), kbClient.getQuery());
		queryEndpoint = "/test/Query/Endpoint";
		updateEndpoint = "/test/Update/Endpoint";
		kbClient = new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint, formMechanismIRIsQuery(), userName, password);
		assertNotNull(kbClient.getQueryEndpoint());
		assertNotNull(kbClient.getUpdateEndpoint());
		assertNotNull(kbClient.getQuery());
		assertEquals(queryEndpoint, kbClient.getQueryEndpoint());
		assertEquals(updateEndpoint, kbClient.getUpdateEndpoint());
		assertEquals(formMechanismIRIsQuery(), kbClient.getQuery());
		queryEndpoint = "/extended/Test/QueryEndpoint";
		updateEndpoint = "/extended/Test/UpdateEndpoint";
		kbClient = new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint, formInsertQuery(), userName, password);
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
		RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient(queryEndpoint);
		System.out.println(
				"\nTesting if the URL to connect to an endpoint for performing a query operation is set as expected.");
		assertNotNull(kbClient.getConnectionUrl());
		assertEquals("jdbc:jena:remote:query=".concat(queryEndpoint), kbClient.getConnectionUrl());
		System.out.println("Expected connection URL      :"
				+ "jdbc:jena:remote:query=".concat(queryEndpoint)
				+ "\n matched with the actual one :" + kbClient.getConnectionUrl());
	}
	
	/**
	 * Checks if the connection URL established for the query and update<p> 
	 * endpoints (URLs) is the expected one. 
	 * 
	 * @throws SQLException
	 */
	@Test
	public void connectionURLForQueryAndInsertEndpointsTest() throws SQLException{
		queryEndpoint = "http://localhost:8080/test";
		updateEndpoint = "http://localhost:8080/test";
		RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint);
		assertNotNull(kbClient.getConnectionUrl());
		System.out.println(
				"\nTesting if the URL to connect to an endpoint for performing an insert operation is set as expected.");
		assertEquals("jdbc:jena:remote:query=".concat(queryEndpoint).concat("&update=").concat(updateEndpoint), kbClient.getConnectionUrl());
		System.out.println("Expected connection URL      :"
				+ "jdbc:jena:remote:query=".concat(queryEndpoint).concat("&update=").concat(updateEndpoint)
				+ "\n matched with the actual one :" + kbClient.getConnectionUrl());
	}
	
	/**
	 * Checks if the connection URL established for the query endpoint and<p> 
	 * update endpoint for deletion is the expected one. 
	 * 
	 * @throws SQLException
	 */
	@Test
	public void connectionURLForQueryAndDeleteEndpointsTest() throws SQLException {
		queryEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		updateEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		RemoteKnowledgeBaseClient kbClient = new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint);
		assertNotNull(kbClient.getConnectionUrl());
		System.out.println(
				"\nTesting if the URL to connect to an endpoint for performing a delete query is set as expected.");
		assertEquals("jdbc:jena:remote:query=".concat(queryEndpoint).concat("&update=").concat(updateEndpoint),
				kbClient.getConnectionUrl());
		System.out.println("Expected connection URL      :"
				+ "jdbc:jena:remote:query=".concat(queryEndpoint).concat("&update=").concat(updateEndpoint)
				+ "\n matched with the actual one :" + kbClient.getConnectionUrl());
	}
	
	/**
	 * Verifies the validity of both the query URL and update URL.<br>
	 * For example, standard protocols for URL, i.e. http and https are supported.  
	 * 
	 * @throws SQLException
	 */
	@Test
	public void connectionHttpURLTest() throws SQLException{
		// Tests the query endpoint
		queryEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		RemoteKnowledgeBaseClient rKBClient = new RemoteKnowledgeBaseClient(queryEndpoint);
		assertTrue(rKBClient.isConnectionQueryUrlValid(rKBClient.getConnectionUrl()));
		queryEndpoint = "https://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteKnowledgeBaseClient(queryEndpoint);
		assertTrue(rKBClient.isConnectionQueryUrlValid(rKBClient.getConnectionUrl()));
		queryEndpoint = "httpss://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteKnowledgeBaseClient(queryEndpoint);
		assertFalse(rKBClient.isConnectionQueryUrlValid(rKBClient.getConnectionUrl()));
		// Tests the update endpoint with the update URL only 
		updateEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteKnowledgeBaseClient();
		rKBClient.setUpdateEndpoint(updateEndpoint);
		assertTrue(rKBClient.isConnectionUpdateUrlValid(rKBClient.getConnectionUrl()));
		updateEndpoint = "https://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteKnowledgeBaseClient();
		rKBClient.setUpdateEndpoint(updateEndpoint);
		assertTrue(rKBClient.isConnectionUpdateUrlValid(rKBClient.getConnectionUrl()));
		updateEndpoint = "httpss://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteKnowledgeBaseClient();
		rKBClient.setUpdateEndpoint(updateEndpoint);
		assertFalse(rKBClient.isConnectionUpdateUrlValid(rKBClient.getConnectionUrl()));
		// Tests the update endpoint with both the query URL and update URL
		queryEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		updateEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint);
		assertTrue(rKBClient.isConnectionUpdateUrlValid(rKBClient.getConnectionUrl()));
		queryEndpoint = "https://localhost:8080/blazegraph/namespace/ontokin/sparql";
		updateEndpoint = "https://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint);
		assertTrue(rKBClient.isConnectionUpdateUrlValid(rKBClient.getConnectionUrl()));
		queryEndpoint = "httpss://localhost:8080/blazegraph/namespace/ontokin/sparql";
		updateEndpoint = "httpss://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteKnowledgeBaseClient(queryEndpoint, updateEndpoint);
		assertFalse(rKBClient.isConnectionUpdateUrlValid(rKBClient.getConnectionUrl()));
	}
	
	/**
	 * Verifies the validity of connection URL consisting of a query URL,<br>
	 * user name and password. 
	 * 
	 * @throws SQLException
	 */
	@Test
	public void connectionHttpUrlWithAuthTest() throws SQLException{ 
		userName = "user";
		password = "password";
		queryEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		RemoteKnowledgeBaseClient remoteKBClient = new RemoteKnowledgeBaseClient();
		remoteKBClient.setQueryEndpoint(queryEndpoint);
		remoteKBClient.setUser(userName);
		remoteKBClient.setPassword(password);
		assertEquals("jdbc:jena:remote:query=".concat(queryEndpoint).concat("&user=").concat(userName).concat("&password=").concat(password),
				remoteKBClient.getConnectionUrl());
	}
	
	/**
	 * Tests if the HTTP request to run a SPARQL query returns the expected<p>
	 * result. It also verifies if the mock service created for this test<p>
	 * executes the correct method.
	 * 
	 * @throws SQLException
	 */
	@Test
	public void performMechanismCountQueryTest() throws SQLException{
		RemoteKnowledgeBaseClient kbClient = mock(RemoteKnowledgeBaseClient.class);
		JSONArray jsonArray = new JSONArray();
		JSONObject jsonObject = new JSONObject();
		jsonObject.put("count", "1");
		jsonArray.put(jsonObject);
		kbClient.setQueryEndpoint(queryEndpoint);
		when(kbClient.execute(formMechanismCountQuery())).thenReturn(jsonArray.toString());
		String result = kbClient.execute(formMechanismCountQuery());
		System.out.println("Expected query result      :" + jsonArray.toString()
				+ "\n matched with the actual one :" + result);
		assertEquals(jsonArray.toString(), result);
		verify(kbClient).execute(formMechanismCountQuery());
	}

	/**
	 * A SPARQL query to count the total number of mechanisms in a repository.
	 * 
	 * @return
	 */
	private static String formMechanismCountQuery(){
		String query = "PREFIX ontokin: <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#>\n";
			query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
			query = query.concat("SELECT ?x \n");
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
			query = query.concat("SELECT ?x \n");
			query = query.concat("WHERE\n");
			query = query.concat("{\n");
			query = query.concat("?x rdf:type ontokin:ReactionMechanism .\n");
			query = query.concat("} LIMIT 10\n");
			return query;
	}

	
	/**
	 * A SPARQL query to retrieve the IRIs of all mechanisms in a repository.
	 * 
	 * @return
	 */
	private static String formAnyTriplesQuery(){
		String query = "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>\n";
			query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
			query = query.concat("SELECT ?x ?y ?z \n");
			query = query.concat("WHERE\n");
			query = query.concat("{\n");
			query = query.concat("?x ?y ?z .\n");
			query = query.concat("} LIMIT 10\n");
			return query;
	}
	
	private static String formInsertQuery(){
		String query = "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>\n";
			query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
			query = query.concat("INSERT DATA { <http://www.theworldavatar.com/kb/ontokin/POLIMI_H2CO_1412.owl#ArrheniusCoefficient_182161099217501> ontokin:hasTemperatureExponent \"-0.7\" }");
			return query;
	}
	
	private static String formTempExponentQuery(){
		String query = "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>\n";
			query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
			query = query.concat("SELECT ?tempExponent\n");
			query = query.concat("WHERE\n");
			query = query.concat("{\n");
			query = query.concat("<http://www.theworldavatar.com/kb/ontokin/POLIMI_H2CO_1412.owl#ArrheniusCoefficient_182161099217501> ontokin:hasTemperatureExponent ?tempExponent .\n");
			query = query.concat("}");
			return query;
	}
	
	private static String formDeleteQuery(){
		String query = "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>\n";
			query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
			query = query.concat("DELETE DATA { <http://www.theworldavatar.com/kb/ontokin/POLIMI_H2CO_1412.owl#ArrheniusCoefficient_182161099217501> ontokin:hasTemperatureExponent \"-0.7\" }");
			return query;
	}
}
