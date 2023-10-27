package uk.ac.cam.cares.jps.base.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.*;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONException;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.query.LocalStoreClient;

class LocalStoreClientTest {

	private static String testData = 
			"INSERT DATA {<http://www.example.com/test/s1> <http://www.example.com/test/p1> <http://www.example.com/test/o1> .\n"
			+ "<http://www.example.com/test/s2> <http://www.example.com/test/p2> <http://www.example.com/test/o2> .\n"
			+ "<http://www.example.com/test/s3> <http://www.example.com/test/p3> <http://www.example.com/test/o3> }";
	
	private static String testQuery1 = "SELECT ?o WHERE {<http://www.example.com/test/s1> <http://www.example.com/test/p1> ?o.}";
	private static String expected1 = "[{\"o\":\"http://www.example.com/test/o1\"}]";
	private static String testQuery2 = "SELECT ?o WHERE {<http://www.example.com/test/s2> <http://www.example.com/test/p2> ?o.}";
	private static String expected2 = "[{\"o\":\"http://www.example.com/test/o2\"}]";
	private static String testQuery3 = "SELECT ?o WHERE {<http://www.example.com/test/s3> <http://www.example.com/test/p3> ?o.}";
	private static String expected3 = "[{\"o\":\"http://www.example.com/test/o3\"}]";
	
	private static UpdateRequest getUpdateRequest() throws ParseException {
		
		WhereBuilder where = new WhereBuilder()
				.addBind("<http://www.example.com/test/s1>", "?s")
				.addWhere("?s", "?p", "?o");
				
		UpdateBuilder builder = new UpdateBuilder()
				.addDelete("?s", "?p", "?o")
				.addWhere(where);
		
		return builder.buildRequest();
	}
	
	private static Query getConstruct() {
		Var varS = Var.alloc("s");
		Var varP = Var.alloc("p");
		Var varO = Var.alloc("o");
		
		ConstructBuilder builder = new ConstructBuilder()
				.addConstruct( varS, varP, varO)
				.addWhere(varS, varP, varO);
		return builder.build();
	} 
	
	/////////////////////////////////
	// Test constructor
	////////////////////////////////
	
	/**
	 * Test constructor with file path provided.
	 */
	@Test
	public void testConstructor() {
		
		LocalStoreClient storeClient = new LocalStoreClient();
		assertNull(storeClient.getQuery());
		assertTrue(storeClient.isConnected());
		assertTrue(storeClient.isEmpty());
	}
	
	@Test
	public void testConstructor2() {
		
		String query = "Test query string.";
		
		LocalStoreClient storeClient = new LocalStoreClient(query);
		assertEquals(query, storeClient.getQuery());
		assertTrue(storeClient.isConnected());
		assertTrue(storeClient.isEmpty());
	}
	
	////////////////////////////////
	// Setter and Getter methods
	////////////////////////////////
	
	@Test
	public void testSetGetQuery(){
		
		String query = "Test query string.";
		
		LocalStoreClient storeClient = new LocalStoreClient();
		
		assertNull(storeClient.getQuery());
		storeClient.setQuery(query);
		assertEquals(query, storeClient.getQuery());
	}
	
	@Test 
	public void testSetGetQueryEndpoint() {
		
		String endpoint = "http://www.example.com/test/query/endpoint";
	
		LocalStoreClient storeClient = new LocalStoreClient();
		
		assertNull(storeClient.getQueryEndpoint());
		storeClient.setQueryEndpoint(endpoint);
		assertNull(storeClient.getQueryEndpoint()); //null expected, no endpoint
	}
	
	@Test 
	public void testSetGetUpdateEndpoint() {
		
		String endpoint = "http://www.example.com/test/update/endpoint";
	
		LocalStoreClient storeClient = new LocalStoreClient();
		
		assertNull(storeClient.getUpdateEndpoint());
		storeClient.setUpdateEndpoint(endpoint);
		assertNull(storeClient.getUpdateEndpoint()); //null expected, no endpoint
	}
	
	@Test 
	public void testSetGetUser() {
		
		String user = "Test user.";
	
		LocalStoreClient storeClient = new LocalStoreClient();
		
		assertNull(storeClient.getUser());
		storeClient.setUser(user);
		assertNull(storeClient.getUser()); //null expected, no user
	}
	
	@Test 
	public void testSetGetPassword() {
		
		String password = "Test password.";
	
		LocalStoreClient storeClient = new LocalStoreClient();
		
		assertNull(storeClient.getPassword());
		storeClient.setUser(password);
		assertNull(storeClient.getPassword()); //null expected, no password
	}
	
	///////////////////////////////
	// Test update methods
	///////////////////////////////
	
	/**
	 * Test Sparql update from query variable
	 */
	@Test
	public void testExecuteUpdate() throws ParseException {

		// Test by executing update from query variable to load data
		LocalStoreClient storeClient = new LocalStoreClient(testData);
		storeClient.executeUpdate();
	
		// Query for contents
		String result = storeClient.execute(testQuery1);
		assertEquals(expected1, result);
		result = storeClient.execute(testQuery2);
		assertEquals(expected2, result);
		result = storeClient.execute(testQuery3);
		assertEquals(expected3, result);
	}
	
	/**
	 * Test Sparql Update with argument as string 
	 */
	@Test
	public void testExecuteUpdateWithStringArgument() throws ParseException {

		// Test by executing update from argument to load data
		LocalStoreClient storeClient = new LocalStoreClient();
		storeClient.executeUpdate(testData);

		// Query for contents
		String result = storeClient.execute(testQuery1);
		assertEquals(expected1, result);
		result = storeClient.execute(testQuery2);
		assertEquals(expected2, result);
		result = storeClient.execute(testQuery3);
		assertEquals(expected3, result);
	}

	/**
	 * Test Sparql Update with argument as UpdateRequest
	 */
	@Test
	public void testExecuteUpdateWithUpdateRequestArgument() throws ParseException {
		
		// Instantiate and load data
		LocalStoreClient storeClient = new LocalStoreClient();
		storeClient.executeUpdate(testData);
		
		// Query for contents
		String result = storeClient.execute(testQuery1);
		assertEquals(expected1, result);
		
		// Test execute with UpdateRequest argument to delete triple
		storeClient.executeUpdate(getUpdateRequest());
		result = storeClient.execute(testQuery1);
		assertEquals("[]", result);
	}
	
	///////////////////////////////
	// Test query methods
	///////////////////////////////
	
	/**
	 * Test Sparql query. Should return result as String.
	 */
	@Test
	public void testExecute() {
		
		// Instantiate with query variable and load test data using update
		LocalStoreClient storeClient = new LocalStoreClient(testQuery1);
		storeClient.executeUpdate(testData);
		
		// Test execute
		String result = storeClient.execute();	
		assertEquals(expected1, result);
	}
	
	/**
	 * Test Sparql query with String. Should return result as String.
	 */
	@Test
	public void testExecuteWithArgument() {
		
		// Instantiate and load test data using update 
		LocalStoreClient storeClient = new LocalStoreClient(testData);
		storeClient.executeUpdate();
		
		// Test execute with query as argument
		String result = storeClient.execute(testQuery1);
		assertEquals(expected1, result);
	}
	
	/**
	 * Test Sparql query. Should return result as JSONArray.
	 * 
	 * @throws JSONException
	 */
	@Test
	public void testExecuteQuery() throws JSONException {

		// Instantiate with query variable and load test data using update
		LocalStoreClient storeClient = new LocalStoreClient(testQuery1);
		storeClient.executeUpdate(testData);
		
		JSONArray result = storeClient.executeQuery();		
		String strResult = result.getJSONObject(0).get("o").toString(); 
		
		String expected = new JSONArray(expected1).getJSONObject(0).getString("o");
		assertEquals(expected, strResult);
	}
	
	/**
	 * Test Sparql query with String. Should return result as JSONArray.
	 * 
	 * @throws JSONException
	 */
	@Test
	public void testExecuteQueryWithArgument () {
		
		LocalStoreClient storeClient = new LocalStoreClient(testData);
		storeClient.executeUpdate(testData);
		
		JSONArray result = storeClient.executeQuery(testQuery1);		
		String strResult = result.getJSONObject(0).get("o").toString();   
		
		String expected = new JSONArray(expected1).getJSONObject(0).getString("o");
		assertEquals(expected, strResult);
	}	
	
	///////////////////////////////
	// Test Get, Insert, Construct
	///////////////////////////////
	
	
	/**
	 * Test construct
	 */
	@Test
	public void testConstruct() {
		
		LocalStoreClient storeClient = new LocalStoreClient(testData);
		storeClient.executeUpdate();
		
		Model model1 = storeClient.executeConstruct(getConstruct());
		Model model2 = storeClient.executeConstruct(getConstruct().toString());
		
		assertNotNull(model1.isEmpty());
		assertNotNull(model2.isEmpty());
		assertTrue(model1.containsAll(model2)); 
		
		//Insert model into new LocalStoreClient
		UpdateBuilder builder = new UpdateBuilder()
				.addInsert(model1);
		
		LocalStoreClient storeClient2 = new LocalStoreClient();
		storeClient2.executeUpdate(builder.buildRequest());
		
		// Query for contents
		String result = storeClient2.execute(testQuery1);
		assertEquals(expected1, result);
		result = storeClient2.execute(testQuery2);
		assertEquals(expected2, result);
		result = storeClient2.execute(testQuery3);
		assertEquals(expected3, result);
	}
	
	/**
	 * Test insert and get methods
	 */
	@Test
	public void testInsertAndGet() {
		
		String content = 
		"<rdf:RDF"+System.getProperty("line.separator")+
		"    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\""+System.getProperty("line.separator")+
		"    xmlns:j.0=\"http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#\">"+System.getProperty("line.separator")+
		"  <j.0:FoodCourt rdf:about=\"http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/FoodCourt-001.owl#FoodCourt-001\"/>"+System.getProperty("line.separator")+
		"</rdf:RDF>"+System.getProperty("line.separator");
		
		LocalStoreClient storeClient = new LocalStoreClient();
		storeClient.insert(null, content, null);
		
		//check insert was successful
		String result = storeClient.execute("SELECT ?o WHERE {?s ?p ?o}");
		assertEquals("[{\"o\":\"http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#FoodCourt\"}]",result);
		
		//test get
		
		//rdfxml
		result = storeClient.get(null, null);
		assertEquals(content, result);
		
		result = storeClient.get(null, "application/rdf+xml");
		assertEquals(content, result);		
		
		//n-triples
		String contentNT = 
		"<http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/FoodCourt-001.owl#FoodCourt-001> "+
		"<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#FoodCourt> .\n";
		
		result = storeClient.get(null, "application/n-triples");
		assertEquals(contentNT, result);
	}
	
}
