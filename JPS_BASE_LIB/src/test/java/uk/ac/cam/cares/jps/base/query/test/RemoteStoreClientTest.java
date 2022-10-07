package uk.ac.cam.cares.jps.base.query.test;

import static org.junit.Assert.*;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Test;
import org.mockito.Mockito;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.ArgumentMatchers.any;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This class covers both unit tests and regression tests on RemoteStoreClient,<p>
 * which is designed to perform query and update operations on virtually any<p>
 * SPARQL Endpoints.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class RemoteStoreClientTest {

	String queryEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
	String updateEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
	String userName = "user";
	String password = "password";
	
	/**
	 * Verifies if the StoreClient constructor that is designed to<p>
	 * set the query endpoint (URL) assigns the value to the corresponding<p>
	 * member variable. 
	 * 
	 * @throws SQLException
	 */
	@Test
	public void queryEndpointSetupTest() throws SQLException{
		RemoteStoreClient kbClient = new RemoteStoreClient(queryEndpoint);
		assertNotNull(kbClient.getQueryEndpoint());
		assertEquals(queryEndpoint, kbClient.getQueryEndpoint());
	}
	
	/**
	 * Verifies if the StoreClient constructor that is designed to<p>
	 * set both the query and update endpoints (URLs) assigns the values to<p>
	 * the corresponding member variables. 
	 * 
	 * @throws SQLException
	 */
	@Test
	public void queryAndUpdateEndpointsSetupTest() throws SQLException{
		RemoteStoreClient kbClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);
		assertNotNull(kbClient.getQueryEndpoint());
		assertNotNull(kbClient.getUpdateEndpoint());
		assertEquals(updateEndpoint, kbClient.getQueryEndpoint());
		assertEquals(queryEndpoint, kbClient.getUpdateEndpoint());
	}
	
	/**
	 * Checks if the StoreClient constructor that is designed to<p>
	 * set the query and update endpoints (URLs) and query assigns the values<p>
	 * to the corresponding member variables. 
	 * 
	 * @throws SQLException
	 */
	@Test
	public void endpointsAndQuerySetupTest() throws SQLException{
		userName = "user";
		password = "password";
		RemoteStoreClient kbClient = new RemoteStoreClient(queryEndpoint, updateEndpoint, formMechanismCountQuery(), userName, password);
		assertNotNull(kbClient.getQueryEndpoint());
		assertNotNull(kbClient.getUpdateEndpoint());
		assertNotNull(kbClient.getQuery());
		assertEquals(queryEndpoint, kbClient.getQueryEndpoint());
		assertEquals(updateEndpoint, kbClient.getUpdateEndpoint());
		assertEquals(formMechanismCountQuery(), kbClient.getQuery());
		queryEndpoint = "/test/Query/Endpoint";
		updateEndpoint = "/test/Update/Endpoint";
		kbClient = new RemoteStoreClient(queryEndpoint, updateEndpoint, formMechanismIRIsQuery(), userName, password);
		assertNotNull(kbClient.getQueryEndpoint());
		assertNotNull(kbClient.getUpdateEndpoint());
		assertNotNull(kbClient.getQuery());
		assertEquals(queryEndpoint, kbClient.getQueryEndpoint());
		assertEquals(updateEndpoint, kbClient.getUpdateEndpoint());
		assertEquals(formMechanismIRIsQuery(), kbClient.getQuery());
		queryEndpoint = "/extended/Test/QueryEndpoint";
		updateEndpoint = "/extended/Test/UpdateEndpoint";
		kbClient = new RemoteStoreClient(queryEndpoint, updateEndpoint, formInsertQuery(), userName, password);
		assertNotNull(kbClient.getQueryEndpoint());
		assertNotNull(kbClient.getUpdateEndpoint());
		assertNotNull(kbClient.getQuery());
		assertEquals(queryEndpoint, kbClient.getQueryEndpoint());
		assertEquals(updateEndpoint, kbClient.getUpdateEndpoint());
		assertEquals(formInsertQuery(), kbClient.getQuery());
	}
	
	@Test
	public void testIsUpdateEndpointBlazegraphBackended() {
		queryEndpoint = "/test/Query/Endpoint";
		RemoteStoreClient kbClient = new RemoteStoreClient(queryEndpoint);
		// should be false as no update endpoint is provided
		assertTrue(!kbClient.isUpdateEndpointBlazegraphBackended());

		updateEndpoint = UUID.randomUUID().toString() + "/blazegraph/namespace/" + UUID.randomUUID().toString()
				+ "/sparql";
		kbClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);
		assertTrue(kbClient.isUpdateEndpointBlazegraphBackended());

		updateEndpoint = UUID.randomUUID().toString() + "//blazegraph//namespace//" + UUID.randomUUID().toString()
				+ "/sparql";
		kbClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);
		// Paths should be able to resolve "//" to "/"
		assertTrue(kbClient.isUpdateEndpointBlazegraphBackended());

		updateEndpoint = UUID.randomUUID().toString();
		kbClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);
		assertTrue(!kbClient.isUpdateEndpointBlazegraphBackended());
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
		RemoteStoreClient kbClient = new RemoteStoreClient(queryEndpoint);
		assertNotNull(kbClient.getConnectionUrl());
		assertEquals("jdbc:jena:remote:query=".concat(queryEndpoint), kbClient.getConnectionUrl());
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
		RemoteStoreClient kbClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);
		assertNotNull(kbClient.getConnectionUrl());
		assertEquals("jdbc:jena:remote:query=".concat(queryEndpoint).concat("&update=").concat(updateEndpoint), kbClient.getConnectionUrl());
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
		RemoteStoreClient kbClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);
		assertNotNull(kbClient.getConnectionUrl());
		assertEquals("jdbc:jena:remote:query=".concat(queryEndpoint).concat("&update=").concat(updateEndpoint),
				kbClient.getConnectionUrl());
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
		RemoteStoreClient rKBClient = new RemoteStoreClient(queryEndpoint);
		assertTrue(rKBClient.isConnectionQueryUrlValid(rKBClient.getConnectionUrl()));
		queryEndpoint = "https://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteStoreClient(queryEndpoint);
		assertTrue(rKBClient.isConnectionQueryUrlValid(rKBClient.getConnectionUrl()));
		queryEndpoint = "httpss://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteStoreClient(queryEndpoint);
		assertFalse(rKBClient.isConnectionQueryUrlValid(rKBClient.getConnectionUrl()));
		// Tests the update endpoint with the update URL only 
		updateEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteStoreClient();
		rKBClient.setUpdateEndpoint(updateEndpoint);
		assertTrue(rKBClient.isConnectionUpdateUrlValid(rKBClient.getConnectionUrl()));
		updateEndpoint = "https://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteStoreClient();
		rKBClient.setUpdateEndpoint(updateEndpoint);
		assertTrue(rKBClient.isConnectionUpdateUrlValid(rKBClient.getConnectionUrl()));
		updateEndpoint = "httpss://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteStoreClient();
		rKBClient.setUpdateEndpoint(updateEndpoint);
		assertFalse(rKBClient.isConnectionUpdateUrlValid(rKBClient.getConnectionUrl()));
		// Tests the update endpoint with both the query URL and update URL
		queryEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		updateEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);
		assertTrue(rKBClient.isConnectionUpdateUrlValid(rKBClient.getConnectionUrl()));
		queryEndpoint = "https://localhost:8080/blazegraph/namespace/ontokin/sparql";
		updateEndpoint = "https://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);
		assertTrue(rKBClient.isConnectionUpdateUrlValid(rKBClient.getConnectionUrl()));
		queryEndpoint = "httpss://localhost:8080/blazegraph/namespace/ontokin/sparql";
		updateEndpoint = "httpss://localhost:8080/blazegraph/namespace/ontokin/sparql";
		rKBClient = new RemoteStoreClient(queryEndpoint, updateEndpoint);
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
		RemoteStoreClient remoteKBClient = new RemoteStoreClient();
		remoteKBClient.setQueryEndpoint(queryEndpoint);
		remoteKBClient.setUser(userName);
		remoteKBClient.setPassword(password);
		assertEquals("jdbc:jena:remote:query=".concat(queryEndpoint).concat("&user=").concat(userName).concat("&password=").concat(password),
				remoteKBClient.getConnectionUrl());
	}
	
	/**
	 * Tests if the HTTP request to run a federated SPARQL query returns the expected<p>
	 * result. It also verifies if the mock service created for this test<p>
	 * executes the correct method.
	 * @throws Exception 
	 */
	@Test
	public void performMechanismCountQueryTest() throws Exception{
		RemoteStoreClient kbClient = mock(RemoteStoreClient.class);
		JSONArray jsonArray = new JSONArray();
		JSONObject jsonObject = new JSONObject();
		jsonObject.put("scfEnergyValue", "-464.940687165");
		jsonArray.put(jsonObject);
		List<String> endpoints = new ArrayList<>();
		String queryEndpointOntoSpeciesKB = "http://localhost:8080/blazegraph/namespace/ontospecies/sparql";
		String queryEndpointOntoCompChemKB = "http://localhost:8080/blazegraph/namespace/ontocompchem/sparql";
		endpoints.add(queryEndpointOntoSpeciesKB);
		endpoints.add(queryEndpointOntoCompChemKB);
		kbClient.setQueryEndpoint(queryEndpoint);
		when(kbClient.executeFederatedQuery(endpoints, formFederatedQuery())).thenReturn(jsonArray);
		JSONArray result = kbClient.executeFederatedQuery(endpoints, formFederatedQuery());
		assertEquals(jsonArray.toString(), result.toString());
		verify(kbClient).executeFederatedQuery(endpoints, formFederatedQuery());
	}

	/**
	 * Tests if the HTTP request to run a SPARQL query returns the expected<p>
	 * result. It also verifies if the mock service created for this test<p>
	 * executes the correct method.
	 * 
	 * @throws SQLException
	 */
	@Test
	public void performFederatedQueryTest() throws SQLException{
		RemoteStoreClient kbClient = mock(RemoteStoreClient.class);
		JSONArray jsonArray = new JSONArray();
		JSONObject jsonObject = new JSONObject();
		jsonObject.put("count", "1");
		jsonArray.put(jsonObject);
		kbClient.setQueryEndpoint(queryEndpoint);
		when(kbClient.execute(formMechanismCountQuery())).thenReturn(jsonArray.toString());
		String result = kbClient.execute(formMechanismCountQuery());
		assertEquals(jsonArray.toString(), result);
		verify(kbClient).execute(formMechanismCountQuery());
	}

	
	/**
	 * Test insert
	 */
	@Test
	public void testInsert() {
		
		String content = 
		"<rdf:RDF\r\n"+
		"    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\r\n"+
		"    xmlns:j.0=\"http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#\">\r\n"+
		"  <j.0:FoodCourt rdf:about=\"http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/FoodCourt-001.owl#FoodCourt-001\"/>\r\n"+
		"</rdf:RDF>\r\n";
		
		RemoteStoreClient kbClient = Mockito.spy(RemoteStoreClient.class);
		Mockito.doReturn(1).when(kbClient).executeUpdate(any(UpdateRequest.class));
		
		kbClient.insert(null, content, null);
		
		Mockito.verify(kbClient).insert(null, content, null);
		Mockito.verify(kbClient).executeUpdate(any(UpdateRequest.class));
	}
	
	/**
	 * Test get method
	 */
	@Test
	public void testGet() {
		
		String actual = 
		"<rdf:RDF\r\n"+
				"    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\r\n"+
				"    xmlns:j.0=\"http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#\">\r\n"+
				"  <j.0:FoodCourt rdf:about=\"http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/FoodCourt-001.owl#FoodCourt-001\"/>\r\n"+
				"</rdf:RDF>\r\n";
				
		//mock result
		Model model = ModelFactory.createDefaultModel();
		Statement s = ResourceFactory.createStatement(ResourceFactory.createResource("http://www.theworldavatar.com/kb/sgp/singapore/wastenetwork/FoodCourt-001.owl#FoodCourt-001"), 
				ResourceFactory.createProperty("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"), ResourceFactory.createResource("http://www.theworldavatar.com/ontology/ontowaste/OntoWaste.owl#FoodCourt"));
		model.add(s);
		
		RemoteStoreClient kbClient = Mockito.spy(RemoteStoreClient.class);
		Mockito.doReturn(model).when(kbClient).executeConstruct(any(Query.class));
		
		String resourceUrl = null;
		String accept = null;
		
		String result = kbClient.get(resourceUrl, accept);
		
		verify(kbClient).get(resourceUrl, accept);
		verify(kbClient).executeConstruct(any(Query.class));
		
		assertEquals(result, actual);
	}

	/**
	 * Test connect method
	 */
	@Test
	public void testConnect() throws NoSuchMethodException, NoSuchFieldException, SQLException, InvocationTargetException, IllegalAccessException {
		queryEndpoint = "http://localhost:8080/test";
		RemoteStoreClient kbClient = new RemoteStoreClient(queryEndpoint);
		Method connect = kbClient.getClass().getDeclaredMethod("connect");
		connect.setAccessible(true);
		Field connection = kbClient.getClass().getDeclaredField("conn");
		connection.setAccessible(true);
		Field statement = kbClient.getClass().getDeclaredField("stmt");
		statement.setAccessible(true);
		Connection conn = (Connection) connection.get(kbClient);
		java.sql.Statement stmt = (java.sql.Statement) statement.get(kbClient);
		assertNull(conn);
		assertNull(stmt);
		connect.invoke(kbClient);
		conn = (Connection) connection.get(kbClient);
		stmt = (java.sql.Statement) statement.get(kbClient);
		assertFalse(conn.isClosed());
		assertNotNull(stmt);
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
	
	/**
	 * A federated query developed to eqecute against the endpoints of OntoSpecies and OntoCompChem Knowledge Bases.  
	 * @return
	 */
	public static String formFederatedQuery() {
		String query ="PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> "
				+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> "
				+ "PREFIX gc: <http://purl.org/gc/> "
				+ "SELECT DISTINCT ?species ?compchemspecies ?crid ?atomicBond ?geometry ?enthalpyOfFormationValue ?scfEnergyValue ?zeroEnergyValue "
				+ "WHERE { "
				+ "?species OntoSpecies:casRegistryID ?crid . "
				+ "?species OntoSpecies:hasAtomicBond ?atomicBond . "
				+ "?species OntoSpecies:hasGeometry ?geometry . "
				+ "?species OntoSpecies:hasStandardEnthalpyOfFormation ?enthalpy . "
				+ "?enthalpy OntoSpecies:value ?enthalpyOfFormationValue ."
				+ "?compchemspecies ontocompchem:hasUniqueSpecies ?species . "
				+ "?compchemspecies gc:isCalculationOn ?scfEnergy . "
				+ "?scfEnergy a ontocompchem:ScfEnergy . "
				+ "?scfEnergy gc:hasElectronicEnergy ?scfElectronicEnergy . "
				+ "?scfElectronicEnergy gc:hasValue ?scfEnergyValue . "
				+ "?compchemspecies gc:isCalculationOn ?zeroEnergy . "
				+ "?zeroEnergy a ontocompchem:ZeroPointEnergy . "
				+ "?zeroEnergy gc:hasElectronicEnergy ?zeroElectronicEnergy . "
				+ "?zeroElectronicEnergy gc:hasValue ?zeroEnergyValue . "
				+ "}";
		
		return query;
	}

	/**
	 * A federated query developed to execute against the endpoints of<br>
	 * OntoSpecies and OntoCompChem Knowledge Bases to retrieve partial<br>
	 *  details of species.
	 *   
	 * @return
	 */
	public static String formFederatedQuery2() {
		String query ="PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> "
				+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> "
				+ "PREFIX gc: <http://purl.org/gc/> "
				+ "SELECT * "
				+ "WHERE { "
				+ "?species OntoSpecies:casRegistryID ?crid . "
				+ "?compchemspecies ontocompchem:hasUniqueSpecies ?species . "
				+ "?compchemspecies gc:isCalculationOn ?scfEnergy . "
				+ "?scfEnergy a ontocompchem:ScfEnergy . "
				+ "?scfEnergy gc:hasElectronicEnergy ?scfElectronicEnergy . "
				+ "?scfElectronicEnergy gc:hasValue ?scfEnergyValue . "
				+ "?compchemspecies gc:isCalculationOn ?zeroEnergy . "
				+ "?zeroEnergy a ontocompchem:ZeroPointEnergy . "
				+ "?zeroEnergy gc:hasElectronicEnergy ?zeroElectronicEnergy . "
				+ "?zeroElectronicEnergy gc:hasValue ?zeroEnergyValue . "
				+ "}";
		
		return query;
	}

	public static void main(String[] args){
		RemoteStoreClient kbClient = new RemoteStoreClient();
		List<String> endpoints = new ArrayList<>();
		String queryEndpointOntoSpeciesKB = "http://www.theworldavatar.com/blazegraph/namespace/ontospecies/sparql";
		String queryEndpointOntoCompChemKB = "http://www.theworldavatar.com/blazegraph/namespace/ontocompchem/sparql";
		endpoints.add(queryEndpointOntoSpeciesKB);
		endpoints.add(queryEndpointOntoCompChemKB);
		try {
			JSONArray result = kbClient.executeFederatedQuery(endpoints, formFederatedQuery2());
			System.out.println(result.toString());
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
}
