package uk.ac.cam.cares.jps.scenario.kg.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.FileBasedStoreClient;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.scenario.kg.AccessAgent;

public class AccessAgentTest{

	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder();
	
	private String filePath;
	private String queryString = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
	
	@Before
	public void setUp() throws URISyntaxException, IOException {
		// Test rdf file
		String filePathDir = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" ;
		
		Path testResourcePath = Paths.get(filePathDir+"/testRDF.rdf");
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/testRDF.rdf");		
		Files.copy(testResourcePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		filePath = tempFilePath.toString();
	}
	
	/** assert that KBANew is created 
	 * 
	 */
	@Test
	 public void testNewKBAgent() {
		AccessAgent jpsa = null;
        try {
            jpsa = new AccessAgent();
        } finally {
            assertNotNull(jpsa);
        }
    }
	
	@Test
	public void testProcessRequestParameters() {
		
		AccessAgent agent = Mockito.spy(AccessAgent.class);
		Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
		Mockito.doReturn(null).when(agent).get(any(JSONObject.class));
		Mockito.doNothing().when(agent).put(any(JSONObject.class));
		Mockito.doNothing().when(agent).post(any(JSONObject.class));
		
		JSONObject requestParams;
		
		//test http get
		requestParams = new JSONObject();
		requestParams.put(JPSConstants.METHOD, HttpGet.METHOD_NAME);
		agent.processRequestParameters(requestParams, null);
		verify(agent).validateInput(requestParams);
		verify(agent).get(requestParams);
		
		//test http put
		requestParams = new JSONObject();
		requestParams.put(JPSConstants.METHOD, HttpPut.METHOD_NAME);
		agent.processRequestParameters(requestParams, null);
		verify(agent).validateInput(requestParams);
		verify(agent).put(requestParams);
		
		//test http post
		requestParams = new JSONObject();
		requestParams.put(JPSConstants.METHOD, HttpPost.METHOD_NAME);
		agent.processRequestParameters(requestParams, null);
		verify(agent).validateInput(requestParams);
		verify(agent).post(requestParams);
	}
	
	/** test inputValidate() method of KnowledgeBaseAgent
	 * @throws ParseException 
	 * @throws JSONException 
	 * 
	 */
	@Test
	public void testValidateInput() throws JSONException, ParseException {
		JSONObject jo = new JSONObject()
				.put(JPSConstants.TARGETIRI,  filePath)
				.put(JPSConstants.REQUESTURL, "http://www.example.com/jps/kb/test")
				.put(JPSConstants.METHOD, "GET");
		
		AccessAgent jpsa = new AccessAgent();
		String queryString = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
		jo.put(JPSConstants.QUERY_SPARQL_QUERY , queryString );
		assertTrue(jpsa.validateInput(jo));// Query present
		jo.remove(JPSConstants.QUERY_SPARQL_QUERY );
		jo.put(JPSConstants.QUERY_SPARQL_UPDATE , queryString);
		assertFalse(jpsa.validateInput(jo));//Update wrong format
		jo.remove(JPSConstants.QUERY_SPARQL_UPDATE );
		jo.put(JPSConstants.QUERY_SPARQL_UPDATE , getUpdateRequest().toString());
		assertTrue(jpsa.validateInput(jo));// Update present
	}
	
	@Test
	public void testGetWithSparqlQuery() {

		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "GET")
			.put(JPSConstants.TARGETIRI, filePath)
			.put(JPSConstants.QUERY_SPARQL_QUERY,queryString );

		AccessAgent jpsa = new AccessAgent();
        JSONObject result = jpsa.get(jo);		
		JSONArray ja = new JSONArray(result.getString("result")); 
		jo = ja.getJSONObject(0); 
		assertEquals("OH",jo.get("o").toString());
	}
	
	@Test(expected = JPSRuntimeException.class)
	public void testGetWithSparqlUpdate() throws ParseException {
		
		String testUpdate = getUpdateRequest().toString();
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "GET")
			.put(JPSConstants.TARGETIRI, filePath)
			.put(JPSConstants.QUERY_SPARQL_UPDATE, testUpdate );

		AccessAgent jpsa = new AccessAgent();
        @SuppressWarnings("unused")
		JSONObject result = jpsa.get(jo);		
	}
	
	@Test
	public void testGetWithoutQuery() {
		
		// write a test file to temporary folder
		String content = "<http://www.theworldavatar.com/kb/species/species.owl#species_10> <http://www.w3.org/2008/05/skos#altLabel> \"Ar\" .\n";		
		String folderPath = tempFolder.getRoot().toString();
		String testFilePath = folderPath + "/TestGet.nt";
		FileUtil.writeFileLocally(testFilePath, content); 
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "GET")
			.put(JPSConstants.TARGETIRI, testFilePath)
			.put(JPSConstants.HEADERS, "application/n-triples");
		
		AccessAgent jpsa = new AccessAgent();
        JSONObject result = jpsa.get(jo);		
		String strResult = result.getString("result"); 
		
		assertEquals(content, strResult);		
	}
	
	@Test
	public void testPut() {
		
		String content = "<http://www.theworldavatar.com/kb/species/species.owl#species_10> <http://www.w3.org/2008/05/skos#altLabel> \"Ar\" .\n";			
		String contentRDF = "<rdf:RDF\r\n"+
	    "    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\r\n"+
	    "    xmlns:j.0=\"http://www.w3.org/2008/05/skos#\">\r\n"+
	    "  <rdf:Description rdf:about=\"http://www.theworldavatar.com/kb/species/species.owl#species_10\">\r\n"+
	    "    <j.0:altLabel>Ar</j.0:altLabel>\r\n"+
	    "  </rdf:Description>\r\n"+
	    "</rdf:RDF>\r\n";
		
		String folderPath = tempFolder.getRoot().toString();
		String testFilePath = folderPath + "/TestPut.nt"; 
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "PUT")
			.put(JPSConstants.TARGETIRI, testFilePath)
			.put(JPSConstants.CONTENT, content)
			.put(JPSConstants.CONTENTTYPE, "application/n-triples");
		
		AccessAgent jpsa = new AccessAgent();
        jpsa.put(jo);		
		
        String strResult = FileUtil.readFileLocally(testFilePath);
		
		assertEquals(contentRDF, strResult);		
	}
	
	@Test(expected = JPSRuntimeException.class)
	public void testPutWithSparqlUpdate() throws ParseException {
				
		String testUpdate = getUpdateRequest().toString();
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "PUT")
			.put(JPSConstants.TARGETIRI, filePath)
			.put(JPSConstants.QUERY_SPARQL_UPDATE, testUpdate );
		
		AccessAgent jpsa = new AccessAgent();
        jpsa.put(jo);
	}
	
	@Test(expected = JPSRuntimeException.class)
	public void testPutWithSparqlQuery() {
				
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "PUT")
			.put(JPSConstants.TARGETIRI, filePath)
			.put(JPSConstants.QUERY_SPARQL_QUERY, queryString );
		
		AccessAgent jpsa = new AccessAgent();
        jpsa.put(jo);								
	}
	
	@Test
	public void testPost() throws ParseException {
		
		String testUpdate = getUpdateRequest().toString();
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "POST")
			.put(JPSConstants.TARGETIRI,  filePath)
			.put(JPSConstants.QUERY_SPARQL_UPDATE , testUpdate );
		
		AccessAgent jpsa = new AccessAgent();
        jpsa.post(jo);		
        
        FileBasedStoreClient kbClient = new FileBasedStoreClient(filePath);
        JSONArray ja = kbClient.executeQuery(queryString);
		JSONObject result = ja.getJSONObject(0); 
		assertEquals("TEST",result.get("o").toString());      
	}
	
	@Test(expected = JPSRuntimeException.class)
	public void testPostWithSparqlQuery() {
				
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "POST")
			.put(JPSConstants.TARGETIRI, filePath)
			.put(JPSConstants.QUERY_SPARQL_QUERY, queryString );
		
		AccessAgent jpsa = new AccessAgent();
        jpsa.post(jo);								
	}
	
	@Test(expected = JPSRuntimeException.class)
	public void testPostWithoutSparqlUpdate() {
				
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "POST")
			.put(JPSConstants.TARGETIRI, filePath);
		
		AccessAgent jpsa = new AccessAgent();
        jpsa.post(jo);								
	}	
	
	@Test
	public void testgetShortIRI() {
		String testUrl = "http://www.theworldavatar.com/kb/sgp/singapore/SGTemperatureSensor-001.owl";
		String result = AccessAgent.getShortIRI(testUrl);
		assertEquals("http://kb/sgp/singapore/SGTemperatureSensor-001.owl",result);
		
		testUrl = "http://www.theworldavatar.com/kb/ontokin";
		result = AccessAgent.getShortIRI(testUrl);
		assertEquals("http://kb/ontokin",result);
		
		testUrl = "http://localhost:8080/kb/ontokin";
		result = AccessAgent.getShortIRI(testUrl);
		assertEquals("http://kb/ontokin",result);
		
		testUrl = "http://kb/ontokin";
		result = AccessAgent.getShortIRI(testUrl);
		assertEquals("http://kb/ontokin",result);
	}
	
	/**
	 * Returns the test Sparql update.
	 * 
	 * @return UpdateRequest
	 * @throws ParseException
	 */
	private static UpdateRequest getUpdateRequest() throws ParseException {
		
		//DELETE {?s ?p ?o} 
		//INSERT {?s ?p \"TEST\" } 
		//WHERE {?s ?p ?o.
		//		 FILTER(?s = <http://www.theworldavatar.com/kb/species/species.owl#species_1> && ?p = <http://www.w3.org/2008/05/skos#altLabel>)}
		
		WhereBuilder where = new WhereBuilder()
				.addWhere("?s", "?p", "?o")
				.addFilter("?s = <http://www.theworldavatar.com/kb/species/species.owl#species_1> && ?p = <http://www.w3.org/2008/05/skos#altLabel>");
				
		// Build update
		UpdateBuilder builder = new UpdateBuilder();
				
		// Add where 
		builder.addInsert("?s", "?p", "TEST")
			.addDelete("?s", "?p", "?o")
			.addWhere(where);
		
		return builder.buildRequest();
	}
}