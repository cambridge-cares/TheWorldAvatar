package uk.ac.cam.cares.jps.accessagent.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;

import java.io.File;
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
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.FileBasedStoreClient;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.accessagent.AccessAgent;

public class AccessAgentTest{

	@TempDir
	File tempFolder;
	
	private String filePath;
	private String queryString = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
	
	@BeforeEach
	public void setUp() throws URISyntaxException, IOException {
		// Test rdf file				
		Path testResourcePath = Paths.get(this.getClass().getResource("/testRDF.rdf").toURI());
		Path tempFilePath = Paths.get(tempFolder.getPath() + "/testRDF.rdf");		
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
	
	@Test
	public void testValidateInput() throws JSONException, ParseException {
		JSONObject jo = new JSONObject()
				.put(JPSConstants.TARGETIRI,  "http://www.example.com/target.owl")
				.put(JPSConstants.REQUESTURL, "http://www.example.com/jps/kb")
				.put(JPSConstants.METHOD, "GET");
		
		// Query present
		AccessAgent agent = new AccessAgent();
		String queryString = getQuery();
		jo.put(JPSConstants.QUERY_SPARQL_QUERY , queryString );
		assertTrue(agent.validateInput(jo));
		
		// Update wrong format
		jo.remove(JPSConstants.QUERY_SPARQL_QUERY );
		jo.put(JPSConstants.QUERY_SPARQL_UPDATE , queryString);
		assertFalse(agent.validateInput(jo));
		
		// Update present
		jo.remove(JPSConstants.QUERY_SPARQL_UPDATE );
		jo.put(JPSConstants.QUERY_SPARQL_UPDATE , getUpdateRequest());
		assertTrue(agent.validateInput(jo));
		
		// No targetIRI
		jo.remove(JPSConstants.TARGETIRI );
		assertFalse(agent.validateInput(jo));
	}
	
	@Test
	public void testGetWithSparqlQuery() {

		AccessAgent agent = Mockito.spy(AccessAgent.class);
		Mockito.doReturn(createStoreClient(filePath)).when(agent).getStoreClient(any(String.class),any(boolean.class),any(boolean.class));
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "GET")
			.put(JPSConstants.TARGETIRI, filePath)
			.put(JPSConstants.QUERY_SPARQL_QUERY,queryString );

        JSONObject result = agent.get(jo);		
		JSONArray ja = new JSONArray(result.getString("result")); 
		jo = ja.getJSONObject(0); 
		assertEquals("OH",jo.get("o").toString());
	}
	
	public void testGetWithSparqlUpdate() throws ParseException {
		
		AccessAgent agent = Mockito.spy(AccessAgent.class);
		Mockito.doReturn(createStoreClient(filePath)).when(agent).getStoreClient(any(String.class),any(boolean.class),any(boolean.class));
		
		String testUpdate = getUpdateRequest();
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "GET")
			.put(JPSConstants.TARGETIRI, filePath)
			.put(JPSConstants.QUERY_SPARQL_UPDATE, testUpdate );

		Assertions.assertThrows(JPSRuntimeException.class, ()->{agent.get(jo);});		
	}
	
	@Test
	public void testGetWithoutQuery() {
		
		// write a test file to temporary folder
		String content = "<http://www.theworldavatar.com/kb/species/species.owl#species_10> <http://www.w3.org/2008/05/skos#altLabel> \"Ar\" .\n";		
		String folderPath = tempFolder.getPath();
		String testFilePath = folderPath + "/TestGet.nt";
		FileUtil.writeFileLocally(testFilePath, content); 
		
		AccessAgent agent = Mockito.spy(AccessAgent.class);
		Mockito.doReturn(createStoreClient(testFilePath)).when(agent).getStoreClient(any(String.class),any(boolean.class),any(boolean.class));
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "GET")
			.put(JPSConstants.TARGETIRI, testFilePath)
			.put(JPSConstants.HEADERS, "application/n-triples");
		
        JSONObject result = agent.get(jo);		
		String strResult = result.getString("result"); 
		
		assertEquals(content, strResult);		
	}
	
	@Test
	public void testPut() {
		
		String content = "<http://www.theworldavatar.com/kb/species/species.owl#species_10> <http://www.w3.org/2008/05/skos#altLabel> \"Ar\" .\n";			
		
		String folderPath = tempFolder.getPath();
		String testFilePath = folderPath + "/TestPut.nt"; 
		
		AccessAgent agent = Mockito.spy(AccessAgent.class);
		Mockito.doReturn(createStoreClient(testFilePath)).when(agent).getStoreClient(any(String.class),any(boolean.class),any(boolean.class));
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "PUT")
			.put(JPSConstants.TARGETIRI, testFilePath)
			.put(JPSConstants.CONTENT, content)
			.put(JPSConstants.CONTENTTYPE, "application/n-triples");
		
        agent.put(jo);		
		
        String strResult = FileUtil.readFileLocally(testFilePath);
		
		assertEquals(content, strResult);		
	}
	
	public void testPutWithSparqlUpdate() throws ParseException {
				
		AccessAgent agent = Mockito.spy(AccessAgent.class);
		Mockito.doReturn(createStoreClient(filePath)).when(agent).getStoreClient(any(String.class),any(boolean.class),any(boolean.class));
		
		String testUpdate = getUpdateRequest().toString();
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "PUT")
			.put(JPSConstants.TARGETIRI, filePath)
			.put(JPSConstants.QUERY_SPARQL_UPDATE, testUpdate );
		
        Assertions.assertThrows( JPSRuntimeException.class, ()->{agent.put(jo);});
	}
	
	public void testPutWithSparqlQuery() {
				
		AccessAgent agent = Mockito.spy(AccessAgent.class);
		Mockito.doReturn(createStoreClient(filePath)).when(agent).getStoreClient(any(String.class),any(boolean.class),any(boolean.class));
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "PUT")
			.put(JPSConstants.TARGETIRI, filePath)
			.put(JPSConstants.QUERY_SPARQL_QUERY, queryString );
		
		Assertions.assertThrows(JPSRuntimeException.class,  ()->{agent.put(jo);});								
	}
	
	@Test
	public void testPost() throws ParseException {
		
		AccessAgent agent = Mockito.spy(AccessAgent.class);
		Mockito.doReturn(createStoreClient(filePath)).when(agent).getStoreClient(any(String.class),any(boolean.class),any(boolean.class));
		
		String testUpdate = getUpdateRequest().toString();
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "POST")
			.put(JPSConstants.TARGETIRI,  filePath)
			.put(JPSConstants.QUERY_SPARQL_UPDATE , testUpdate );
		
		agent.post(jo);		
        
        FileBasedStoreClient kbClient = new FileBasedStoreClient(filePath);
        JSONArray ja = kbClient.executeQuery(queryString);
		JSONObject result = ja.getJSONObject(0); 
		assertEquals("TEST",result.get("o").toString());      
	}
	
	public void testPostWithSparqlQuery() {
		
		AccessAgent agent = Mockito.spy(AccessAgent.class);
		Mockito.doReturn(createStoreClient(filePath)).when(agent).getStoreClient(any(String.class),any(boolean.class),any(boolean.class));
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "POST")
			.put(JPSConstants.TARGETIRI, filePath)
			.put(JPSConstants.QUERY_SPARQL_QUERY, queryString );
		
        Assertions.assertThrows(JPSRuntimeException.class, ()->{agent.post(jo);});								
	}
	
	public void testPostWithoutSparqlUpdate() {
				
		AccessAgent agent = Mockito.spy(AccessAgent.class);
		Mockito.doReturn(createStoreClient(filePath)).when(agent).getStoreClient(any(String.class),any(boolean.class),any(boolean.class));
		
		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/kb/test")
			.put(JPSConstants.METHOD, "POST")
			.put(JPSConstants.TARGETIRI, filePath);
		
        Assertions.assertThrows(JPSRuntimeException.class, ()->{agent.post(jo);});								
	}	
	
	///////////////////////////////////////////////
	
	/**
	 * Create test store client.
	 * Could mock this instead.
	 */
	private StoreClientInterface createStoreClient(String file) {
		return new FileBasedStoreClient(file);
	}
	
	/**
	 * Returns the test Sparql update.
	 * 
	 * @return UpdateRequest
	 * @throws ParseException
	 */
	private static String getUpdateRequest() throws ParseException {
		
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
		
		return builder.buildRequest().toString();
	}
	
	/**
	 * Return test SPARQL query
	 * @return
	 */
	private static String getQuery() {
		return "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
	}
}