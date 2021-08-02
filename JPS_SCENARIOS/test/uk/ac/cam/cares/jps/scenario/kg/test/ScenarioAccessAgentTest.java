package uk.ac.cam.cares.jps.scenario.kg.test;

import static org.junit.Assert.*;
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
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.scenario.ScenarioLog;
import uk.ac.cam.cares.jps.scenario.ScenarioManagementAgent;
import uk.ac.cam.cares.jps.scenario.kb.ScenarioStoreClient;
import uk.ac.cam.cares.jps.scenario.kg.AccessAgent;
import uk.ac.cam.cares.jps.scenario.kg.ScenarioAccessAgent;

public class ScenarioAccessAgentTest {

	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder();
	
	private String filePath;
	private String queryString = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
	
	String scenarioName = "testScenario";
	String scenarioResource = "http://example.com/test/scenarioResource.owl";
	String accept = "application/n-triples";
	
	String scenarioUrl;
	
	@Before
	public void setUp() throws URISyntaxException, IOException { //TODO
		// Test rdf file
		String filePathDir = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" ;
		
		Path testResourcePath = Paths.get(filePathDir+"/testRDF.rdf");
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/testRDF.rdf");		
		Files.copy(testResourcePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		filePath = tempFilePath.toString();
	}
			
	
	private JSONObject createRequestParams() {
		
		scenarioUrl = ScenarioAccessAgent.getScenarioUrl(scenarioName);
		
		JSONObject requestParams;
		requestParams = new JSONObject()
				.put(JPSConstants.METHOD, HttpGet.METHOD_NAME)
				.put(JPSConstants.PATH, "/"+scenarioName)
				.put(JPSConstants.REQUESTURL, scenarioUrl)
				.put(JPSConstants.SCENARIO_RESOURCE, scenarioResource)
				.put(JPSConstants.SCENARIO_DATASET, "test")
				.put(JPSConstants.CONTENTTYPE, accept)
				.put(JPSConstants.HEADERS, accept)
				.put(JPSConstants.TARGETIRI, "test");
		
		return requestParams;
	}
	
	@Test
	 public void testScenarioAccessAgent() {
		ScenarioAccessAgent agent = null;
        try {
            agent = new ScenarioAccessAgent();
        } finally {
            assertNotNull(agent);
        }
    }
	
	@Test
	public void testProcessRequestParameters() {
		
		ScenarioAccessAgent agent = Mockito.spy(ScenarioAccessAgent.class);
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
	public void testGetScenarioUrl() {
		String scenarioUrl = ScenarioAccessAgent.getScenarioUrl(scenarioName);
		assertNotNull(scenarioUrl);
		assertTrue(scenarioUrl.contains(scenarioName));
	}

	@Test 
	public void testGet() {	//TODO
		
		String getOrQuery = "getOrQueryCalled";
		String call = "callCalled";
		
		ScenarioAccessAgent agent = Mockito.spy(ScenarioAccessAgent.class);
		Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
		Mockito.doReturn(getOrQuery).when(agent).getOrQuery(any(JSONObject.class), any(String.class), any(boolean.class));
		Mockito.doReturn(call).when(agent).call(any(JSONObject.class),any(String.class),any(ScenarioLog.class));
		
		JSONObject requestParams = createRequestParams();
		JSONObject result;
		
		//No operation
		result = agent.get(requestParams);
		verify(agent).get(requestParams);
		verify(agent).getOrQuery(any(JSONObject.class), any(String.class), any(boolean.class));
		assertEquals(getOrQuery,result.getString("result"));
		
		//Operations
		requestParams.put(JPSConstants.PATH, "/"+scenarioName+"/call");
		requestParams.put(JPSConstants.REQUESTURL, scenarioUrl+"/call");
		result = agent.get(requestParams);
		verify(agent,Mockito.times(2)).get(requestParams);
		verify(agent).call(any(JSONObject.class),any(String.class),any(ScenarioLog.class));
		assertEquals(call,result.getString("result"));
	}
	
	/**
	 * Test getFromKnowledgeBase is called when no sparql query is provided.
	 */
	@Test
	public void testGetOrQuery() {
		
		String getKG = "getFromKG";
		String queryKG = "queryKG";
		
		ScenarioAccessAgent agent = Mockito.spy(ScenarioAccessAgent.class);
		Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
		Mockito.doReturn(getKG).when(agent).getFromKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class),any(String.class));
		Mockito.doReturn(queryKG).when(agent).queryKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class));
		
		JSONObject requestParams = createRequestParams();
		
		String result = agent.getOrQuery(requestParams,scenarioName,false);
		
		verify(agent).getFromKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class),any(String.class));
		verify(agent,Mockito.times(0)).queryKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class));
		assertEquals(result,getKG);
	}
	
	/**
	 * Test queryKnowledgeBase is called when a sparql query is provided. 
	 */
	@Test
	public void testGetOrQueryWithSparqlQuery() {
		
		String getKG = "getFromKG";
		String queryKG = "queryKG";
		
		ScenarioAccessAgent agent = Mockito.spy(ScenarioAccessAgent.class);
		Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
		Mockito.doReturn(getKG).when(agent).getFromKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class),any(String.class));
		Mockito.doReturn(queryKG).when(agent).queryKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class));
		
		JSONObject requestParams = createRequestParams();	
		requestParams.put(JPSConstants.QUERY_SPARQL_QUERY, "test");
		
		String result;
		result = agent.getOrQuery(requestParams,scenarioName,false);
		
		verify(agent,Mockito.times(0)).getFromKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class),any(String.class));
		verify(agent).queryKnowledgeBase(any(ScenarioStoreClient.class),any(String.class),any(String.class),any(boolean.class));
		assertEquals(result,queryKG);
	}
	
	/**
	 * Test exception is thrown if a sparql update is given in Http Get request. 
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testGetOrQueryWithSparqlUpdate() {
				
		ScenarioAccessAgent agent = Mockito.spy(ScenarioAccessAgent.class);
		Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
		
		JSONObject requestParams  = createRequestParams();
		requestParams.put(JPSConstants.QUERY_SPARQL_UPDATE, "test");
		
		agent.getOrQuery(requestParams,scenarioName,false);		
	}
	
	/**
	 * Test get from existing file in scenario folder
	 */
	@Test
	public void testGetFromKnowledgeBaseFileExists() {
		
		String getReturn = "getCalled!";
		ScenarioStoreClient storeClient = Mockito.mock(ScenarioStoreClient.class);
		Mockito.when(storeClient.exists(scenarioResource)).thenReturn(true);
		Mockito.when(storeClient.get(scenarioResource, accept)).thenReturn(getReturn);
		 
		ScenarioAccessAgent agent = new ScenarioAccessAgent();
		String result = agent.getFromKnowledgeBase(storeClient, (String) null, scenarioResource, false, accept);
		 
		assertEquals(getReturn,result);
		verify(storeClient).exists(scenarioResource);
		verify(storeClient).get(scenarioResource, accept);
		verify(storeClient, Mockito.times(0)).put(any(String.class),any(String.class),any(String.class));
	}

	@Test
	public void testGetDatasetUrl() {
		String testUrl = "http://localhost:8080/kb/testDataset/test.owl";
		String result = ScenarioAccessAgent.getDatasetUrl(testUrl);
		assertEquals("http://localhost:8080/dataset/testDataset",result);
		
		testUrl = "http://localhost:8080/dataset/testDataset";
		result = ScenarioAccessAgent.getDatasetUrl(testUrl);
		assertEquals("http://localhost:8080/dataset/testDataset",result);
		
		testUrl = "http://localhost:8080/testDataset";
		result = ScenarioAccessAgent.getDatasetUrl(testUrl);
		assertEquals("http://localhost:8080/testDataset",result);
	}
	
	//TODO copy on read
	//TODO this call KBC -> KBAgent
	/*
	@Test 
	public void testGetFromKnowledgeBase() {

		String getReturn = "getCalled!";
		
		ScenarioStoreClient storeClient = Mockito.mock(ScenarioStoreClient.class);
		Mockito.when(storeClient.exists(scenarioResource)).thenReturn(false);
		Mockito.when(storeClient.get(scenarioResource, accept)).thenReturn(getReturn);
		 
		ScenarioAccessAgent agent = new ScenarioAccessAgent();
		String result = agent.getFromKnowledgeBase(storeClient, (String) null, filePath, false, accept);
		
		assertEquals(getReturn,result);
		verify(storeClient).exists(scenarioResource);
		verify(storeClient, Mockito.times(0)).get(scenarioResource, accept);
		verify(storeClient, Mockito.times(0)).put(any(String.class),any(String.class),any(String.class));
	}
	*/
	
	
	/*
	@Test
	public void testQueryKnowledgeBase() {
	
		String getReturn = "getCalled!";
		ScenarioStoreClient storeClient = Mockito.mock(ScenarioStoreClient.class);
		Mockito.when(storeClient.exists(scenarioResource)).thenReturn(true);
		Mockito.when(storeClient.get(scenarioResource, accept)).thenReturn(getReturn);
		 
		ScenarioAccessAgent agent = new ScenarioAccessAgent();
		String result = agent.queryKnowledgeBase(storeClient, (String) null, scenarioResource, false);
		
		assertEquals(getReturn,result);
		verify(storeClient).exists(scenarioResource);
		verify(storeClient).get(scenarioResource, accept);
		verify(storeClient, Mockito.times(0)).put(any(String.class),any(String.class),any(String.class));
	}
	*/
	
	/////////////////////////
	
	/*
	@Test
	public void testGetWithSparqlQuery() {

		JSONObject jo = new JSONObject();
		jo.put(JPSConstants.REQUESTURL, "/jps/scenario/test")
			.put(JPSConstants.METHOD, "GET")
			.put(JPSConstants.TARGETIRI, filePath)
			.put(JPSConstants.QUERY_SPARQL_QUERY,queryString );

		ScenarioAccessAgent jpsa = new ScenarioAccessAgent();
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
	
	/*
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
        
        FileBasedKnowledgeBaseClient kbClient = new FileBasedKnowledgeBaseClient(filePath);
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
	*/
	
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
