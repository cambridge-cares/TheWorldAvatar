package uk.ac.cam.cares.jps.scenario.kb.test;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.FileBasedKnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.scenario.kb.ScenarioStoreClient;

public class ScenarioStoreClientTest {

	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder();
	
	private String scenarioFolderPath; 
	private String filePath;
	private String scenarioUrl = "http://example.com/test";
	private String testResourceUrl = "testResourceUrl";
	private String testResourceUrlDoesNotExist = "testResourceUrlDoesNotExist";

	String queryString = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
	
	//TODO test getFilePath?
	
	@Before
	public void setUp() throws URISyntaxException, IOException {
		
		scenarioFolderPath = tempFolder.getRoot().toString();
		
		// Test rdf file
		String filePathDir = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" ;
		
		Path testResourcePath = Paths.get(filePathDir+"/testRDF.rdf");
		Path tempFilePath = Paths.get(scenarioFolderPath + "/testRDF.rdf");		
		Files.copy(testResourcePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		filePath = tempFilePath.toString();	
	}
	
	/**
	 * Create a Mockito Spy of the ScenarioStoreClient that uses temporary folder as the scenario folder
	 */
	private ScenarioStoreClient createClientSpy(String testFilePath) {
		ScenarioStoreClient storeClient =  new ScenarioStoreClient(scenarioUrl);
		ScenarioStoreClient storeClientSpy = Mockito.spy(storeClient);
		Mockito.doReturn(testFilePath).when(storeClientSpy).getFilePath(testResourceUrl);
		Mockito.doReturn("").when(storeClientSpy).getFilePath(testResourceUrlDoesNotExist);
		return storeClientSpy;
	}
	
	@Test
	public void testNewStoreClient() {
		ScenarioStoreClient storeClient = new ScenarioStoreClient(scenarioUrl);	
		assertEquals(scenarioUrl, storeClient.getScenarioUrl());
	}
	
	@Test
	public void testExists() {
		ScenarioStoreClient storeClient = createClientSpy(filePath);
		assertTrue(storeClient.exists(testResourceUrl));
		assertFalse(storeClient.exists(testResourceUrlDoesNotExist));
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
		
		String testFilePath = scenarioFolderPath + "/TestPut.nt";
		
		ScenarioStoreClient storeClient = createClientSpy(testFilePath);
		storeClient.put(testResourceUrl, content, "application/n-triples");
		
        String strResult = FileUtil.readFileLocally(testFilePath);
		assertEquals(contentRDF, strResult);
	}

	@Test
	public void testGet() {
		
		// write a test file to temporary folder
		String content = "<http://www.theworldavatar.com/kb/species/species.owl#species_10> <http://www.w3.org/2008/05/skos#altLabel> \"Ar\" .\n";		
		String testFilePath = scenarioFolderPath + "/TestGet.nt";
		FileUtil.writeFileLocally(testFilePath, content); 
		
		ScenarioStoreClient storeClient = createClientSpy(testFilePath);
		String strResult = storeClient.get(testResourceUrl, "application/n-triples");
		
		assertEquals(content, strResult);		
	}
	
	@Test
	public void testQuery() {
		ScenarioStoreClient storeClient = createClientSpy(filePath);
		String strResult = storeClient.query(testResourceUrl, queryString);
		
		assertEquals("[{\"o\":\"OH\"}]",strResult);
	}
	
	@Test
	public void testUpdate() throws ParseException {
		ScenarioStoreClient storeClient = createClientSpy(filePath);
		storeClient.update(testResourceUrl, getUpdateRequest().toString());
		
		//use a file based client to query for the changes
		FileBasedKnowledgeBaseClient kbClient = new FileBasedKnowledgeBaseClient(filePath);
        JSONArray ja = kbClient.executeQuery(queryString);
		JSONObject result = ja.getJSONObject(0); 
		assertEquals("TEST",result.get("o").toString());
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
