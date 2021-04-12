package uk.ac.cam.cares.jps.scenario.kg.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.scenario.kg.KnowledgeBaseAgentNew;

public class TestKnowledgeBA   {


	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder();
	private String filePath;
	private String filePathOWL;
	private String queryString = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";

	
	@Before
	public void setUp() throws URISyntaxException, IOException {
		// Test rdf file
		String filePathDir = AgentLocator.getCurrentJpsAppDirectory(this) + "/testres" ;
		
		Path testResourcePath = Paths.get(filePathDir+"/testRDF.rdf");
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/testRDF.rdf");		
		Files.copy(testResourcePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		filePath = tempFilePath.toString();
		
		// Test owl file
		Path testResourcePathOWL = Paths.get(filePathDir+"/testOWL.owl");
		Path tempFilePathOWL = Paths.get(tempFolder.getRoot().toString() + "/testOWL.owl");
		Files.copy(testResourcePathOWL, tempFilePathOWL, StandardCopyOption.REPLACE_EXISTING);
		filePathOWL = tempFilePathOWL.toString();
	}
	/** assert that KBANew is created 
	 * 
	 */
	@Test
	 public void testNewKBAgent() {
	        KnowledgeBaseAgentNew jpsa = null;
	        try {
	            jpsa = new KnowledgeBaseAgentNew();
	        } finally {
	            assertNotNull(jpsa);
	        }
	    }
	 /** Test Sparql query with String. Should return result as String.
	  */
	@Test
	public void testBaseQueryDirect() {
		JSONObject jo = new JSONObject()
				.put(JPSConstants.TARGETIRI, filePath)
				.put(JPSConstants.QUERY_SPARQL_QUERY,queryString );
//		AgentCaller.executeGetWithJsonParameter("jps/kb/scenarioFolder", jo.toString());

        KnowledgeBaseAgentNew jpsa = new KnowledgeBaseAgentNew();
        JSONObject result = jpsa.main(jo);		
		JSONArray ja = new JSONArray(result.getString("result")); 
		jo = ja.getJSONObject(0); 
		assertEquals("OH",jo.get("o").toString());
	}
	/** Test Sparql update with String. Should return result as String. Uses testBaseQueryDirect
	 * 
	 * @throws ParseException
	 */
	@Test
	public void testBaseUpdateDirect() throws ParseException {
		
		testBaseQueryDirect();
		String testUpdate = getUpdateRequest().toString();
		KnowledgeBaseAgentNew jpsa = new KnowledgeBaseAgentNew();
		JSONObject jo = new JSONObject()
		.put(JPSConstants.TARGETIRI,  filePath)
		.put(JPSConstants.QUERY_SPARQL_UPDATE , testUpdate );
        jpsa.main(jo);
        String queryString = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
        jo = new JSONObject()
        		.put(JPSConstants.TARGETIRI,  filePath)
        		.put(JPSConstants.QUERY_SPARQL_QUERY,queryString );
        JSONObject result = jpsa.main(jo);
        JSONArray ja = new JSONArray(result.getString("result")); 
		jo = ja.getJSONObject(0); 
		assertEquals("TEST",jo.get("o").toString());
	}
	/** test inputValidate() method of KnowledgeBaseAgent
	 * @throws ParseException 
	 * @throws JSONException 
	 * 
	 */
	@Test
	public void testValidateInput() throws JSONException, ParseException {
		JSONObject jo = new JSONObject()
				.put(JPSConstants.TARGETIRI,  filePath);

		KnowledgeBaseAgentNew jpsa = new KnowledgeBaseAgentNew();
		assertFalse(jpsa.validateInput(jo)); // No query/update
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
	/** Test Sparql update with String. Should return result as String. Uses testBaseQueryDirect
	 *  Uses AgentCaller
	 * @throws ParseException
	 */
	@Test
	public void testBaseUpdateAgent() throws ParseException {
		
		testBaseQueryDirect();
		String testUpdate = getUpdateRequest().toString();
		KnowledgeBaseAgentNew jpsa = new KnowledgeBaseAgentNew();
		 JSONObject jo = new JSONObject()
		.put(JPSConstants.TARGETIRI,  filePath)
		.put(JPSConstants.QUERY_SPARQL_UPDATE , testUpdate );
		AgentCaller.executeGetWithJsonParameter("jps/kb-new", jo.toString());
		String queryString = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
        jo = new JSONObject()
        		.put(JPSConstants.TARGETIRI,  filePath)
        		.put(JPSConstants.QUERY_SPARQL_QUERY,queryString );
        JSONObject result = jpsa.main(jo);
        JSONArray ja = new JSONArray(result.getString("result")); 
		jo = ja.getJSONObject(0); 
		assertEquals("TEST",jo.get("o").toString());
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
