package uk.ac.cam.cares.jps.base.query.test;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;

import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.riot.Lang;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.FileBasedKnowledgeBaseClient;

/**
 * JUnit tests for FileBasedKnowledgeClient
 * 
 * @author Casper Lindberg
 *
 */
public class FileBasedKnowledgeBaseClientTest {

	private FileBasedKnowledgeBaseClient kbClient;
	
	// temporary folder for testing
	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder();
		
	private String filePath;
	private String filePathNQ;
	private String filePathNQ2;
	private String filePathOWL;
	
	private String testQuery = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
	
	/**
	 * Copy test resources into temporary folder 
	 * @throws URISyntaxException
	 * @throws IOException
	 */
	@Before
	public void setup() throws URISyntaxException, IOException {
		
		// Test rdf file
		Path testResourcePath = Paths.get(this.getClass().getResource("/KBClientTest/testRDF.rdf").toURI());
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/testRDF.rdf");		
		Files.copy(testResourcePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		filePath = tempFilePath.toString();
		
		// Test owl file
		Path testResourcePathOWL = Paths.get(this.getClass().getResource("/KBClientTest/testOWL.owl").toURI());
		Path tempFilePathOWL = Paths.get(tempFolder.getRoot().toString() + "/testOWL.owl");
		Files.copy(testResourcePathOWL, tempFilePathOWL, StandardCopyOption.REPLACE_EXISTING);
		filePathOWL = tempFilePathOWL.toString();
		
		// Test NQ file
		Path testResourcePathNQ = Paths.get(this.getClass().getResource("/KBClientTest/testQuads.nq").toURI());
		Path tempFilePathNQ = Paths.get(tempFolder.getRoot().toString() + "/testQuads.nq");
		Files.copy(testResourcePathNQ, tempFilePathNQ, StandardCopyOption.REPLACE_EXISTING);
		filePathNQ = tempFilePathNQ.toString();
		
		Path testResourcePathNQ2 = Paths.get(this.getClass().getResource("/KBClientTest/testQuads2.nq").toURI());
		Path tempFilePathNQ2 = Paths.get(tempFolder.getRoot().toString() + "/testQuads2.nq");
		Files.copy(testResourcePathNQ2, tempFilePathNQ2, StandardCopyOption.REPLACE_EXISTING);
		filePathNQ2 = tempFilePathNQ2.toString();
	}
	
	/**
	 * Test constructor with file path provided
	 */
	@Test
	public void testConstructorWithFilePath() {
		
		kbClient = new FileBasedKnowledgeBaseClient(filePath);
		assertEquals(filePath, kbClient.getFilePath());
		assertEquals(null, kbClient.getQuery());
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
	}
	
	/**
	 * Test named graph constructor
	 * @throws IllegalAccessException 
	 * @throws IllegalArgumentException 
	 * @throws SecurityException 
	 * @throws NoSuchFieldException 
	 */
	@Test
	public void testNamedGraphConstructor() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
	
		kbClient = new FileBasedKnowledgeBaseClient("http://example.com/triples", filePath);
		
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
		
		//check graph is loaded
		assertNotNull(kbClient.getClass().getDeclaredField("dataset"));
		Field field = kbClient.getClass().getDeclaredField("dataset");;
		field.setAccessible(true);
		Dataset fieldValue = (Dataset) field.get(kbClient);
			    
		Model model = fieldValue.getNamedModel("http://example.com/triples");
		assertFalse(model.isEmpty());
	}
	
	/**
	 * Test constructor with multiple files and contexts
	 * @throws SecurityException 
	 * @throws NoSuchFieldException 
	 * @throws IllegalAccessException 
	 * @throws IllegalArgumentException 
	 */
	@Test
	public void testConstructorWithArray() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		String[] filePaths = new String[2];
		filePaths[0] = filePath;
		filePaths[1] = filePathNQ;
		
		String[] graphs = new String[2];
		graphs[0] = "http://example.com/triples";
		graphs[1] = "http://example.com/context";
				
		kbClient = new FileBasedKnowledgeBaseClient(graphs, filePaths);
		
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
		
		//check loaded to graphs
		// access private variable
		assertNotNull(kbClient.getClass().getDeclaredField("dataset"));
		Field field = kbClient.getClass().getDeclaredField("dataset");;
		field.setAccessible(true);
		Dataset fieldValue = (Dataset) field.get(kbClient);
			    
		Model model0 = fieldValue.getNamedModel(graphs[0]);
		Model model1 = fieldValue.getNamedModel(graphs[1]);
		
		assertFalse(model0.isEmpty());
		assertFalse(model1.isEmpty());
	}
	
	/**
	 * Test initialisation of Dataset and RDFConnection
	 * 
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 * @throws NoSuchMethodException
	 * @throws SecurityException
	 */
	@Test
	public void testInit() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		
		// access private member
		assertNotNull(kbClient.getClass().getDeclaredMethod("init"));
	    Method init = kbClient.getClass().getDeclaredMethod("init");
	    init.setAccessible(true);
		
	    init.invoke(kbClient);
	    
		assertTrue(kbClient.isConnected());
		assertTrue(kbClient.isEmpty());
	}
	
	/**
	 * Test load graph
	 * @throws SecurityException 
	 * @throws NoSuchMethodException 
	 * @throws InvocationTargetException 
	 * @throws IllegalArgumentException 
	 * @throws IllegalAccessException 
	 * @throws NoSuchFieldException 
	 */
	@Test
	public void testLoadGraph() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchFieldException {
	
		kbClient = new FileBasedKnowledgeBaseClient();
	
	    //load triples to default graph
	    kbClient.load(null, filePath);
		
  		assertNotNull(kbClient.getClass().getDeclaredField("dataset"));
  		Field field = kbClient.getClass().getDeclaredField("dataset");;
  		field.setAccessible(true);
  		Dataset fieldValue = (Dataset) field.get(kbClient);
  		Model model1 = fieldValue.getDefaultModel();
  		assertFalse(model1.isEmpty());
	    
	    //load triples to named graph
	    kbClient.load( "http://example.com/triples", filePathOWL);
	    
	    fieldValue = (Dataset) field.get(kbClient);
  		Model model2 = fieldValue.getNamedModel("http://example.com/triples");
  		assertFalse(model2.isEmpty());
	    
		//load quads 
	    kbClient.load( "http://example.com/context", filePathNQ);
	    
	    fieldValue = (Dataset) field.get(kbClient);
  		Model model3 = fieldValue.getNamedModel("http://example.com/context");
  		assertFalse(model3.isEmpty());
	}
	
	/**
	 * Test load quads. Supplied name overriden by context
	 * @throws IllegalAccessException 
	 * @throws IllegalArgumentException 
	 * @throws SecurityException 
	 * @throws NoSuchFieldException 
	 */
	@Test
	public void testLoadQuad() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		
		//Try loading quad to a different named graph. Model should be loaded to the context
		kbClient.load("http://example.com/differentContext", filePathNQ);
		
		assertNotNull(kbClient.getClass().getDeclaredField("dataset"));
  		Field field = kbClient.getClass().getDeclaredField("dataset");;
  		field.setAccessible(true);
  		Dataset fieldValue = (Dataset) field.get(kbClient);
  		Model model1 = fieldValue.getNamedModel("http://example.com/context");
  		assertFalse(model1.isEmpty());
  		
  		assertNotNull(kbClient.getClass().getDeclaredField("graphs"));
  		Field field2 = kbClient.getClass().getDeclaredField("graphs");;
  		field2.setAccessible(true);
  		@SuppressWarnings("unchecked")
		ArrayList<String> field2Value = (ArrayList<String>) field2.get(kbClient);
  		assertTrue(field2Value.contains("http://example.com/context"));
  		assertFalse(field2Value.contains("http://example.com/differentContext"));
	}
	 
	/**
	 * Test load quads. Supplied name overriden by context
	 * @throws IllegalAccessException 
	 * @throws IllegalArgumentException 
	 * @throws SecurityException 
	 * @throws NoSuchFieldException 
	 */
	@Test
	public void testLoadQuadDefault() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		
		//Try loading quad to default. Model should be loaded to a named graph
		kbClient.load(filePathNQ);
		
		//Check the quad is loaded to the correct context
		assertNotNull(kbClient.getClass().getDeclaredField("dataset"));
  		Field field = kbClient.getClass().getDeclaredField("dataset");;
  		field.setAccessible(true);
  		Dataset fieldValue = (Dataset) field.get(kbClient);
  		Model model1 = fieldValue.getNamedModel("http://example.com/context");
  		assertFalse(model1.isEmpty());
  		
  		assertNotNull(kbClient.getClass().getDeclaredField("graphs"));
  		Field field2 = kbClient.getClass().getDeclaredField("graphs");;
  		field2.setAccessible(true);
  		@SuppressWarnings("unchecked")
		ArrayList<String> field2Value = (ArrayList<String>) field2.get(kbClient);
  		assertTrue(field2Value.contains("http://example.com/context"));
  		
  		//Assert default graph is empty
  		Model model2 = fieldValue.getDefaultModel();
  		assertTrue(model2.isEmpty());
  		
  		assertTrue(kbClient.getFilePath() == null);
	}
	
	/**
	 * Multiple contexts in quad. Expect error
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testMultipleContexts() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load("http://example.com/context", filePathNQ2);
	}
	
	/**
	 * Multiple quads with same context. Expect error.
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testSameContext() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load("http://example.com/context", filePathNQ);
		kbClient.load("http://example.com/context2", filePathNQ);
	}
	
	/**
	 * Loading multiple files to same graph. Expect error.
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testSameGraph() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load("http://example.com/context", filePathOWL);
		kbClient.load("http://example.com/context", filePath);
	}
	
	/**
	 * Multiple files to default graph. Expect error.
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testMultipleDefault() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePathOWL);
		kbClient.load(filePath);
	}
	
	/**
	 * Load a model
	 */
	@Test
	public void testLoadFileToDefault() {
	
		kbClient = new FileBasedKnowledgeBaseClient();
		
		kbClient.load(filePath);
		
		assertEquals(filePath, kbClient.getFilePath());
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
	}
	
	/**
	 * Test exception thrown by giving bad file path to load
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testLoadBadFile() {
	
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load("Example/does/not/exist");
	}
	
	/**
	 * Load model
	 */
	@Test
	public void testLoad() {
				
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.setFilePath(filePath);
		kbClient.load();
		
		assertEquals(filePath, kbClient.getFilePath());
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
	}

	/**
	 * Test exception when calling load without specifying a path
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testLoadNoFile() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load();
	}
	
	//******************
	
	/**
	 * Test write to file
	 * @throws IOException
	 */
	@Test
	public void testWriteToFileDefault() throws IOException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		
		//load file
		kbClient.load(filePath);
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());

		File file = new File(filePath);
		
		//delete file
		Files.deleteIfExists(Paths.get(filePath));
		assertFalse(file.exists());
		
		//write file
		kbClient.writeToFile();
		assertTrue(file.exists());
	}
	
	/**
	 * Test writing to new file
	 * @throws IOException
	 */
	@Test
	public void testWriteToFile() throws IOException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePath);
		
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
				
		//write file
		String newFilePath = Paths.get(tempFolder.getRoot().toString() + "/newfile.owl").toString();
		kbClient.writeToFile(newFilePath, Lang.RDFXML);
		
		File file = new File(newFilePath);
		assertTrue(file.exists());
	} 
	
	/**
	 * Test end. Should write to file and then close resources
	 */
	@Test
	public void testEnd() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePath);
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());		

		//Change output path to check if file is written
		String newFilePath = Paths.get(tempFolder.getRoot().toString() + "/newfile.owl").toString();
		kbClient.setFilePath(newFilePath);
		
		kbClient.end();
		
		assertFalse(kbClient.isConnected());
		assertTrue(kbClient.isEmpty());
		File file = new File(newFilePath);
		assertTrue(file.exists());
	}
	
	/**
	 * Set and get file path variable
	 * 
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	@Test
	public void testSetGetFilePath() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
	
		kbClient = new FileBasedKnowledgeBaseClient();
		
		kbClient.setFilePath(filePath);
		
		// access private variable
		assertNotNull(kbClient.getClass().getDeclaredField("defaultFilePath"));
	    Field field = kbClient.getClass().getDeclaredField("defaultFilePath");;
	    field.setAccessible(true);
	    String fieldValue = (String) field.get(kbClient);
	    
	    assertEquals(filePath, fieldValue);
	    assertEquals(filePath, kbClient.getFilePath());
	}

	/**
	 * Set and get Query endpoint should set and get the filePath variable
	 * 
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	@Test
	public void testSetGetQueryEndpoint() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
	
		kbClient = new FileBasedKnowledgeBaseClient();
	
		assertEquals(filePath, kbClient.setQueryEndpoint(filePath));
		
		// access private variable
		assertNotNull(kbClient.getClass().getDeclaredField("defaultFilePath"));
	    Field field = kbClient.getClass().getDeclaredField("defaultFilePath");;
	    field.setAccessible(true);
	    String fieldValue = (String) field.get(kbClient);
	    
	    assertEquals(filePath, fieldValue);
	    assertEquals(filePath, kbClient.getQueryEndpoint());
	}
	
	/**
	 * Set and get Update endpoint should set and get the filePath variable
	 * 
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	@Test 
	public void testSetGetUpdateEndpoint() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {

		kbClient = new FileBasedKnowledgeBaseClient();
		
		assertEquals(filePath, kbClient.setUpdateEndpoint(filePath));
		
		// access private variable
		assertNotNull(kbClient.getClass().getDeclaredField("defaultFilePath"));
	    Field field = kbClient.getClass().getDeclaredField("defaultFilePath");;
	    field.setAccessible(true);
	    String fieldValue = (String) field.get(kbClient);
	    
	    assertEquals(filePath, fieldValue);
	    assertEquals(filePath, kbClient.getUpdateEndpoint());
	}
	
	/**
	 * Set and get the query string
	 * 
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	@Test
	public void testSetGetQuery() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		
		assertEquals(testQuery, kbClient.setQuery(testQuery));
		
		// access private variable
		assertNotNull(kbClient.getClass().getDeclaredField("query"));
	    Field field = kbClient.getClass().getDeclaredField("query");;
	    field.setAccessible(true);
	    String fieldValue = (String) field.get(kbClient);
		
	    assertEquals(testQuery, fieldValue);
		assertEquals(testQuery, kbClient.getQuery());
	}
	
	/**
	 * Set a new serialisation language for output
	 * 
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	@Test
	public void testSetOutputLang() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		
		// access private variable
		assertNotNull(kbClient.getClass().getDeclaredField("defaultLangOut"));
	    Field field = kbClient.getClass().getDeclaredField("defaultLangOut");;
	    field.setAccessible(true);
	    
	    Lang fieldValue = (Lang) field.get(kbClient);
	    assertEquals(Lang.RDFXML,fieldValue); //default language
	    
	    //Set lang to NQ
		kbClient.setOutputLang(Lang.NQ);
		fieldValue = (Lang) field.get(kbClient);
		assertEquals(Lang.NQ,fieldValue);
	}
	
	//Test with different file type
	/**
	 * Test load and write with different serialisation language. 
	 * langOut variable should be updated during load.
	 * 
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 */
	@Test
	public void testOutputLang() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
	
		kbClient = new FileBasedKnowledgeBaseClient();
		
		kbClient.load(filePathNQ);
		
		// access private variable
		assertNotNull(kbClient.getClass().getDeclaredField("defaultLangOut"));
	    Field field = kbClient.getClass().getDeclaredField("defaultLangOut");;
	    field.setAccessible(true);
	    
	    Lang fieldValue = (Lang) field.get(kbClient);
	    
	    assertEquals(Lang.NQ, fieldValue); 
	}

	/**
	 * Test Sparql update
	 * 
	 * @throws ParseException
	 */
	@Test
	public void testExecuteUpdate() throws ParseException {

		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePath);

		String result = kbClient.execute(testQuery);
		assertEquals("[{\"o\":\"OH\"}]", result);
		
		//perform update
		String testUpdate = getUpdateRequest().toString();
		kbClient.setQuery(testUpdate);
		kbClient.executeUpdate();
		
		result = kbClient.execute(testQuery);
		assertEquals("[{\"o\":\"TEST\"}]", result);
	}
	
	/**
	 * Test Sparql Update with String
	 * @throws ParseException
	 */
	@Test
	public void testExecuteUpdateWithStringArgument() throws ParseException {


		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePath);

		String result = kbClient.execute(testQuery);
		assertEquals("[{\"o\":\"OH\"}]", result);
		
		//perform update
		kbClient.executeUpdate(getUpdateRequest().toString());
		
		result = kbClient.execute(testQuery);
		assertEquals("[{\"o\":\"TEST\"}]", result);
	}

	/**
	 * Test Sparql Update with UpdateRequest 
	 * @throws ParseException
	 */
	@Test
	public void testExecuteUpdateWithUpdateRequestArgument() throws ParseException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePath);

		String result = kbClient.execute(testQuery);
		assertEquals("[{\"o\":\"OH\"}]", result);
		
		//perform update
		kbClient.executeUpdate(getUpdateRequest());
		
		result = kbClient.execute(testQuery);
		assertEquals("[{\"o\":\"TEST\"}]", result);
	}
	
	/**
	 * Test Sparql query. Should return result as String.
	 */
	@Test
	public void testExecute() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePath);
		kbClient.setQuery(testQuery);
		
		String result = kbClient.execute();
		
		String expectedQueryResult = "[{\"o\":\"OH\"}]";
		
		assertEquals(expectedQueryResult, result);
	}
	
	/**
	 * Test Sparql query with String. Should return result as String.
	 */
	@Test
	public void testExecuteWithArgument() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePath);
		
		String result = kbClient.execute(testQuery);
		
		String expectedQueryResult = "[{\"o\":\"OH\"}]";
		
		assertEquals(expectedQueryResult, result);
	}
	
	/**
	 * Test Sparql query. Should return result as JSONArray.
	 * @throws JSONException
	 */
	@Test
	public void testExecuteQuery() throws JSONException {
		 
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePath);
		kbClient.setQuery(testQuery);
		
		JSONArray result = kbClient.executeQuery();		
		JSONObject jo = result.getJSONObject(0); 
		
		assertEquals("OH", jo.get("o").toString());
	}
	
	/**
	 * Test Sparql query with String. Should return result as JSONArray.
	 * @throws JSONException
	 */
	@Test
	public void testExecuteQueryWithArgument () {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePath);
		
		JSONArray result = kbClient.executeQuery(testQuery);
		JSONObject jo = result.getJSONObject(0); 
		
		assertEquals("OH", jo.get("o").toString());
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
