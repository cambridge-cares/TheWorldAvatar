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
import java.util.List;

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
	 * Copy test resources into temporary folder.
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
	
	//// Test constructors
	
	/**
	 * Test constructor with file path provided.
	 */
	@Test
	public void testConstructorWithFilePath() {
		
		kbClient = new FileBasedKnowledgeBaseClient(filePath);
		assertEquals(filePath, kbClient.getPath(null));
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
	 * Test initialisation of Dataset and RDFConnection.
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
	 * Test constructor with multiple files and contexts.
	 * @throws SecurityException 
	 * @throws NoSuchFieldException 
	 * @throws IllegalAccessException 
	 * @throws IllegalArgumentException 
	 */
	@Test
	public void testLoadMultipleGraphs() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		String[] filePaths = new String[2];
		filePaths[0] = filePath;
		filePaths[1] = filePathNQ;
		
		String[] graphs = new String[2];
		graphs[0] = "http://example.com/triples";
		graphs[1] = "http://example.com/context";
				
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(graphs, filePaths);
		
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
		Model modelNull = fieldValue.getDefaultModel();
		
		assertTrue(modelNull.isEmpty());
		assertFalse(model0.isEmpty());
		assertFalse(model1.isEmpty());
		
		//Load default graph
		kbClient.load(filePathOWL);
		modelNull = fieldValue.getDefaultModel();
		assertFalse(modelNull.isEmpty());
	}
	
	/**
	 * Test load graphs and write.
	 * 
	 * @throws SecurityException 
	 * @throws NoSuchMethodException 
	 * @throws InvocationTargetException 
	 * @throws IllegalArgumentException 
	 * @throws IllegalAccessException 
	 * @throws NoSuchFieldException 
	 * @throws IOException 
	 */
	@Test
	public void testLoadGraphAndWrite() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchFieldException, IOException {
	
		kbClient = new FileBasedKnowledgeBaseClient();
	
		// TEST LOAD
		
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
  		
  		// TEST WRITE
  		
  		//Test write to file
		File file0 = new File(filePath);
		File file1 = new File(filePathOWL);
		File file2 = new File(filePathNQ);
		
		//delete files if they exist
		Files.deleteIfExists(Paths.get(filePath));
		assertFalse(file0.exists());
		Files.deleteIfExists(Paths.get(filePathOWL));
		assertFalse(file1.exists());
		Files.deleteIfExists(Paths.get(filePathNQ));
		assertFalse(file2.exists());
  			
		//write file
		kbClient.writeToFile();
		assertTrue(file0.exists());
		assertTrue(file1.exists());
		assertTrue(file2.exists());
	}
	
	/**
	 * Test load quads. Supplied name should be overriden by context in file.
	 * 
	 * @throws IllegalAccessException 
	 * @throws IllegalArgumentException 
	 * @throws SecurityException 
	 * @throws NoSuchFieldException 
	 * @throws NoSuchMethodException 
	 * @throws InvocationTargetException 
	 */
	@Test
	public void testLoadQuadOverwriteName() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException, NoSuchMethodException, InvocationTargetException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		
		String wrongContext = "http://example.com/differentContext"; 
		String context = "http://example.com/context";
		
		kbClient.load(wrongContext, filePathNQ);
		
		// check a model is loaded to the context given in the file
		assertNotNull(kbClient.getClass().getDeclaredField("dataset"));
  		Field field = kbClient.getClass().getDeclaredField("dataset");;
  		field.setAccessible(true);
  		Dataset fieldValue = (Dataset) field.get(kbClient);
  		assertFalse(fieldValue.containsNamedModel(wrongContext));
  		assertTrue(fieldValue.containsNamedModel(context));
  		Model model1 = fieldValue.getNamedModel(context);
  		assertFalse(model1.isEmpty());
  		
  		List<String> names = kbClient.getGraphNames();
  		assertEquals(1, names.size());
  		assertEquals(context, names.get(0));
  		
  		assertTrue(kbClient.containsGraph(context));
  		assertFalse(kbClient.containsGraph(wrongContext));
	}
	 
	/**
	 * Test load quads to default graph. The model should be loaded as a named graph.
	 * 
	 * @throws IllegalAccessException 
	 * @throws IllegalArgumentException 
	 * @throws SecurityException 
	 * @throws NoSuchFieldException 
	 */
	@Test
	public void testLoadQuadDefault() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		
		String context = "http://example.com/context";
		
		//Try loading quad to default. Model should be loaded to a named graph
		kbClient.load(filePathNQ);
		
		//Check the quad is loaded to the correct named graph
		assertNotNull(kbClient.getClass().getDeclaredField("dataset"));
  		Field field = kbClient.getClass().getDeclaredField("dataset");;
  		field.setAccessible(true);
  		Dataset fieldValue = (Dataset) field.get(kbClient);		  		
  		assertTrue(fieldValue.containsNamedModel(context));
  		Model model1 = fieldValue.getNamedModel(context);
  		assertFalse(model1.isEmpty());
		  		
		assertTrue(kbClient.containsGraph(context));
  		
  		//Assert default graph is empty
  		Model model2 = fieldValue.getDefaultModel();
  		assertTrue(model2.isEmpty());
  		assertTrue(kbClient.getPath(null) == null);
  		assertTrue(kbClient.getLang(null) == null);
	}
	
	/**
	 * Load a model.
	 */
	@Test
	public void testLoadFileToDefault() {
	
		kbClient = new FileBasedKnowledgeBaseClient();
		
		kbClient.load(filePath);
		
		assertEquals(filePath, kbClient.getPath(null));
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
	}
	
	/**
	 * Test exception thrown by giving bad file path to load.
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testLoadBadFile() {
	
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load("Example/does/not/exist");
	}

	/**
	 * Test exception when calling load without specifying a path
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testLoadNoFile() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load();
	}
	
	////  Test errors thrown by edge cases
	
	/**
	 * Multiple contexts in quad. Expect an error.
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testMultipleContexts() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load("http://example.com/context", filePathNQ2);
	}
	
	/**
	 * Multiple quads with same context. Expect an error.
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testSameContext() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load("http://example.com/context", filePathNQ);
		kbClient.load("http://example.com/context2", filePathNQ);
	}
	
	/**
	 * Loading multiple files to same graph. Expect an error.
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testSameGraph() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load("http://example.com/context", filePathOWL);
		kbClient.load("http://example.com/context", filePath);
	}
	
	/**
	 * Loading multiple files to default graph. Expect an error.
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testMultipleDefault() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePathOWL);
		kbClient.load(filePath);
	}
	
	//// Writing to file

	/**
	 * Test writing to new file
	 * @throws IOException
	 */
	@Test
	public void testWriteGraphToFile() throws IOException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		
		//TEST WRITE DEFAULT
		
		kbClient.load(filePath);
		
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
				
		//write file
		String newFilePath = Paths.get(tempFolder.getRoot().toString() + "/newfile.nq").toString();
		kbClient.writeToFile(null, newFilePath, Lang.NQ);
		
		File file = new File(newFilePath);
		assertTrue(file.exists());
		
		// TEST WRITE NAMED
		
		kbClient.load( "http://example.com/triples", filePathOWL);
		
		//write file
		String newFilePath2 = Paths.get(tempFolder.getRoot().toString() + "/newfile2.nq").toString();
		kbClient.writeToFile("http://example.com/triples", newFilePath2, Lang.NQ);
		
		File file2 = new File(newFilePath2);
		assertTrue(file2.exists());
	} 
	
	/**
	 * Test end. Should write to file and then close resources.
	 */
	@Test
	public void testEnd() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePath);
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());		

		//Change output path to check if file is written
		String newFilePath = Paths.get(tempFolder.getRoot().toString() + "/newfile.owl").toString();
		kbClient.setPath(null, newFilePath);
		
		kbClient.end();
		
		assertFalse(kbClient.isConnected());
		assertTrue(kbClient.isEmpty());
		File file = new File(newFilePath);
		assertTrue(file.exists());
	}
	
	/**
	 * Set and get file path variable.
	 * 
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	@Test
	public void testSetGetFilePath() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
	
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.setPath(filePath);
	    assertEquals(filePath, kbClient.getPath(null));
	}

	/**
	 * Set and get Query endpoint should set and get the filePath variable.
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
	    assertEquals(filePath, kbClient.getPath(null));
	    assertEquals(filePath, kbClient.getQueryEndpoint());
	}
	
	/**
	 * Set and get Update endpoint should set and get the filePath variable.
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
	    assertEquals(filePath, kbClient.getPath(null));
	    assertEquals(filePath, kbClient.getUpdateEndpoint());
	}
	
	/**
	 * Set and get the query string.
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
	 * Set a new serialisation language for output.
	 * 
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 * @throws NoSuchFieldException
	 * @throws SecurityException
	 */
	@Test
	public void testSetOutputLang() throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		
	    assertEquals(null,kbClient.getLang(null));
	    
	    //Set lang to NQ
		kbClient.setOutputLang(Lang.NQ);
		assertEquals(Lang.NQ,kbClient.getLang(null));
	}
	
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
		List<String> names = kbClient.getGraphNames();
		
	    assertEquals(null, kbClient.getLang("default"));
	    assertEquals(Lang.NQUADS, kbClient.getLang(names.get(0)));
	}

	/**
	 * Set output serialization language for named graphs.
	 * @param langOut
	 * @throws SecurityException 
	 * @throws NoSuchFieldException 
	 * @throws IllegalAccessException 
	 * @throws IllegalArgumentException 
	 */
	@Test
	public void testSetOutputLangs() throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		kbClient = new FileBasedKnowledgeBaseClient();
	
		kbClient.load(filePath);
		kbClient.load(filePathNQ);
		
		List<String> names = kbClient.getGraphNames();
		
		//Check loaded languages
		assertEquals(Lang.RDFXML, kbClient.getLang(null));
		assertEquals(1, names.size());
		assertEquals(Lang.NQ, kbClient.getLang(names.get(0)));
		
		//Change language
		kbClient.setOutputLang(names.get(0), Lang.RDFXML);
		
		assertEquals(Lang.RDFXML,kbClient.getLang(names.get(0)));
	}
	
	/**
	 * Test Sparql update.
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
		
		//check file is written after update
		FileBasedKnowledgeBaseClient kbClientNew = new FileBasedKnowledgeBaseClient();
		kbClientNew.load(filePath);	
		String resultNew = kbClientNew.execute(testQuery);
		assertEquals("[{\"o\":\"TEST\"}]", resultNew);
	}
	
	/**
	 * Test Sparql Update with String.
	 * @throws ParseException
	 */
	@Test
	public void testExecuteUpdateWithStringArgument() throws ParseException {

		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePath);
		kbClient.setAutoWrite(false); //turn off autowrite
		
		String result = kbClient.execute(testQuery);
		assertEquals("[{\"o\":\"OH\"}]", result);
		
		//perform update
		kbClient.executeUpdate(getUpdateRequest().toString());
		
		result = kbClient.execute(testQuery);
		assertEquals("[{\"o\":\"TEST\"}]", result);

		//check file is not written after update
		FileBasedKnowledgeBaseClient kbClientNew = new FileBasedKnowledgeBaseClient();
		kbClientNew.load(filePath);	
		String resultNew = kbClientNew.execute(testQuery);
		assertEquals("[{\"o\":\"OH\"}]", resultNew);
	}

	/**
	 * Test Sparql Update with UpdateRequest.
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
	 * 
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
	 * 
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
