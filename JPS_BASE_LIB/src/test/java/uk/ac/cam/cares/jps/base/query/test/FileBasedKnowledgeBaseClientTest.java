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

import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
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

public class FileBasedKnowledgeBaseClientTest {

	private FileBasedKnowledgeBaseClient kbClient;
	
	// temporary folder for testing
	@Rule
	public TemporaryFolder tempFolder = new TemporaryFolder();
		
	private String filePath;
	private String filePathNQ;
	
	private String testQuery = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
	
	/**
	 * Copy test resources into temporary folder 
	 * @throws URISyntaxException
	 * @throws IOException
	 */
	@Before
	public void setup() throws URISyntaxException, IOException {
		
		// Test owl file
		Path testResourcePath = Paths.get(this.getClass().getResource("/KBClientTest/species.rdf").toURI());
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/species.rdf");		
		Files.copy(testResourcePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		filePath = tempFilePath.toString();
		
		// Test NQ file
		Path testResourcePathNQ = Paths.get(this.getClass().getResource("/KBClientTest/species.nq").toURI());
		Path tempFilePathNQ = Paths.get(tempFolder.getRoot().toString() + "/species.nq");
		Files.copy(testResourcePathNQ, tempFilePathNQ, StandardCopyOption.REPLACE_EXISTING);
		filePathNQ = tempFilePathNQ.toString();
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
	 * Test constructor with bad file path
	 */
	@Test(expected = JPSRuntimeException.class)
	public void testConstructorWithBadFilePath() {
		
		kbClient = new FileBasedKnowledgeBaseClient("Example/does/not/exist");
	}
	
	/**
	 * Test constructor with file path and query string
	 */
	@Test 
	public void testConstructorWithFilePathAndQuery() {
				
		kbClient = new FileBasedKnowledgeBaseClient(filePath, testQuery);
		assertEquals(filePath, kbClient.getFilePath());
		assertEquals(testQuery, kbClient.getQuery());
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
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
	 * Load a model
	 */
	@Test
	public void testLoadFile() {
	
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
		assertNotNull(kbClient.getClass().getDeclaredField("filePath"));
	    Field field = kbClient.getClass().getDeclaredField("filePath");;
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
		assertNotNull(kbClient.getClass().getDeclaredField("filePath"));
	    Field field = kbClient.getClass().getDeclaredField("filePath");;
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
		assertNotNull(kbClient.getClass().getDeclaredField("filePath"));
	    Field field = kbClient.getClass().getDeclaredField("filePath");;
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
		assertNotNull(kbClient.getClass().getDeclaredField("langOut"));
	    Field field = kbClient.getClass().getDeclaredField("langOut");;
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
		assertNotNull(kbClient.getClass().getDeclaredField("langOut"));
	    Field field = kbClient.getClass().getDeclaredField("langOut");;
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
