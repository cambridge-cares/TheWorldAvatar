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

import org.apache.jena.riot.Lang;
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
	
	private String testQuery = "SELECT ?p ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> ?p ?o.}";
	
	//copy test resources into temporary folder
	@Before
	public void setup() throws URISyntaxException, IOException {
		
		//get test owl file
		Path testResourcePath = Paths.get(this.getClass().getResource("/KBClientTest/species.owl").toURI());
		Path tempFilePath = Paths.get(tempFolder.getRoot().toString() + "/species.owl");		
		Files.copy(testResourcePath, tempFilePath, StandardCopyOption.REPLACE_EXISTING);
		filePath = tempFilePath.toString();
		
		//get test NQ file
		Path testResourcePathNQ = Paths.get(this.getClass().getResource("/KBClientTest/species.nq").toURI());
		Path tempFilePathNQ = Paths.get(tempFolder.getRoot().toString() + "/species.nq");
		Files.copy(testResourcePathNQ, tempFilePathNQ, StandardCopyOption.REPLACE_EXISTING);
		filePathNQ = tempFilePathNQ.toString();
	}
	
	@Test
	public void testConstructorWithFilePath() {
		
		kbClient = new FileBasedKnowledgeBaseClient(filePath);
		assertEquals(filePath, kbClient.getFilePath());
		assertEquals(null, kbClient.getQuery());
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
	}
	
	@Test(expected = JPSRuntimeException.class)
	public void testConstructorWithBadFilePath() {
		
		kbClient = new FileBasedKnowledgeBaseClient("Example/does/not/exist");
	}
	
	@Test 
	public void testConstructorWithFilePathAndQuery() {
				
		kbClient = new FileBasedKnowledgeBaseClient(filePath, testQuery);
		assertEquals(filePath, kbClient.getFilePath());
		assertEquals(testQuery, kbClient.getQuery());
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
	}
	
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
	
	@Test
	public void testLoadFile() {
	
		kbClient = new FileBasedKnowledgeBaseClient();
		
		kbClient.load(filePath);
		
		assertEquals(filePath, kbClient.getFilePath());
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
	}
	
	@Test(expected = JPSRuntimeException.class)
	public void testLoadBadFile() {
	
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load("Example/does/not/exist");
	}
	
	@Test
	public void testLoad() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.setFilePath(filePath);
		kbClient.load();
		
		assertEquals(filePath, kbClient.getFilePath());
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());
	}

	@Test(expected = JPSRuntimeException.class)
	public void testLoadNoFile() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load();
	}
	
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
		
		//TODO: check contents
	}
	
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
		
		//TODO: check contents
	} 
	
	@Test
	public void testEnd() {
		
		kbClient = new FileBasedKnowledgeBaseClient();
		kbClient.load(filePath);
		assertTrue(kbClient.isConnected());
		assertFalse(kbClient.isEmpty());		

		//change output path to check if file is written
		String newFilePath = Paths.get(tempFolder.getRoot().toString() + "/newfile.owl").toString();
		kbClient.setFilePath(newFilePath);
		
		kbClient.end();
		
		assertFalse(kbClient.isConnected());
		assertTrue(kbClient.isEmpty());
		File file = new File(newFilePath);
		assertTrue(file.exists());
	}
	
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

	//Query endpoint should set the filePath variable
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
	
	//Query endpoint should set the filePath variable
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

	
	/*
	@Test
	public void testExecuteUpdate() {
		
	}
	
	@Test
	public void testExecuteUpdateString() {

	}

	@Test
	public void testExecuteUpdateUpdateRequest () {
		
	}
	
	@Test
	public void testExecute() {
	
	}
	
	@Test
	public void testExecuteString () {
		
	}
	
	@Test
	public void testExecuteQueryString () {
		
	}	
	
	@Test
	public void testExecuteQuery() {
		
	}
	
	@Test
	public void testPerfromExecuteQuery() {
		
	}

	@Test
	public void testConvert() {
		
	}
	*/
}
