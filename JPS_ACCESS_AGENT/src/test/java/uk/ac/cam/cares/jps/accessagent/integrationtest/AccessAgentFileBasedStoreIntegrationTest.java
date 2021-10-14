package uk.ac.cam.cares.jps.accessagent.integrationtest;

import static org.junit.Assert.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.FileBasedStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.StoreRouter;
import uk.ac.cam.cares.jps.base.util.FileUtil;

/**
 * Integration tests for the AccessAgentCaller, AccessAgent, StoreRouter and FileBasedStoreClient.
 * <p> These tests require the AccessAgent to be running on the localhost.   
 * The OntoKGRouter triple store must be accessible on the www.theworldavatar.com and contain 
 * routing information (i.e. the Tomcat root folder) consistent with the variable <i>rootPath</i>. 
 * <p> The tests can be run by commenting out the @Ignore annotation
 * @author csl37
 *
 */
@Ignore("Requires the AccessAgent to be deployed and running on localhost. Requires Internet access to OntoKGRouter on www.theworldavatar.com. The tomcat root path must correspond to that on OntoKGRouter.")
public class AccessAgentFileBasedStoreIntegrationTest {

	// Test targetIRI and corresponding file path
	static String targetIRI = "http://localhost:8080/kb/test/test.nt";
	
	static String rootPathWin =  "C:\\TOMCAT\\webapps\\ROOT";
	static String rootPath =  "/C:/TOMCAT/webapps/ROOT";
	static String rootPathFromRouter = "file://"+rootPath;
	static String filePath = rootPath + "/kb/test/test.nt";	
	
	// Test query
	static String query = "SELECT ?o WHERE {<http://www.example.com/test/s> <http://www.example.com/test/p> ?o.}";
	
	// Test content and content type, n-triples
	static String content = "<http://www.example.com/test/s> <http://www.example.com/test/p>	<http://www.example.com/test/o>.";
	static String contentType = MediaType.APPLICATION_N_TRIPLES.type;
	
	///////////////////////////////////////////////////////////////
	//Initial checks
	///////////////////////////////////////////////////////////////
	
	@BeforeClass
	public static void initialChecks() {
		checkOntoKGRouter();
		checkStoreRouter();
		checkTomcatRoot();
	}
	
	/**
	 * Perform initial check to ensure that the StoreRouter is able to connect 
	 * to the OntoKGRouter triple store and get the correct test endpoint.
	 */
	public static void checkOntoKGRouter() {
		
		String tomcatrootpathQuery =  "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/ontokgrouter/tomcatrootpath> 	<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasFilePath> ?o.}";
		
		// Get the OntoKGRouter endpoint from the StoreRouter class
		String ontokgrouterEndpoint = TestHelper.getRouterEndpoint();
		assertNotNull("Failed to get OntoKGRouter endpoint!",ontokgrouterEndpoint);
 
		RemoteStoreClient ontokgrouter = new RemoteStoreClient(ontokgrouterEndpoint);
		try {
			// Query the OntoKGRouter to check that the store contains the correct path 
			JSONObject result1 = ontokgrouter.executeQuery(tomcatrootpathQuery).getJSONObject(0);
			assertEquals("OntoKGRouter did not return the expected Tomcat root path!", rootPathFromRouter, result1.get("o").toString()); 
		}catch(RuntimeException e) {
			fail("Failed to connect to OntoKGRouter triple store!");
			e.printStackTrace();
		}
	}
	
	/**
	 * Perform initial check to ensure the StoreRouter instantiates a RemoteStoreClient 
	 * with the correct endpoint.
	 */
	public static void checkStoreRouter() {
		
		StoreClientInterface storeClient = StoreRouter.getStoreClient(targetIRI, true, true);
		
		// Is a FileBasedStoreClient
		assertEquals(FileBasedStoreClient.class, storeClient.getClass());

		// Check store client file path and serialization language
		assertEquals(filePath, ((FileBasedStoreClient) storeClient).getPath(null));
		assertEquals(RDFLanguages.contentTypeToLang(MediaType.APPLICATION_N_TRIPLES.type),((FileBasedStoreClient) storeClient).getLang(null));
	}
	
	/**
	 * Check the local tomcat root directory exists.
	 */
	public static void checkTomcatRoot() {
		Path tomcatRootDir = Paths.get(rootPathWin);
		assertTrue(Files.isDirectory(tomcatRootDir));
		assertTrue(Files.exists(tomcatRootDir));
	}
	
	///////////////////////////////////////////////////////////////
	//Set up and clean up test environment
	///////////////////////////////////////////////////////////////	
	
	@Before
	public void setupTestStore() {
		cleanup();
		FileUtil.writeFileLocally(filePath, content);
	}
	
	@After
	public void cleanup() {
		deleteFile(filePath);
	}
	
	///////////////////////////////////////////////////////////////
	//Integration tests
	///////////////////////////////////////////////////////////////	
	
	@Test
	public void testGet() {	
		
		//get
		String result = AccessAgentCaller.get(null, targetIRI, contentType);
		
		JSONObject jo = new JSONObject(result);
		String result2 = jo.getString("result");
		assertEquals(TestHelper.removeWhiteSpace(content), TestHelper.removeWhiteSpace(result2));			
	}
	
	@Test
	public void testGetWithQuery() {
		
		//query
		String result = AccessAgentCaller.query(null, targetIRI, query);
		
		JSONObject jo = new JSONObject(result);
		String result2 = jo.getString("result");		
		JSONArray ja = new JSONArray(result2); 
		jo = ja.getJSONObject(0); 
		assertEquals("http://www.example.com/test/o",jo.get("o").toString());
	}
	
	@Test
	public void testPut() {
		
		String putFilePath = rootPathWin+"\\kb\\test\\testPut.nt";
		String putTargetIRI = "http://localhost:8080/kb/test/testPut.nt";
		
		//delete file if it already exists
		deleteFile(putFilePath);
        
		//put
        AccessAgentCaller.put(null, putTargetIRI, content, contentType);
        
        //check file exists and check contents
        File f= new File(putFilePath);
        assertTrue(f.exists());
        String strResult = FileUtil.readFileLocally(putFilePath);		
		assertEquals(TestHelper.removeWhiteSpace(content), TestHelper.removeWhiteSpace(strResult));
		
		//cleanup
		deleteFile(putFilePath);
	}
	
	@Test
	public void testPost() throws ParseException, IOException {
				
		//Update
		AccessAgentCaller.update(null, targetIRI, TestHelper.getUpdateRequest());
		
		FileBasedStoreClient kbClient = new FileBasedStoreClient(filePath);
		JSONObject result = kbClient.executeQuery(query).getJSONObject(0);; 
		assertEquals("TEST",result.get("o").toString());   
	}
	
	///////////////////////////////////////////////////////////////
	//Helper methods
	///////////////////////////////////////////////////////////////	
	
	/**
	 * Deletes file
	 * @param path
	 */
	private void deleteFile(String path) {
		//delete file if it already exists
		File f= new File(path);
		if ( f.exists() ) {	f.delete();}
        assertFalse(f.exists());
	}
}