package uk.ac.cam.cares.jps.agent.rfidquery;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class RFIDQueryAgentLauncherTest {
	
    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    // Name of the properties files
    private final String clientPropertiesFilename = "client.properties";
    // Argument array used with the main function containing all necessary arguments
    private String[] args;
    String clientPropertiesFile;
    JSONObject testRequestParams;

    @Before
    public void initializePropertyFileAndCreateMockParams() throws IOException {
        File clientPropertyFile= folder.newFile(clientPropertiesFilename);
        testRequestParams = new JSONObject();
        testRequestParams.put("dataIRIs", "some_data_IRI");
        testRequestParams.put("hours", "10");
        testRequestParams.put("timeSeriesClientProperties", "TEST_CLIENTPROPERTIES");
        testRequestParams.put("speciesProperties","TEST_CLIENTPROPERTIES");
        // Paths to the three different property files
        clientPropertiesFile = clientPropertyFile.getCanonicalPath();
        args = new String[] {clientPropertiesFile, "some_data_iri", "10", "true", clientPropertiesFile};
    }
    
    @Test
    public void testInitializeAgentNoRequestParams() {
        RFIDQueryAgentLauncher testLauncher = new RFIDQueryAgentLauncher();
        JSONObject empty = new JSONObject();
        try {
            testLauncher.initializeAgent(empty, "/check");
        }
        catch (JSONException e) {
            Assert.assertEquals("missing input parameters!", e.getMessage());
        }
        try {
            testLauncher.initializeAgent(empty, "/retrieveData");
        }
        catch (JSONException e) {
            Assert.assertEquals("missing input parameters!", e.getMessage());
        }
        try {
            testLauncher.initializeAgent(empty, "/sendNotification");
        }
        catch (JSONException e) {
            Assert.assertEquals("missing input parameters!", e.getMessage());
        }
    }  
    
    @Test
    public void loadTSClientConfigsTest() throws NoSuchMethodException, IllegalAccessException, IOException, NoSuchFieldException {
    	RFIDQueryAgentLauncher testLauncher = new RFIDQueryAgentLauncher();
        // Filepath to not yet created file in temporary test folder
    	String filepath = "";
        // Error messages
        String fileNotFound = "No properties file found at specified filepath: " + filepath;
        String noURL = "Properties file is missing \"db.url=<db_url>\"";
        String noUser = "Properties file is missing \"db.user=<db_user>\"";
        String noPassword = "Properties file is missing \"db.password=<db_password>\"";
        String noSparqlQueryEndpoint = "Properties file is missing \"sparql.query.endpoint=<sparql_query_endpoint>\"";
        String noSparqlUpdateEndpoint = "Properties file is missing \"sparql.update.endpoint=<sparql_update_endpoint>\"";
        // Set private method to be accessible
        Method loadTSClientConfig = RFIDQueryAgentLauncher.class.getDeclaredMethod("loadTSClientConfigs", String.class);
        loadTSClientConfig.setAccessible(true);

        // Test for non-existing properties file
        try {
            loadTSClientConfig.invoke(testLauncher, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(FileNotFoundException.class, e.getCause().getClass());
            Assert.assertEquals(fileNotFound, e.getCause().getMessage());
        }
        filepath = Paths.get(folder.getRoot().toString(), "client.properties").toString();

        // Test for missing db url
        writePropertyFile(filepath, Arrays.asList("db.user=postgres", "db.password=postgres", "sparql.query.endpoint=test", "sparql.update.endpoint=test"));
        // Try loading config
        try {
            loadTSClientConfig.invoke(testLauncher, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noURL, e.getCause().getMessage());
        }

        // Test for missing db user
        writePropertyFile(filepath, Arrays.asList("db.url=test", "db.password=postgres", "sparql.query.endpoint=test", "sparql.update.endpoint=test"));
        // Try loading configs
        try {
            loadTSClientConfig.invoke(testLauncher, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noUser, e.getCause().getMessage());
        }
        
        // Test for missing db password
        writePropertyFile(filepath, Arrays.asList("db.url=test", "db.user=postgres", "sparql.query.endpoint=test", "sparql.update.endpoint=test"));
        // Try loading configs
        try {
            loadTSClientConfig.invoke(testLauncher, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noPassword, e.getCause().getMessage());
        }
        
        // Test for missing sparql query endpoint
        writePropertyFile(filepath, Arrays.asList("db.url=test", "db.user=postgres", "db.password=postgres", "sparql.update.endpoint=test"));
        // Try loading configs
        try {
            loadTSClientConfig.invoke(testLauncher, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noSparqlQueryEndpoint, e.getCause().getMessage());
        }
        
        // Test for missing sparql update endpoint
        writePropertyFile(filepath, Arrays.asList("db.url=test", "db.user=postgres", "db.password=postgres", "sparql.query.endpoint=test"));
        // Try loading configs
        try {
            loadTSClientConfig.invoke(testLauncher, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noSparqlUpdateEndpoint, e.getCause().getMessage());
        }
    }
    
    private void writePropertyFile(String filepath, List<String> properties) throws IOException {
        // Overwrite potentially existing properties file
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        for (String s : properties) {
            writer.write(s + "\n");
        }
        // Close the file and return the file
        writer.close();
    }
}
