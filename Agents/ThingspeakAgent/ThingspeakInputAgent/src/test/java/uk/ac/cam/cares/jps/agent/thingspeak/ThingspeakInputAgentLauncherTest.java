package uk.ac.cam.cares.jps.agent.thingspeak;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

import com.github.stefanbirkner.systemlambda.Statement;
import com.github.stefanbirkner.systemlambda.SystemLambda;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;


import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

public class ThingspeakInputAgentLauncherTest {

	
	private static final Logger LOGGER = LogManager.getLogger(ThingspeakInputAgentLauncherTest.class);
	
    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    // Name of the properties files
    private final String agentPropertiesFilename = "agent.properties";
    private final String clientPropertiesFilename = "client.properties";
    private final String apiPropertiesFilename = "api.properties";
    // Argument array used with the main function containing all the paths to the property files as string
    private String[] args;

    @Before
    public void initializePropertyFile() throws IOException {
        File agentPropertyFile= folder.newFile(agentPropertiesFilename);
        File clientPropertyFile= folder.newFile(clientPropertiesFilename);
        File apiPropertyFile= folder.newFile(apiPropertiesFilename);
        // Paths to the three different property files
       
        String agentPropertiesFile = agentPropertyFile.getCanonicalPath();
        String clientPropertiesFile = clientPropertyFile.getCanonicalPath();
        String apiPropertiesFile = apiPropertyFile.getCanonicalPath();
        args = new String[] {agentPropertiesFile, clientPropertiesFile, apiPropertiesFile};
        
        


    }
    
    @Test
    public void testProcessRequestParams() throws IOException {
    	ThingspeakInputAgentLauncher testLauncher = new ThingspeakInputAgentLauncher();
    	//test empty requestparams
    	JSONObject testRequestParams = new JSONObject();
    	JSONObject testMessage = testLauncher.processRequestParameters(testRequestParams);
    Assert.assertEquals(testMessage.get("Result"), "Request parameters are not defined correctly.");
       //test non-empty requestParams but with incorrect keys
    
    testRequestParams.put("ageProperties", "TEST_AGENTPROPERTIES");
    testRequestParams.put("apiProperties", "TEST_APIPROPERTIES");
    testRequestParams.put("clientProperties", "TEST_CLIENTPROPERTIES");
   
    testMessage = testLauncher.processRequestParameters(testRequestParams);
    Assert.assertEquals(testMessage.get("Result"), "Request parameters are not defined correctly.");
    
    //test invalid environment variables in requestParams
    testRequestParams.remove("ageProperties");
  //invalid environment variables TEST_AGENTPROPERTIE should cause validateInput to return back false and processRequestParameters to
    //return back the jsonMessage {"Result":"Request parameters are not defined correctly."}
    testRequestParams.put("agentProperties", "TEST_AGENTPROPERTIE");
    testRequestParams.put("apiProperties", "TEST_APIPROPERTIES");
    testRequestParams.put("clientProperties", "TEST_CLIENTPROPERTIES");
    
    String folderName = "mappings";
    File mappingFolder = folder.newFolder(folderName);
    // Create empty file in mappings folder
    File mappingFile = new File(Paths.get(mappingFolder.getCanonicalPath(), "allTypes.properties").toString());
    Assert.assertTrue(mappingFile.createNewFile());
    //try and catch is required to use SystemLambda to mock environment variables
    
    try {
    	SystemLambda.withEnvironmentVariable("TEST_AGENTPROPERTIES", mappingFolder.getCanonicalPath()).execute((Statement) () -> {
    		JSONObject testMessage01 = testLauncher.processRequestParameters(testRequestParams);
    		Assert.assertEquals(testMessage01.get("Result"), "Request parameters are not defined correctly.");
    		});
		} catch (Exception e) {
		//no Exception should be thrown here
		}
    }
    
    @Test
    public void testMainNoArgs() {
    	ThingspeakInputAgentLauncher testLauncher = new ThingspeakInputAgentLauncher();
        String[] args = {};
        try {
            testLauncher.initializeAgent(args);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Need three properties files in the following order: 1) input agent 2) time series client 3) API connector.",
                    e.getMessage());
        }
    }

    @Test
    public void testMainInvalidAgentPropertyFile() {
    	ThingspeakInputAgentLauncher testLauncher = new ThingspeakInputAgentLauncher();
        // Empty agent properties file should result in an error
        try {
            testLauncher.initializeAgent(args);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("The Thingspeak input agent could not be constructed!", e.getMessage());
        }
    }

    @Test
    public void testMainErrorWhenCreatingAPIConnector() throws IOException, SQLException {
    	ThingspeakInputAgentLauncher testLauncher = new ThingspeakInputAgentLauncher();
        createProperClientPropertiesFile();
        // Use a mock for the input agent
        try(MockedConstruction<ThingspeakInputAgent> mockAgent = Mockito.mockConstruction(ThingspeakInputAgent.class)) {
            // Empty API properties file should result in an exception
            try {
                testLauncher.initializeAgent(args);
                Assert.fail();
            }
            catch (JPSRuntimeException e) {
                // Ensure that the method to set the time series client was invoked once
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).setTsClient(Mockito.any());
                // Ensure that the initialization was invoked once
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).initializeTimeSeriesIfNotExist();
                Assert.assertEquals("Could not construct the Thingspeak API connector needed to interact with the API!", e.getMessage());
            }
        }

    }

    @Test
    public void testMainErrorWhenRetrievingReadings() throws IOException {
    	ThingspeakInputAgentLauncher testLauncher = new ThingspeakInputAgentLauncher();
        createProperClientPropertiesFile();
        createProperAPIPropertiesFile();
        // Use a mock for the input agent
        try(MockedConstruction<ThingspeakInputAgent> ignored = Mockito.mockConstruction(ThingspeakInputAgent.class)) {
            // Use a mock for the connector that throws an exception when readings are requested
            try(MockedConstruction<ThingspeakAPIConnector> mockConnector = Mockito.mockConstruction(ThingspeakAPIConnector.class,
                    (mock, context) -> Mockito.when(mock.getAllReadings()).thenThrow(new JPSRuntimeException("exception")))) {
                try {
                    testLauncher.initializeAgent(args);
                    Assert.fail();
                }
                catch (JPSRuntimeException e) {
                    // Ensure that the connect method was invoked once
                    Mockito.verify(mockConnector.constructed().get(0), Mockito.times(1)).getAllReadings();
                    Assert.assertEquals("Some readings could not be retrieved.", e.getMessage());
                    Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
                    Assert.assertEquals("exception", e.getCause().getMessage());
                }
            }
        }
    }


    @Test
    public void testReadingsNotEmpty() throws IOException {
    	ThingspeakInputAgentLauncher testLauncher = new ThingspeakInputAgentLauncher();
        createProperClientPropertiesFile();
        createProperAPIPropertiesFile();
        // Create dummy readings to return
        JSONObject responseBody = new JSONObject();
        JSONObject jsonobject1 = new JSONObject();
        JSONObject jsonobject2 = new JSONObject();
        JSONArray jsonarray1 = new JSONArray();
        
        jsonobject1.put("created_at", "2022-11-09T03:05:18Z");
        jsonobject1.put("entry_id", 59267);
        jsonobject1.put("field1", "621");
        
        jsonarray1.put(jsonobject1);
        
        jsonobject2.put("id", 1876219);
        jsonobject2.put("name", "co2");
        jsonobject2.put("description", "co2 test sensor");
        jsonobject2.put("latitude", "0.0");
        jsonobject2.put("longitude","0.0");
        jsonobject2.put("field1", "ppm");
        jsonobject2.put("created_at", "2022-09-28T15:29:24Z");
        jsonobject2.put("updated_at", "2022-09-28T17:00:37Z");
        jsonobject2.put("last_entry_id", 59267);
        
        responseBody.put("channel", jsonobject2);
        responseBody.put("feeds", jsonarray1);
        // Use a mock for the input agent
        try(MockedConstruction<ThingspeakInputAgent> mockAgent = Mockito.mockConstruction(ThingspeakInputAgent.class)) {
            // Use a mock for the connector that returns the dummy readings
            try(MockedConstruction<ThingspeakAPIConnector> ignored = Mockito.mockConstruction(ThingspeakAPIConnector.class,
                    (mock, context) -> {
                        Mockito.when(mock.getAllReadings()).thenReturn(responseBody);
                    })) {
                testLauncher.initializeAgent(args);
                // Ensure that the update of the agent was invoked
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).updateData(responseBody);
            }
        }
    }

    private void createProperAgentPropertiesFile() throws IOException {
        // Create a properties file that points to the example/test mapping folder in the resources //
        // Create mappings folder
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), agentPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("thingspeak.mappingfolder=TEST_MAPPINGS");
        }
    }

    private void createProperClientPropertiesFile() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), clientPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("db.url=jdbc:postgresql:timeseries\n");
            writer.write("db.user=postgres\n");
            writer.write("db.password=cares1010\n");
            writer.write("sparql.query.endpoint=http://localhost:9999/blazegraph/namespace/kb/sparql\n");
            writer.write("sparql.update.endpoint=http://localhost:9999/blazegraph/namespace/kb/sparql\n");
        }
    }

    private void createProperAPIPropertiesFile() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), apiPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("thingspeak.channelNumber=12345\n");
            writer.write("thingspeak.apiKey=testapikey\n");
            writer.write("thingspeak.results=1\n");
            writer.write("path.url=https://api.thingspeak.com/channels/\n");
           
        }
    }
    
    @Test
    public void loadTSClientConfigsTest() throws NoSuchMethodException, IllegalAccessException, IOException, NoSuchFieldException {
    	ThingspeakInputAgentLauncher testLauncher = new ThingspeakInputAgentLauncher();
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
        Method loadTSClientConfig = ThingspeakInputAgentLauncher.class.getDeclaredMethod("loadTSClientConfigs", String.class);
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
        // Try loading RDB configs
        try {
            loadTSClientConfig.invoke(testLauncher, filepath);
            Assert.fail();
        } catch (InvocationTargetException e) {
            Assert.assertEquals(IOException.class, e.getCause().getClass());
            Assert.assertEquals(noSparqlQueryEndpoint, e.getCause().getMessage());
        }
        
        // Test for missing sparql update endpoint
        writePropertyFile(filepath, Arrays.asList("db.url=test", "db.user=postgres", "db.password=postgres", "sparql.query.endpoint=test"));
        // Try loading RDB configs
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
