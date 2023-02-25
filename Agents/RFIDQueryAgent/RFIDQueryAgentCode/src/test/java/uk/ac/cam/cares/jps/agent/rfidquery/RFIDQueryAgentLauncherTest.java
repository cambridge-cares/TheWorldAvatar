package uk.ac.cam.cares.jps.agent.rfidquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
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

    @Before
    public void initializePropertyFile() throws IOException {
        File clientPropertyFile= folder.newFile(clientPropertiesFilename);
        // Paths to the three different property files
       
        String clientPropertiesFile = clientPropertyFile.getCanonicalPath();
        args = new String[] {clientPropertiesFile, "some_data_iri", "10", "true", clientPropertiesFile};
    }
    
    @Test
    public void testProcessRequestParams() throws IOException {
        RFIDQueryAgentLauncher testLauncher = new RFIDQueryAgentLauncher();
    	//test empty requestparams
    	JSONObject testRequestParams = new JSONObject();
        JSONObject testMessage = new JSONObject();
        try {
    	testMessage = testLauncher.processRequestParameters(testRequestParams);
        }
        catch (JPSRuntimeException e){
            Assert.assertEquals("Unable to validate request sent to the agent!", e.getMessage());
        }
        //test non-empty requestParams but with incorrect keys
        testRequestParams.put("dataIR", "some_data_IRI");
        testRequestParams.put("hours", "10");
        testRequestParams.put("timeSeriesClientProperties", "TEST_CLIENTPROPERTIES");
        testRequestParams.put("speciesProperties","TEST_CLIENTPROPERTIES");
        testRequestParams.put("containSpecies","true");
        try {
        testMessage = testLauncher.processRequestParameters(testRequestParams);
        }
        catch (JPSRuntimeException e){
            Assert.assertEquals("Unable to validate request sent to the agent!", e.getMessage());
        }
        //test invalid environment variables in requestParams
        testRequestParams.remove("dataIR");
        testRequestParams.remove("timeSeriesClientProperties");
        //invalid environment variables TEST_CLIENTPROPERT should cause validateInput to return back false and processRequestParameters to
        //return back the jsonMessage {"Result":"Request parameters are not defined correctly."}
        testRequestParams.put("dataIRIs", "some_data_IRI");
        testRequestParams.put("timeSeriesClientProperties", "TEST_CLIENTPROPERT");
        String folderName = "configs";
        File Folder = folder.newFolder(folderName);

        // Create empty file in mappings folder
        File File = new File(Paths.get(Folder.getCanonicalPath(), "client.properties").toString());
        Assert.assertTrue(File.createNewFile());
        
        //try and catch is required to use SystemLambda to mock environment variables
        //mock TEST_CLIENTPROPERTIES to point to temporary client.properties file
        try {
            SystemLambda.withEnvironmentVariable("TEST_CLIENTPROPERTIES", Folder.getCanonicalPath()).execute((Statement) () -> {
                JSONObject testMessage01 = testLauncher.processRequestParameters(testRequestParams);
            });
        } catch (Exception e) {
            Assert.assertEquals("Unable to validate request sent to the agent!", e.getMessage());
        }
    }
    
    @Test
    public void testMainNoArgs() {
        RFIDQueryAgentLauncher testLauncher = new RFIDQueryAgentLauncher();
        String[] args = {};
        try {
            testLauncher.initializeAgent(args);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Need five arguments in the following order:1) time series client for timeseries data 2)list of data IRIs 3)Number of hours 4) species sparql endpoints 5)whether tagged object contains some chemical (true or false)", e.getMessage());
        }
    }

    @Test
    public void testMainErrorWhenQueryingForStatusAndCheckingTimeStamps() throws IOException {
    	RFIDQueryAgentLauncher testLauncher = new RFIDQueryAgentLauncher();
        createProperClientPropertiesFile();
        // Use a mock for the agent that throws an exception when attempting to query the knowledge graph
        try(MockedConstruction<RFIDQueryAgent> mockAgent =  Mockito.mockConstruction(RFIDQueryAgent.class,
        (mock, context) -> Mockito.when(mock.queriesStatusAndCheckTimeStamps()).thenThrow(new JPSRuntimeException("exception")))) {
                try {
                    testLauncher.initializeAgent(args);
                    Assert.fail();
                }
                catch (JPSRuntimeException e) {
                    Assert.assertEquals("Unable to query for latest data and/or check RFID Status Threshold!", e.getMessage());
                    Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
                }
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
