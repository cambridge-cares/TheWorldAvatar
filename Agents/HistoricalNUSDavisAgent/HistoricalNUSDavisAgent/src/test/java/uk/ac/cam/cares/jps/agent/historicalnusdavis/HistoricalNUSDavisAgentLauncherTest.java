package uk.ac.cam.cares.jps.agent.historicalnusdavis;

import com.github.stefanbirkner.systemlambda.Statement;
import com.github.stefanbirkner.systemlambda.SystemLambda;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;


public class HistoricalNUSDavisAgentLauncherTest {
    private static final Logger LOGGER = LogManager.getLogger(HistoricalNUSDavisAgentLauncherTest.class);

    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    // Name of the properties files
    private final String agentPropertiesFilename = "agent.properties";
    private final String clientPropertiesFilename = "client.properties";
    private final String connectorPropertiesFilename = "xlsxconnector.properties";
    // Argument array used with the main function containing all the paths to the property files as string
    private String[] args;

    @Before
    public void initializePropertyFile() throws IOException {
        File agentPropertyFile= folder.newFile(agentPropertiesFilename);
        File clientPropertyFile= folder.newFile(clientPropertiesFilename);
        File apiPropertyFile= folder.newFile(connectorPropertiesFilename);
        // Paths to the three different property files

        String agentPropertiesFile = agentPropertyFile.getCanonicalPath();
        String clientPropertiesFile = clientPropertyFile.getCanonicalPath();
        String apiPropertiesFile = apiPropertyFile.getCanonicalPath();
        args = new String[] {agentPropertiesFile, clientPropertiesFile, apiPropertiesFile};

    }

    @Test
    public void testProcessRequestParams() throws IOException {
        HistoricalNUSDavisAgentLauncher testLauncher = new HistoricalNUSDavisAgentLauncher();
        //test empty requestparams
        JSONObject testRequestParams = new JSONObject();
        JSONObject testMessage = testLauncher.processRequestParameters(testRequestParams);
        Assert.assertEquals(testMessage.get("Result"), "Request parameters are not defined correctly.");
        //test non-empty requestParams but with incorrect keys

        testRequestParams.put("ageProperties", "TEST_AGENTPROPERTIES");
        testRequestParams.put("connectorProperties", "TEST_CONNECTORPROPERTIES");
        testRequestParams.put("clientProperties", "TEST_CLIENTPROPERTIES");

        testMessage = testLauncher.processRequestParameters(testRequestParams);
        Assert.assertEquals(testMessage.get("Result"), "Request parameters are not defined correctly.");

        //test invalid environment variables in requestParams
        testRequestParams.remove("ageProperties");
        testRequestParams.put("agentProperties", "TEST_AGENTPROPERTIE");
        testRequestParams.put("connectorProperties", "TEST_CONNECTORPROPERTIES");
        testRequestParams.put("clientProperties", "TEST_CLIENTPROPERTIES");

        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Create empty file in mappings folder
        File mappingFile = new File(Paths.get(mappingFolder.getCanonicalPath(), "weather.properties").toString());
        Assert.assertTrue(mappingFile.createNewFile());
        //try and catch is required to use SystemLambda to mock environment variables
        //invalid environment variables TEST_AGENTPROPERTIE should cause validateInput to return back false and processRequestParameters to
        //return back the jsonMessage {"Result":"Request parameters are not defined correctly."}
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
        String[] args = {};
        try {
            HistoricalNUSDavisAgentLauncher.initializeAgent(args);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Need three properties files in the following order: 1) input agent 2) time series client 3) xlsx connector.",
                    e.getMessage());
        }
    }

    @Test
    public void testMainInvalidAgentPropertyFile() {
        // Empty agent properties file should result in an error
        try {
            HistoricalNUSDavisAgentLauncher.initializeAgent(args);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("The historical NUS Davis agent could not be constructed!", e.getMessage());
        }
    }

    @Test
    public void testMainErrorWhenCreatingTSClient() throws IOException {
        //create agent properties file
        createProperAgentPropertiesFile();
        //File testFile=new File(Paths.get(args[0],"agent.properties").toString());
        //Assert.assertTrue(testFile.exists());
        //Create folder with mapping file
        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Create empty file in mappings folder
        File mappingFile = new File(Paths.get(mappingFolder.getCanonicalPath(), "weather.properties").toString());
        Assert.assertTrue(mappingFile.createNewFile());
        // Empty properties file for time series client should result in exception
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                HistoricalNUSDavisAgentLauncher.initializeAgent(args);
            });
        }
        catch (Exception e) {
            Assert.assertEquals("Could not construct the time series client needed by the input agent!", e.getMessage());
        }

    }

    @Test
    public void testMainErrorWhenCreatingAPIConnector() throws IOException {
        createProperClientPropertiesFile();
        // Use a mock for the input agent
        try(MockedConstruction<HistoricalNUSDavisAgent> mockAgent = Mockito.mockConstruction(HistoricalNUSDavisAgent.class)) {
            // Empty API properties file should result in an exception
            try {
                HistoricalNUSDavisAgentLauncher.initializeAgent(args);
                Assert.fail();
            }
            catch (JPSRuntimeException e) {
                // Ensure that the method to set the time series client was invoked once
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).setTsClient(Mockito.any());
                // Ensure that the initialization was invoked once
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).initializeTimeSeriesIfNotExist();
                Assert.assertEquals("Could not construct the NUS Davis weather station xlsx connector needed to interact with the excel file!", e.getMessage());
            }
        }

    }

    @Test
    public void testMainErrorWhenRetrievingReadings() throws IOException {
        createProperClientPropertiesFile();
        createProperAPIPropertiesFile();
        // Use a mock for the input agent
        try(MockedConstruction<HistoricalNUSDavisAgent> ignored = Mockito.mockConstruction(HistoricalNUSDavisAgent.class)) {
            // Use a mock for the connector that throws an exception when readings are requested
            try(MockedConstruction<HistoricalNUSDavisXLSXConnector> mockConnector = Mockito.mockConstruction(HistoricalNUSDavisXLSXConnector.class,
                    (mock, context) -> Mockito.when(mock.getReadings()).thenThrow(new JPSRuntimeException("exception")))) {
                try {
                    HistoricalNUSDavisAgentLauncher.initializeAgent(args);
                    Assert.fail();
                }
                catch (JPSRuntimeException e) {
                    Assert.assertEquals("Some readings could not be retrieved.", e.getMessage());
                    Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
                    Assert.assertEquals("exception", e.getCause().getMessage());
                }
            }
        }
    }


    @Test
    public void testReadingsNotEmpty() throws IOException {
        createProperClientPropertiesFile();
        createProperAPIPropertiesFile();
        // Create dummy readings to return
        String[] keys = {"temp_in","dew_point","heat_index","wind_chill","bar","hum_in","pressure","solar_rad", "rain_day_mm" ,"wind_dir"};
        String[] timestamps = {"1558729481"};
        double value =0.0;
        JSONObject readings = new JSONObject();
        JSONArray sensors= new JSONArray();
        JSONArray data= new JSONArray();
        JSONObject jsObj1= new JSONObject();

        readings.put("stationId",12345);
        jsObj1.put( "lsid",396862);
        jsObj1.put("sensor_type",50);
        jsObj1.put("data_structure_type",2);

        for(int i=0; i<timestamps.length;i++) {
            JSONObject measurements = new JSONObject();
            measurements.put(HistoricalNUSDavisAgent.timestampKey,timestamps[i]);
            for(String key: keys) {
                measurements.put(key, value);
            }
            data.put(measurements);
            value++;
        }
        jsObj1.put("data",data);
        sensors.put(jsObj1);
        readings.put("sensors",sensors);

        // Use a mock for the input agent
        try(MockedConstruction<HistoricalNUSDavisAgent> mockAgent = Mockito.mockConstruction(HistoricalNUSDavisAgent.class)) {
            // Use a mock for the connector that returns the dummy readings
            try(MockedConstruction<HistoricalNUSDavisXLSXConnector> ignored = Mockito.mockConstruction(HistoricalNUSDavisXLSXConnector.class,
                    (mock, context) -> {
                        Mockito.when(mock.getReadings()).thenReturn(readings);
                    })) {
                HistoricalNUSDavisAgentLauncher.initializeAgent(args);
                // Ensure that the update of the agent was invoked
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).updateData(readings);
            }
        }
    }

    private void createProperAgentPropertiesFile() throws IOException {
        // Create a properties file that points to the example/test mapping folder in the resources //
        // Create mappings folder
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), agentPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("nusDavisWeatherStation.mappingfolder=TEST_MAPPINGS");
        }
    }

    private void createProperClientPropertiesFile() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), clientPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("db.url=jdbc:postgresql:nusWeatherTimeseries\n");
            writer.write("db.user=postgres\n");
            writer.write("db.password=postgres\n");
            writer.write("sparql.query.endpoint=http://localhost:9999/blazegraph/namespace/kb/sparql\n");
            writer.write("sparql.update.endpoint=http://localhost:9999/blazegraph/namespace/kb/sparql\n");
        }
    }

    private void createProperAPIPropertiesFile() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), connectorPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("numOfKeys=18\n");
        }
    }
}
