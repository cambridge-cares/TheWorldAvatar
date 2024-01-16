package uk.ac.cam.cares.jps.agent.historicalpirmasens;

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


public class HistoricalPirmasensStationAgentLauncherTest {
    private static final Logger LOGGER = LogManager.getLogger(HistoricalPirmasensStationAgentLauncherTest.class);

    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    // Name of the properties files
    private final String agentPropertiesFilename = "agent.properties";
    private final String clientPropertiesFilename = "client.properties";
    private final String connectorPropertiesFilename = "csvconnector.properties";
    // Argument array used with the main function containing all the paths to the property files as string
    private String[] args;

    @Before
    public void initializePropertyFile() throws IOException {
        File agentPropertyFile= folder.newFile(agentPropertiesFilename);
        File clientPropertyFile= folder.newFile(clientPropertiesFilename);
        File connectorPropertyFile= folder.newFile(connectorPropertiesFilename);
        // Paths to the three different property files

        String agentPropertiesFile = agentPropertyFile.getCanonicalPath();
        String clientPropertiesFile = clientPropertyFile.getCanonicalPath();
        String connectorPropertiesFile = connectorPropertyFile.getCanonicalPath();
        args = new String[] {agentPropertiesFile, clientPropertiesFile, connectorPropertiesFile};

    }

    @Test
    public void testProcessRequestParams() throws IOException {
        HistoricalPirmasensStationAgentLauncher testLauncher = new HistoricalPirmasensStationAgentLauncher();
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
            HistoricalPirmasensStationAgentLauncher.initializeAgent(args);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Need three properties files in the following order: 1) input agent 2) time series client 3) csv connector.",
                    e.getMessage());
        }
    }

    @Test
    public void testMainInvalidAgentPropertyFile() {
        // Empty agent properties file should result in an error
        try {
            HistoricalPirmasensStationAgentLauncher.initializeAgent(args);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("The historical pirmasens station agent could not be constructed!", e.getMessage());
        }
    }

    @Test
    public void testMainErrorWhenCreatingTSClient() throws IOException {
        //create agent properties file
        createProperAgentPropertiesFile();
        //Create folder with mapping file
        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Create empty file in mappings folder
        File mappingFile = new File(Paths.get(mappingFolder.getCanonicalPath(), "weather.properties").toString());
        Assert.assertTrue(mappingFile.createNewFile());
        // Empty properties file for time series client should result in exception
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                HistoricalPirmasensStationAgentLauncher.initializeAgent(args);
            });
        }
        catch (Exception e) {
            Assert.assertEquals("Could not construct the time series client needed by the historical agent!", e.getMessage());
        }

    }

    @Test
    public void testMainErrorWhenCreatingAPIConnector() throws IOException {
        createProperClientPropertiesFile();
        // Use a mock for the input agent
        try(MockedConstruction<HistoricalPirmasensStationAgent> mockAgent = Mockito.mockConstruction(HistoricalPirmasensStationAgent.class)) {
            // Empty API properties file should result in an exception
            try {
                HistoricalPirmasensStationAgentLauncher.initializeAgent(args);
                Assert.fail();
            }
            catch (JPSRuntimeException e) {
                // Ensure that the method to set the time series client was invoked once
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).setTsClient(Mockito.any());
                // Ensure that the initialization was invoked once
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).initializeTimeSeriesIfNotExist();
                Assert.assertEquals("Could not construct the historical pirmasens station csv connector needed to interact with the CSV file!", e.getMessage());
            }
        }

    }

    @Test
    public void testMainErrorWhenRetrievingReadings() throws IOException {
        createProperClientPropertiesFile();
        createProperAPIPropertiesFile();
        // Use a mock for the input agent
        try(MockedConstruction<HistoricalPirmasensStationAgent> ignored = Mockito.mockConstruction(HistoricalPirmasensStationAgent.class)) {
            // Use a mock for the connector that throws an exception when readings are requested
            try(MockedConstruction<HistoricalPirmasensStationCSVConnector> mockConnector = Mockito.mockConstruction(HistoricalPirmasensStationCSVConnector.class,
                    (mock, context) -> Mockito.when(mock.getReadings()).thenThrow(new JPSRuntimeException("exception")))) {
                try {
                    HistoricalPirmasensStationAgentLauncher.initializeAgent(args);
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
        String[] keys = {"Niederschlag","Ozon","eBC_PM2-5","Gesamt_UV_Strahlung","Stickstoffdioxid","Relative_Feuchte","Luftdruck", "Temperatur" ,"Windrichtung_rohwert", "PM10", "Stickstoffmonoxid", "Windgeschwindigkeit_rohwert"};
        String[] timestamps = {"2021-07-11T16:10:00", "2021-07-11T16:15:00", "2021-07-11T16:20:00", "2021-07-11T16:25:00"};
        double value =0.0;
        JSONObject readings = new JSONObject();
        JSONArray sensors= new JSONArray();
        JSONArray data= new JSONArray();
        JSONObject jsObj1= new JSONObject();

        for(int i=0; i<timestamps.length;i++) {
            JSONObject measurements = new JSONObject();
            measurements.put(HistoricalPirmasensStationAgent.timestampKey,timestamps[i]);
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
        try(MockedConstruction<HistoricalPirmasensStationAgent> mockAgent = Mockito.mockConstruction(HistoricalPirmasensStationAgent.class)) {
            // Use a mock for the connector that returns the dummy readings
            try(MockedConstruction<HistoricalPirmasensStationCSVConnector> ignored = Mockito.mockConstruction(HistoricalPirmasensStationCSVConnector.class,
                    (mock, context) -> {
                        Mockito.when(mock.getReadings()).thenReturn(readings);
                    })) {
                HistoricalPirmasensStationAgentLauncher.initializeAgent(args);
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
            writer.write("pirmasensStation.mappingfolder=TEST_MAPPINGS");
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
            writer.write("numOfKeys=12\n");
        }
    }
}
