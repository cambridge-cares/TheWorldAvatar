package uk.ac.cam.cares.jps.agent.caresWeatherStation;

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
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;

public class CARESWeatherStationInputAgentLauncherTest {

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
    public void testMainNoArgs() {
        String[] args = {};
        try {
            CARESWeatherStationInputAgentLauncher.initializeAgent(args);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Need three properties files in the following order: 1) input agent 2) time series client 3) API connector.",
                    e.getMessage());
        }
    }

    @Test
    public void testMainInvalidAgentPropertyFile() {
        // Empty agent properties file should result in an error
        try {
            CARESWeatherStationInputAgentLauncher.initializeAgent(args);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("The CARESWeatherStation input agent could not be constructed!", e.getMessage());
        }
    }

    @Test
    public void testMainErrorWhenCreatingAPIConnector() throws IOException {
        createProperClientPropertiesFile();
        // Use a mock for the input agent
        try(MockedConstruction<CARESWeatherStationInputAgent> mockAgent = Mockito.mockConstruction(CARESWeatherStationInputAgent.class)) {
            // Empty API properties file should result in an exception
            try {
                CARESWeatherStationInputAgentLauncher.initializeAgent(args);
                Assert.fail();
            }
            catch (JPSRuntimeException e) {
                // Ensure that the method to set the time series client was invoked once
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).setTsClient(Mockito.any());
                // Ensure that the initialization was invoked once
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).initializeTimeSeriesIfNotExist();
                Assert.assertEquals("Could not construct the CARES weather station API connector needed to interact with the API!", e.getMessage());
            }
        }

    }

    @Test
    public void testMainErrorWhenRetrievingReadings() throws IOException {
        createProperClientPropertiesFile();
        createProperAPIPropertiesFile();
        // Use a mock for the input agent
        try(MockedConstruction<CARESWeatherStationInputAgent> ignored = Mockito.mockConstruction(CARESWeatherStationInputAgent.class)) {
            // Use a mock for the connector that throws an exception when readings are requested
            try(MockedConstruction<CARESWeatherStationAPIConnector> mockConnector = Mockito.mockConstruction(CARESWeatherStationAPIConnector.class,
                    (mock, context) -> Mockito.when(mock.getWeatherReadings()).thenThrow(new JPSRuntimeException("exception")))) {
                try {
                    CARESWeatherStationInputAgentLauncher.initializeAgent(args);
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
        String[] keys = {"temp","dewpt","heatIndex","windChill","windGust","windSpeed","pressure","precipRate", "precipTotal" ,"elev"};
        String[] timestamps = {"2022-07-11T16:10:00Z", "2022-07-11T16:15:00Z", "2022-07-11T16:20:00Z", "2022-07-11T16:25:00Z"};
        JSONObject readings = new JSONObject();
        JSONArray jsArr= new JSONArray();
        double value=0.0;
        for(int i=0; i<timestamps.length;i++) {
            JSONObject currentWeatherData = new JSONObject();
            currentWeatherData.put(CARESWeatherStationInputAgent.timestampKey, timestamps[i]);
            JSONObject measurements = new JSONObject();
            for(String key: keys) {
                measurements.put(key, value);
            }
            currentWeatherData.put("metric_si",measurements);
            jsArr.put(i,currentWeatherData);
            value++;
        }
        readings.put("observations",jsArr);

        // Use a mock for the input agent
        try(MockedConstruction<CARESWeatherStationInputAgent> mockAgent = Mockito.mockConstruction(CARESWeatherStationInputAgent.class)) {
            // Use a mock for SparqlHandler
            try(MockedConstruction<WeatherQueryClient> mockQueryClient = Mockito.mockConstruction(WeatherQueryClient.class)) {
                // Use a mock for the connector that returns the dummy readings
                try(MockedConstruction<CARESWeatherStationAPIConnector> ignored = Mockito.mockConstruction(CARESWeatherStationAPIConnector.class,
                (mock, context) -> {
                    Mockito.when(mock.getWeatherReadings()).thenReturn(readings);
                })) {
                    CARESWeatherStationInputAgentLauncher.initializeAgent(args);
                    // Ensure that the update of the agent was invoked
                    Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).updateData(readings);
                }
            }
        }
    }

    private void createProperAgentPropertiesFile() throws IOException {
        // Create a properties file that points to the example/test mapping folder in the resources //
        // Create mappings folder
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), agentPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("caresWeatherStation.mappingfolder=TEST_MAPPINGS");
        }
    }

    private void createProperClientPropertiesFile() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), clientPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("db.url=jdbc:postgresql:weatherTimeseries\n");
            writer.write("db.user=postgres\n");
            writer.write("db.password=postgres\n");
            writer.write("sparql.query.endpoint=http://localhost:9999/blazegraph/namespace/kb/sparql\n");
            writer.write("sparql.update.endpoint=http://localhost:9999/blazegraph/namespace/kb/sparql\n");
        }
    }

    private void createProperAPIPropertiesFile() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), apiPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("weather.api_key=16bfghijk8910111213145nni99b897r\n");
            writer.write("weather.stationId=IABC2368\n");
            writer.write("weather.api_url=http://localhost:8080/");

        }
    }
}