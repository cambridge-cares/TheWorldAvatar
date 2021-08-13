package uk.ac.cam.cares.jps.agent.aqmesh;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.Objects;

public class AQMeshInputAgentLauncherTest {

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
            AQMeshInputAgentLauncher.main(args);
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
            AQMeshInputAgentLauncher.main(args);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("The AQMesh input agent could not be constructed!", e.getMessage());
        }
    }

    @Test
    public void testMainErrorWhenCreatingTSClient() throws URISyntaxException, IOException {
        createProperAgentPropertiesFile();
        // Empty properties file for time series client should result in exception
        try {
            AQMeshInputAgentLauncher.main(args);
            Assert.fail();
        }
        catch (JPSRuntimeException e) {
            Assert.assertEquals("Could not construct the time series client needed by the input agent!", e.getMessage());
        }

    }

    @Test
    public void testMainErrorWhenCreatingAPIConnector() throws IOException {
        createProperClientPropertiesFile();
        // Use a mock for the input agent
        try(MockedConstruction<AQMeshInputAgent> mockAgent = Mockito.mockConstruction(AQMeshInputAgent.class)) {
            // Empty API properties file should result in an exception
            try {
                AQMeshInputAgentLauncher.main(args);
                Assert.fail();
            }
            catch (JPSRuntimeException e) {
                // Ensure that the method to set the time series client was invoked once
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).setTsClient(Mockito.any());
                // Ensure that the initialization was invoked once
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).initializeTimeSeriesIfNotExist();
                Assert.assertEquals("Could not construct the AQMesh API connector needed to interact with the API!", e.getMessage());
            }
        }

    }

    @Test
    public void testMainErrorWhenRetrievingReadings() throws IOException {
        createProperClientPropertiesFile();
        createProperAPIPropertiesFile();
        // Use a mock for the input agent
        try(MockedConstruction<AQMeshInputAgent> ignored = Mockito.mockConstruction(AQMeshInputAgent.class)) {
            // Use a mock for the connector that throws an exception when readings are requested
            try(MockedConstruction<AQMeshAPIConnector> mockConnector = Mockito.mockConstruction(AQMeshAPIConnector.class,
                    (mock, context) -> Mockito.when(mock.getGasReadings()).thenThrow(new JPSRuntimeException("exception")))) {
                try {
                    AQMeshInputAgentLauncher.main(args);
                    Assert.fail();
                }
                catch (JPSRuntimeException e) {
                    // Ensure that the connect method was invoked once
                    Mockito.verify(mockConnector.constructed().get(0), Mockito.times(1)).connect();
                    // Ensure that the get particle readings was invoked once
                    Mockito.verify(mockConnector.constructed().get(0), Mockito.times(1)).getParticleReadings();
                    Assert.assertEquals("One or both readings could not be retrieved, this might have created a mismatch " +
                            "in the pointers if one readings was successful and needs to be fixed!", e.getMessage());
                    Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
                    Assert.assertEquals("exception", e.getCause().getMessage());
                }
            }
        }
    }


    @Test
    public void testMainBothReadingsNotEmpty() throws IOException {
        createProperClientPropertiesFile();
        createProperAPIPropertiesFile();
        // Create dummy readings to return
        JSONArray readings = new JSONArray();
        readings.put(new JSONObject("{'data' : 1}"));
        // Use a mock for the input agent
        try(MockedConstruction<AQMeshInputAgent> mockAgent = Mockito.mockConstruction(AQMeshInputAgent.class)) {
            // Use a mock for the connector that returns the dummy readings
            try(MockedConstruction<AQMeshAPIConnector> ignored = Mockito.mockConstruction(AQMeshAPIConnector.class,
                    (mock, context) -> {
                        Mockito.when(mock.getGasReadings()).thenReturn(readings);
                        Mockito.when(mock.getParticleReadings()).thenReturn(readings);
                    })) {
                AQMeshInputAgentLauncher.main(args);
                // Ensure that the update of the agent was invoked
                Mockito.verify(mockAgent.constructed().get(0), Mockito.times(1)).updateData(readings, readings);
            }
        }
    }

    @Test
    public void testMainOneReadingEmpty() throws IOException {
        createProperClientPropertiesFile();
        createProperAPIPropertiesFile();
        // Create dummy readings to return
        JSONArray particleReadings = new JSONArray("[]");
        JSONArray gasReadings = new JSONArray();
        gasReadings.put(new JSONObject("{'data' : 1}"));
        // Use a mock for the input agent
        try(MockedConstruction<AQMeshInputAgent> mockAgent = Mockito.mockConstruction(AQMeshInputAgent.class)) {
            // Use a mock for the connector that returns the dummy readings
            try(MockedConstruction<AQMeshAPIConnector> ignored = Mockito.mockConstruction(AQMeshAPIConnector.class,
                    (mock, context) -> {
                        Mockito.when(mock.getGasReadings()).thenReturn(gasReadings);
                        Mockito.when(mock.getParticleReadings()).thenReturn(particleReadings);
                    })) {
                try {
                    AQMeshInputAgentLauncher.main(args);
                    Assert.fail();
                }
                catch (JPSRuntimeException e) {
                    // Ensure that the update of the agent was never invoked
                    Mockito.verify(mockAgent.constructed().get(0), Mockito.never()).updateData(Mockito.any(), Mockito.any());
                    Assert.assertEquals("One of the readings (gas or particle) is empty, that means there is " +
                            "a mismatch in the pointer for each readings. This should be fixed (and might require a clean up of the database)!",
                            e.getMessage());
                }
            }
        }
    }

    @Test
    public void testMainBothReadingsEmpty() throws IOException {
        createProperClientPropertiesFile();
        createProperAPIPropertiesFile();
        // Create dummy readings to return
        JSONArray readings = new JSONArray("[]");
        // Use a mock for the input agent
        try(MockedConstruction<AQMeshInputAgent> mockAgent = Mockito.mockConstruction(AQMeshInputAgent.class)) {
            // Use a mock for the connector that returns the dummy readings
            try(MockedConstruction<AQMeshAPIConnector> ignored = Mockito.mockConstruction(AQMeshAPIConnector.class,
                    (mock, context) -> {
                        Mockito.when(mock.getGasReadings()).thenReturn(readings);
                        Mockito.when(mock.getParticleReadings()).thenReturn(readings);
                    })) {
                AQMeshInputAgentLauncher.main(args);
                // Ensure that the update of the agent was never invoked
                Mockito.verify(mockAgent.constructed().get(0), Mockito.never()).updateData(Mockito.any(), Mockito.any());
            }
        }
    }

    private void createProperAgentPropertiesFile() throws URISyntaxException, IOException {
        // Create a properties file that points to the example/test mapping folder in the resources //
        String mappingFolder = Paths.get(Objects.requireNonNull(getClass().getResource("/mappings"))
                .toURI()).toString().replace("\\","/");
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), agentPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("aqmesh.mappingfolder=" + mappingFolder + "\n");
        }
    }

    private void createProperClientPropertiesFile() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), clientPropertiesFilename).toString();
        try (FileWriter writer = new FileWriter(propertiesFile, false)) {
            writer.write("db.url=jdbc:postgresql:timeseries\n");
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
            writer.write("aqmesh.username=username\n");
            writer.write("aqmesh.password=password\n");
            writer.write("aqmesh.url=https://apitest.aqmeshdata.net/api/\n");
        }
    }
}
