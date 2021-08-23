package uk.ac.cam.cares.jps.agent.aqmesh;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.Mockito;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * This test class is to test the AQMesh input agent with a running KG and postgres database.
 */
@Ignore("Requires both triple store endpoint and postgreSQL database set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")
@Testcontainers
public class AQMeshInputAgentIntegrationTest {

    // Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker_Registry
    @Container
    private final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);
    // Create Docker container with postgres 13.3 image from Docker Hub
    @Container
    private final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");

    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // AQMesh in[ut agent
    private AQMeshInputAgent agent;
    // Time series client for connection with KG and database
    private TimeSeriesClient<OffsetDateTime> tsClient;

    // Default lists of JSON keys (also defining the type)
    private final String[] particleKeys = {"pkey1_p1", "pkey2_p1"};
    private final String[] gasKeys = {"gkey1_prescale", "gkey2_prescale" ,"gkey3_prescale"};
    // Example prefix for IRIs
    private final String examplePrefix = "example:prefix/api_";
    // IRIs corresponding to the keys
    private ArrayList<String> particleIRIs;
    private ArrayList<String> gasIRIs;

    // Default list of timestamps
    private final String[] timestamps = {"2021-07-11T16:10:00", "2021-07-11T16:15:00", "2021-07-11T16:20:00", "2021-07-11T16:25:00"};
    // Values created as example readings
    private ArrayList<Integer> particleValues;
    private ArrayList<Double> gasValues;

    // Readings used by several tests
    JSONArray particleReadings;
    JSONArray gasReadings;

    @Before
    public void initializeAgent() throws IOException {
        // Start the containers
        try {
            // Start Blazegraph container
            blazegraph.start();
            // Start postgreSQL container
            postgres.start();
        } catch (Exception e) {
            throw new AssertionError("TimeSeriesClientIntegrationTest: Docker container startup failed. Please try running tests again");
        }

        // Create a properties file that points to a dummy mapping folder //
        // Create an empty folder
        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Add mapping file into the empty folder
        String particleMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "particle_mapping.properties").toString();
        ArrayList<String> particleMappings = new ArrayList<>();
        particleIRIs = new ArrayList<>();
        for (String key: particleKeys) {
            particleMappings.add(key + "="+examplePrefix+key);
            particleIRIs.add(examplePrefix+key);
        }
        writePropertyFile(particleMappingFile, particleMappings);
        String gasMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "gas_mapping.properties").toString();
        ArrayList<String> gasMappings = new ArrayList<>();
        gasIRIs = new ArrayList<>();
        for (String key: gasKeys) {
            gasMappings.add(key + "="+examplePrefix+key);
            gasIRIs.add(examplePrefix+key);
        }
        writePropertyFile(gasMappingFile, gasMappings);
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writePropertyFile(propertiesFile, Collections.singletonList("aqmesh.mappingfolder=" + mappingFolder.getCanonicalPath().
                replace("\\","/")));
        // Create agent
        agent = new AQMeshInputAgent(propertiesFile);

        // Create and set time-series client //
        // Set endpoint to the triple store. The host and port are read from the container
        String endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        // Default namespace in blazegraph is "kb"
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql";

        // Set up a kb client that points to the location of the triple store
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setUpdateEndpoint(endpoint);
        kbClient.setQueryEndpoint(endpoint);

        // Initialise TimeSeriesClient client with pre-configured kb client
        tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, null, "postgres", "postgres");
        // Configure database access
        tsClient.setRDBClient(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
        // Set client for agent
        agent.setTsClient(tsClient);
    }

    @Before
    public void createExampleReadings() {

        particleReadings = new JSONArray();
        gasReadings = new JSONArray();

        particleValues = new ArrayList<>();
        gasValues = new ArrayList<>();

        double value = 0.0;
        for(String timestamp: timestamps) {
            JSONObject currentGasMeasures = new JSONObject();
            JSONObject currentParticleMeasures = new JSONObject();
            // Put the timestamp in the current reading
            currentGasMeasures.put(AQMeshInputAgent.timestampKey, timestamp);
            currentParticleMeasures.put(AQMeshInputAgent.timestampKey, timestamp);
            // Put values for each key into the current readings
            for(String key: particleKeys) {
                currentParticleMeasures.put(key, (int) value);
            }
            for(String key: gasKeys) {
                currentGasMeasures.put(key, value);
            }
            particleReadings.put(currentParticleMeasures);
            gasReadings.put(currentGasMeasures);
            particleValues.add((int) value);
            gasValues.add(value);
            value++;
        }
    }

    // Cleaning up containers after each test, otherwise unused containers will first be killed when all tests finished
    @After
    public void stopContainers() {
        if (blazegraph.isRunning()) {
            blazegraph.stop();
        }
        if (postgres.isRunning()) {
            postgres.stop();
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

    @Test
    public void testInitializeTimeSeriesIfNotExists() {
        agent.initializeTimeSeriesIfNotExist();
        // Check that time-series instances were created
        Assert.assertEquals(2, tsClient.countTimeSeries());
        // Check that all IRIs have a time-series attached and that they are attached to the same
        String gasTsIRI = "";
        String particleTsIRI = "";
        for (String iri: gasIRIs) {
            Assert.assertTrue(tsClient.checkDataHasTimeSeries(iri));
            if (gasTsIRI.equals("")) {
                gasTsIRI = tsClient.getTimeSeriesIRI(iri);
            }
            else {
                Assert.assertEquals(gasTsIRI, tsClient.getTimeSeriesIRI(iri));
            }
        }
        for (String iri: particleIRIs) {
            Assert.assertTrue(tsClient.checkDataHasTimeSeries(iri));
            if (particleTsIRI.equals("")) {
                particleTsIRI = tsClient.getTimeSeriesIRI(iri);
            }
            else {
                Assert.assertEquals(particleTsIRI, tsClient.getTimeSeriesIRI(iri));
            }
        }
    }

    @Test
    public void testInitializeTimeSeriesIfNotExistsWithExistingTimeSeries() {
        // Insert particle time-series
        ArrayList<String> iris = new ArrayList<>();
        ArrayList<Class<?>> classes = new ArrayList<>();
        for (String key: particleKeys) {
            iris.add(examplePrefix+key);
            classes.add(Double.class);
        }
        tsClient.initTimeSeries(iris, classes, "timeUnit");

        // Create spy to verify executions on the time-series client
        TimeSeriesClient<OffsetDateTime> tsClientSpy = Mockito.spy(tsClient);
        agent.setTsClient(tsClientSpy);
        // Should only insert the gas time-series
        agent.initializeTimeSeriesIfNotExist();
        Mockito.verify(tsClientSpy, Mockito.times(1)).
                initTimeSeries(Mockito.anyList(), Mockito.anyList(), Mockito.anyString());
        // Check that time-series instances were created
        Assert.assertEquals(2, tsClient.countTimeSeries());
        // Check that all IRIs have a time-series attached and that they are attached to the same
        String gasTsIRI = "";
        String particleTsIRI = "";
        for (String iri: gasIRIs) {
            Assert.assertTrue(tsClient.checkDataHasTimeSeries(iri));
            if (gasTsIRI.equals("")) {
                gasTsIRI = tsClient.getTimeSeriesIRI(iri);
            }
            else {
                Assert.assertEquals(gasTsIRI, tsClient.getTimeSeriesIRI(iri));
            }
        }
        for (String iri: particleIRIs) {
            Assert.assertTrue(tsClient.checkDataHasTimeSeries(iri));
            if (particleTsIRI.equals("")) {
                particleTsIRI = tsClient.getTimeSeriesIRI(iri);
            }
            else {
                Assert.assertEquals(particleTsIRI, tsClient.getTimeSeriesIRI(iri));
            }
        }
    }

    @Test
    public void testUpdateTimeSeries() {
        insertTimeSeries();
        // Update time-series data
        agent.updateData(particleReadings, gasReadings);
        // Check that database was updated
        TimeSeries<OffsetDateTime> gasTs = tsClient.getTimeSeries(gasIRIs);
        TimeSeries<OffsetDateTime> particleTs = tsClient.getTimeSeries(particleIRIs);
        Assert.assertEquals(gasReadings.length(), gasTs.getTimes().size());
        Assert.assertEquals(particleReadings.length(), particleTs.getTimes().size());
        // Check that data content is correct
        for (String iri: particleIRIs) {
            Assert.assertEquals(particleValues, particleTs.getValues(iri));
        }
        for (String iri: gasIRIs) {
            Assert.assertEquals(gasValues, gasTs.getValues(iri));
        }
        // Assert timestamps
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[0]), AQMeshInputAgent.ZONE_OFFSET)
                        .isEqual(tsClient.getMinTime(gasIRIs.get(0))));
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[0]), AQMeshInputAgent.ZONE_OFFSET)
                        .isEqual(tsClient.getMinTime(particleIRIs.get(0))));
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[timestamps.length-1]), AQMeshInputAgent.ZONE_OFFSET)
                        .isEqual(tsClient.getMaxTime(gasIRIs.get(0))));
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[timestamps.length-1]), AQMeshInputAgent.ZONE_OFFSET)
                        .isEqual(tsClient.getMaxTime(particleIRIs.get(0))));
    }

    @Test
    public void testUpdateTimeSeriesWithEmptyReadings() {
        insertTimeSeries();
        // Update time-series data (should throw an error)
        try {
            agent.updateData(new JSONArray("[]"), new JSONArray("[]"));
            Assert.fail();
        }
        catch (IllegalArgumentException e) {
            Assert.assertEquals("Readings can not be empty!", e.getMessage());
        }
    }

    @Test
    public void testUpdateTimeSeriesWithPruning() {
        insertTimeSeries();
        // Add data for gas readings up to last reading
        List<OffsetDateTime> times = new ArrayList<>();
        for(int i = 0; i < timestamps.length ; i++) {
            if (i < (timestamps.length-1)) {
                times.add(OffsetDateTime.of(LocalDateTime.parse(timestamps[i]), AQMeshInputAgent.ZONE_OFFSET));
            }
        }
        TimeSeries<OffsetDateTime> gasTs = new TimeSeries<>(times, gasIRIs,
                Collections.nCopies(gasIRIs.size(), gasValues.subList(0, gasValues.size()-1)));
        tsClient.addTimeSeriesData(gasTs);
        // Update data through agent
        agent.updateData(particleReadings, gasReadings);
        // Check that database was updated and existing gas data is untouched
        gasTs = tsClient.getTimeSeries(gasIRIs);
        TimeSeries<OffsetDateTime> particleTs = tsClient.getTimeSeries(particleIRIs);
        Assert.assertEquals(gasReadings.length(), gasTs.getTimes().size());
        Assert.assertEquals(particleReadings.length(), particleTs.getTimes().size());
        // Check that data content is correct
        for (String iri: particleIRIs) {
            Assert.assertEquals(particleValues, particleTs.getValues(iri));
        }
        for (String iri: gasIRIs) {
            Assert.assertEquals(gasValues, gasTs.getValues(iri));
        }
        // Assert timestamps
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[0]), AQMeshInputAgent.ZONE_OFFSET)
                .isEqual(tsClient.getMinTime(gasIRIs.get(0))));
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[0]), AQMeshInputAgent.ZONE_OFFSET)
                .isEqual(tsClient.getMinTime(particleIRIs.get(0))));
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[timestamps.length-1]), AQMeshInputAgent.ZONE_OFFSET)
                .isEqual(tsClient.getMaxTime(gasIRIs.get(0))));
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[timestamps.length-1]), AQMeshInputAgent.ZONE_OFFSET)
                .isEqual(tsClient.getMaxTime(particleIRIs.get(0))));
    }

    private void insertTimeSeries() {
        // Insert particle time-series
        List<Class<?>> classes = Collections.nCopies(particleIRIs.size(), Integer.class);
        tsClient.initTimeSeries(particleIRIs, classes, "timeUnit");
        // Insert gas time-series
        classes = Collections.nCopies(gasIRIs.size(), Double.class);
        tsClient.initTimeSeries(gasIRIs, classes, "timeUnit");
    }

}
