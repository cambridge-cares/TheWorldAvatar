package uk.ac.cam.cares.jps.agent.historicalntuenergy;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Logger;

import com.github.stefanbirkner.systemlambda.SystemLambda;
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
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;


/**
 * This test class is to test the Historical NTUEnergy agent with a running KG and postgres database.
 */

@Testcontainers
public class HistoricalNTUEnergyAgentIntegrationTest {

    // Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);
    // Create Docker container with postgres 13.3 image from Docker Hub
    @Container
    private final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");

    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // NTUEnergy input agent
    private HistoricalNTUEnergyAgent agent;
    // Time series client for connection with KG and database
    private TimeSeriesClient<OffsetDateTime> tsClient;

    // Default lists of JSON keys (also defining the type)
    private final String[] energyKeys = {"NEC_P_KW", "NEC_Q_KVAR"};
    private final String examplePrefix = "example:prefix/energy_";
    private ArrayList<String> energyIRIs;
    // Default list of timestamps
    private final String[] timestamps = {"2021-07-11T16:10:00", "2021-07-11T16:15:00", "2021-07-11T16:20:00", "2021-07-11T16:25:00"};
    private ArrayList<Integer> energyValues;
    JSONArray energyReadings;
    Logger LOGGER = Logger.getLogger(HistoricalNTUEnergyAgentIntegrationTest.class.getName());

    @Before
    public void initializeAgent() throws IOException {
        // Start Blazegraph container
        blazegraph.start();
        // Start postgreSQL container
        postgres.start();

        // Create a properties file that points to a dummy mapping folder //
        // Create an empty folder
        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Add mapping file into the empty folder
        String energyMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "energy.properties").toString();
        ArrayList<String> energyMappings = new ArrayList<>();
        energyIRIs = new ArrayList<>();
        for (String key : energyKeys) {
            energyMappings.add(key + "=" + examplePrefix + key);
            energyIRIs.add(examplePrefix + key);
        }
        writePropertyFile(energyMappingFile, energyMappings);

        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writePropertyFile(propertiesFile, Collections.singletonList("ntuenergy.mappingfolder=TEST_MAPPINGS"));
        try {
            LOGGER.info("propertiesFile: " + propertiesFile);
            LOGGER.info("TEST_MAPPINGS: " + mappingFolder.getCanonicalPath());
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                agent = new HistoricalNTUEnergyAgent(propertiesFile);
            });
        } catch (Exception e) {
        }

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

    @Before
    public void createExampleReadings() {
        energyReadings = new JSONArray();
        energyValues = new ArrayList<>();
        double value = 0.0;
        for (String timestamp : timestamps) {
            JSONObject currentEnergyMeasures = new JSONObject();
            // Put the timestamp in the current reading
            currentEnergyMeasures.put(HistoricalNTUEnergyAgent.timestampKey, timestamp);
            // Put values for each key into the current readings
            for (String key : energyKeys) {
                currentEnergyMeasures.put(key, (int) value);
            }
            energyReadings.put(currentEnergyMeasures);
            energyValues.add((int) value);
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

    @Test
    public void testInitializeTimeSeriesIfNotExists() {
        agent.initializeTimeSeriesIfNotExist();
        // Check that time-series instances were created
        Assert.assertEquals(1, tsClient.countTimeSeries());
        // Check that all IRIs have a time-series attached and that they are attached to the same
        String energyTsIRI = "";
        for (String iri : energyIRIs) {
            Assert.assertTrue(tsClient.checkDataHasTimeSeries(iri));
            if (energyTsIRI.equals("")) {
                energyTsIRI = tsClient.getTimeSeriesIRI(iri);
            } else {
                Assert.assertEquals(energyTsIRI, tsClient.getTimeSeriesIRI(iri));
            }
        }
    }

    @Test
    public void testInitializeTimeSeriesIfNotExistsWithExistingTimeSeries() {
        // Insert energy time-series
        ArrayList<String> iris = new ArrayList<>();
        ArrayList<Class<?>> classes = new ArrayList<>();
        for (String key : energyKeys) {
            iris.add(examplePrefix + key);
            classes.add(Double.class);
        }
        tsClient.initTimeSeries(iris, classes, "timeUnit");

        // Create spy to verify executions on the time-series client
        TimeSeriesClient<OffsetDateTime> tsClientSpy = Mockito.spy(tsClient);
        agent.setTsClient(tsClientSpy);
        // Should only insert the energy time-series
        agent.initializeTimeSeriesIfNotExist();
        // Check that time-series instances were created
        Assert.assertEquals(1, tsClient.countTimeSeries());
        // Check that all IRIs have a time-series attached and that they are attached to the same
        String energyTsIRI = "";
        for (String iri : energyIRIs) {
            Assert.assertTrue(tsClient.checkDataHasTimeSeries(iri));
            if (energyTsIRI.equals("")) {
                energyTsIRI = tsClient.getTimeSeriesIRI(iri);
            } else {
                Assert.assertEquals(energyTsIRI, tsClient.getTimeSeriesIRI(iri));
            }
        }
    }

    @Test
    public void testUpdateTimeSeriesWithEmptyReadings() {
        insertTimeSeries();
        // Update time-series data (should throw an error)
        try {
            agent.updateData(new JSONArray("[]"));
            Assert.fail();
        } catch (IllegalArgumentException e) {
            Assert.assertEquals("Readings can not be empty!", e.getMessage());
        }
    }

    private void insertTimeSeries() {
        // Insert energy time-series
        List<Class<?>> classes = Collections.nCopies(energyIRIs.size(), Double.class);
        tsClient.initTimeSeries(energyIRIs, classes, "timeUnit");
    }
}
