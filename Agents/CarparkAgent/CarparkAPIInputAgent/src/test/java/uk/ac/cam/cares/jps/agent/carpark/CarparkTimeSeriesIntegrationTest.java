package uk.ac.cam.cares.jps.agent.carpark;

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
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

/**
 * This test class is to test the Carpark agent with a running KG and postgres database.
 */
@Ignore("Requires both triple store endpoint and postgreSQL database set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")

@Testcontainers
public class CarparkTimeSeriesIntegrationTest {

    // Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);
    // Create Docker container with postgres 13.3 image from Docker Hub
    @Container
    private final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");

    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private TimeSeriesHandler timeSeriesHandler;
    // Time series client for connection with KG and database
    private TimeSeriesClient<OffsetDateTime> tsClient;

    // Default lists of JSON keys
    private final String[] keys = {"AvailableLots_ACM_C","AvailableLots_ACM_H"};
    // Example prefix for IRIs
    private final String examplePrefix = "example:prefix/api_";
    // IRIs corresponding to the keys
    private ArrayList<String> IRIs;

    // Generate timestamp
    long timestamp = System.currentTimeMillis();
    Date date = new java.util.Date(timestamp);
    SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    Object ts = sdf.format(date);
    private final String[] timestamps = {ts.toString()};

    // Readings used by several tests
    JSONObject carparkDataReadings;

    // Values created as example readings
    private ArrayList<Double> carparkLotsValues;

    // Mappings
    List<JSONKeyToIRIMapper> tsmappings;

    @Before
    public void initialize() throws IOException {
        // Start the containers
        try {
            // Start Blazegraph container
            blazegraph.start();
            // Start postgreSQL container
            postgres.start();
        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }

        // Create a properties file that points to a dummy mapping folder //
        // Create an empty folder
        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Add mapping file into the empty folder
        String mappingFile = Paths.get(mappingFolder.getAbsolutePath(), "carpark.properties").toString();
        ArrayList<String> mappings = new ArrayList<>();
        IRIs = new ArrayList<>();
        for (String key: keys) {
            mappings.add(key + "="+examplePrefix+key);
            IRIs.add(examplePrefix+key);
        }
        writePropertyFile(mappingFile, mappings);
        tsmappings = new ArrayList<>();
        tsmappings = readMappings(mappingFolder.getAbsolutePath());
        
        // Create timeseries handler
        timeSeriesHandler = new TimeSeriesHandler(tsmappings);

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
        timeSeriesHandler.setTsClient(tsClient);
    }

    @Before
    public void createExampleReadings() {
        carparkDataReadings = new JSONObject();
        carparkDataReadings.put("odata.metadata", "http://datamall2.mytransport.sg/ltaodataservice/$metadata#CarParkAvailability");
        JSONArray subArray = new JSONArray();
        carparkLotsValues = new ArrayList<>();
        for (int i = 0; i < keys.length; i ++) {
            JSONObject carpark = new JSONObject();
            carpark.put("CarParkID", "ACM");
            carpark.put("Area", "");
            carpark.put("Development", "testing");
            carpark.put("Location", "1.3210042901052126 103.8850609476153");
            carpark.put("AvailableLots", 5.0);
            carparkLotsValues.add(5.0);
            carpark.put("LotType", keys[i].split("_")[2]);
            carpark.put("Agency", "testing-agency");
            subArray.put(carpark);
        }
        carparkDataReadings.put("value", subArray);
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
        timeSeriesHandler.initializeTimeSeriesIfNotExist();
        // Check that time-series instances were created
        Assert.assertEquals(1, tsClient.countTimeSeries());
        // Check that all IRIs have a time-series attached and that they are attached to the same
        String tsIRI = "";

        for (String iri: IRIs) {
            Assert.assertTrue(tsClient.checkDataHasTimeSeries(iri));
            if (tsIRI.equals("")) {
                tsIRI = tsClient.getTimeSeriesIRI(iri);
            }
            else {
                Assert.assertEquals(tsIRI, tsClient.getTimeSeriesIRI(iri));
            }
        }
    }

    @Test
    public void testInitializeTimeSeriesIfNotExistsWithExistingTimeSeries() {
        // Insert weather time-series
        ArrayList<String> iris = new ArrayList<>();
        ArrayList<Class<?>> classes = new ArrayList<>();
        for (String key: keys) {
            iris.add(examplePrefix+key);
            classes.add(Double.class);
        }
        tsClient.initTimeSeries(iris, classes, "timeUnit");

        // Create spy to verify executions on the time-series client
        TimeSeriesClient<OffsetDateTime> tsClientSpy = Mockito.spy(tsClient);
        timeSeriesHandler.setTsClient(tsClientSpy);
        // Should only insert the time-series
        timeSeriesHandler.initializeTimeSeriesIfNotExist();
        Mockito.verify(tsClientSpy, Mockito.times(0)).
                initTimeSeries(Mockito.anyList(), Mockito.anyList(), Mockito.anyString());
        // Check that time-series instances were created
        Assert.assertEquals(1, tsClient.countTimeSeries());
        // Check that all IRIs have a time-series attached and that they are attached to the same
        String tsIRI = "";
        for (String iri: IRIs) {
            Assert.assertTrue(tsClient.checkDataHasTimeSeries(iri));
            if (tsIRI.equals("")) {
                tsIRI = tsClient.getTimeSeriesIRI(iri);
            }
            else {
                Assert.assertEquals(tsIRI, tsClient.getTimeSeriesIRI(iri));
            }
        }
    }

    @Test
    public void testUpdateTimeSeries() {
        insertTimeSeries();
        // Update time-series data
        timeSeriesHandler.updateData(carparkDataReadings);
        // Check that database was updated
        TimeSeries<OffsetDateTime> ts = tsClient.getTimeSeries(IRIs);
        Assert.assertEquals(timestamps.length, ts.getTimes().size());

        for (int i = 0; i < IRIs.size(); i++) {
            Assert.assertEquals(5.0, ts.getValues(IRIs.get(i)).get(0));
        }
    }

    @Test
    public void testUpdateTimeSeriesWithEmptyReadings() {
        insertTimeSeries();
        // Update time-series data (should throw an error)
        try {
            timeSeriesHandler.updateData(new JSONObject());
            Assert.fail();
        }
        catch(Exception e){
            Assert.assertTrue(e.toString().contains("Readings can not be empty!"));
        }
    }

    private void insertTimeSeries() {
        // Insert weather data time-series
        List<Class<?>> classes = Collections.nCopies(IRIs.size(), Double.class);
        tsClient.initTimeSeries(IRIs, classes, "timeUnit");
    }

    /**
     * Reads the JSON key to IRI mappings from files in the provided folder.
     * @param mappingFolder The path to the folder in which the mapping files are located.
     * @return 
     */
    private List<JSONKeyToIRIMapper> readMappings(String mappingFolder) throws IOException {
        List<JSONKeyToIRIMapper> mappings = new ArrayList<>();
        File folder = new File(mappingFolder);
        File[] mappingFiles = folder.listFiles();
        // Make sure the folder exists and contains files
        if (mappingFiles == null) {
            throw new IOException("Folder does not exist: " + mappingFolder);
        }
        if (mappingFiles.length == 0) {
            throw new IOException("No files in the folder: " + mappingFolder);
        }
        // Create a mapper for each file
        else {
            for (File mappingFile: mappingFiles) {
                JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(CarparkAgent.TIMESERIES_IRI_PREFIX, mappingFile.getAbsolutePath());
                mappings.add(mapper);
                // Save the mappings back to the file to ensure using same IRIs next time
                mapper.saveToFile(mappingFile.getAbsolutePath());
            }
        }
        return mappings;
    }
}