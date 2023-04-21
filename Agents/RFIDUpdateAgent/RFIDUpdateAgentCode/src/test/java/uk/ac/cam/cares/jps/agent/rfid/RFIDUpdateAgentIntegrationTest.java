package uk.ac.cam.cares.jps.agent.rfid;

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

import com.github.stefanbirkner.systemlambda.SystemLambda;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

/**
 * This test class is to test the RFID Update Agent with a running KG and postgres database.
 */

@Ignore("Requires both triple store endpoint and postgreSQL database set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")

@Testcontainers
public class RFIDUpdateAgentIntegrationTest {

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

    // RFID Update Agent
    private RFIDUpdateAgent agent;
    // Time series client for connection with KG and database
    private TimeSeriesClient<OffsetDateTime> tsClient;

    // Default lists of JSON keys (also defining the type)
    private final String[] keys = {"tag_12345_status"};
    // Example prefix for IRIs
    private final String examplePrefix = "example:prefix/api_";
    // IRIs corresponding to the keys
    private ArrayList<String> IRIs;

    // Default list of timestamps
    private final String[] timestamps = new String[4];
    

    // Values created as example readings
    private ArrayList<String> Values;

    // Readings used by several tests
    JSONObject allReadings;

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
        String allTypesMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "allTypes.properties").toString();
        ArrayList<String> allTypesMappings = new ArrayList<>();
        IRIs = new ArrayList<>();
        for (String key: keys) {
            allTypesMappings.add(key + "="+examplePrefix+key);
            IRIs.add(examplePrefix+key);
        }
        writePropertyFile(allTypesMappingFile, allTypesMappings);
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writePropertyFile(propertiesFile, Collections.singletonList("rfid.mappingfolder=TEST_MAPPINGS"));
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
        		 agent = new RFIDUpdateAgent(propertiesFile);
        	 });
        }
        catch (Exception e) {
        } 
        // Create agent
       

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
        tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, null, "postgres", "cares1010");
        // Configure database access
        tsClient.setRDBClient(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
        // Set client for agent
        agent.setTsClient(tsClient);
    }

    @Before
    public void createExampleReadings() {

        allReadings = new JSONObject();
        Values = new ArrayList<>();
        
        //create a list that contains the mock timestamps with the latest timestamp at the top
        long ts01 = 123456;
        long ts02 = 125056;
        long ts03 = 126656;
        long ts04 = 128256;
        long[] ts = new long[4];
        ts[0] = ts04;
        ts[1] = ts03;
        ts[2] = ts02;
        ts[3] = ts01;
        
        //convert the mock timestamps to date time format and put them in a list with the earliest at the top
        for (int i = 0; i < ts.length; i++) { 
        Date date = new java.util.Date(ts[i]);
    	SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    	sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
    	String dateTime = sdf.format(date);
    	timestamps[timestamps.length - 1 - i] = dateTime;
        }
        
        for(String key: keys) {
        	String value = "In";
        	JSONArray values = new JSONArray();
        	for (int i = 0; i < ts.length; i++) {
        		JSONObject tsAndValue = new JSONObject();
        		tsAndValue.put(RFIDUpdateAgent.timestampKey, ts[i]);
        		tsAndValue.put("value", value);
        		values.put(tsAndValue);
        		
        	}
        	allReadings.put(key, values);
        
        }
        String value = "In";
        for (int i = 0; i < ts.length; i++) {
        	Values.add(i, value);
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
        // Insert particle time-series
        ArrayList<String> iris = new ArrayList<>();
        ArrayList<Class<?>> classes = new ArrayList<>();
        for (String key: keys) {
            iris.add(examplePrefix+key);
            classes.add(Double.class);
        }
        tsClient.initTimeSeries(iris, classes, "timeUnit");

        // Create spy to verify executions on the time-series client
        TimeSeriesClient<OffsetDateTime> tsClientSpy = Mockito.spy(tsClient);
        agent.setTsClient(tsClientSpy);
        // Should only insert the time-series
        agent.initializeTimeSeriesIfNotExist();
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
        String key = "tag_12345_status";
        // Update time-series data
        agent.updateData(allReadings, key);
        // Check that database was updated
        TimeSeries<OffsetDateTime> ts = tsClient.getTimeSeries(IRIs);
        Assert.assertEquals(allReadings.getJSONArray(keys[0]).length(), ts.getTimes().size());
        // Check that data content is correct
        String iri = IRIs.get(0);
        Assert.assertEquals(Values, ts.getValues(iri));
        
        // Assert timestamps
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[0]), RFIDUpdateAgent.ZONE_OFFSET)
                        .isEqual(tsClient.getMinTime(IRIs.get(0))));
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[timestamps.length-1]), RFIDUpdateAgent.ZONE_OFFSET)
                        .isEqual(tsClient.getMaxTime(IRIs.get(0))));
    }

    @Test
    public void testUpdateTimeSeriesWithEmptyReadings() {
    	String key = "tag_12345_status";
        insertTimeSeries();
        // Update time-series data (should throw an error)
        try {
            agent.updateData(new JSONObject(), key);
            Assert.fail();
        }
        catch (IllegalArgumentException e) {
            Assert.assertEquals("Readings can not be empty!", e.getMessage());
        }
    }

    @Test
    public void testUpdateTimeSeriesWithPruning() {
    	String key = "tag_12345_status";
        insertTimeSeries();
        // Add data for readings up to last reading
        List<OffsetDateTime> times = new ArrayList<>();
        for(int i = 0; i < timestamps.length ; i++) {
            if (i < (timestamps.length-1)) {
                times.add(OffsetDateTime.of(LocalDateTime.parse(timestamps[i]), RFIDUpdateAgent.ZONE_OFFSET));
            }
        }
        TimeSeries<OffsetDateTime> ts = new TimeSeries<>(times, IRIs,
                Collections.nCopies(IRIs.size(), Values.subList(0, Values.size()-1)));
        tsClient.addTimeSeriesData(ts);
        // Update data through agent
        agent.updateData(allReadings, key);
        // Check that database was updated and existing data is untouched
        ts = tsClient.getTimeSeries(IRIs);
        Assert.assertEquals(allReadings.getJSONArray(keys[0]).length(),ts.getTimes().size());
        // Check that data content is correct
        for (String iri: IRIs) {
            Assert.assertEquals(Values, ts.getValues(iri));
        }
        // Assert timestamps
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[0]), RFIDUpdateAgent.ZONE_OFFSET)
                .isEqual(tsClient.getMinTime(IRIs.get(0))));
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[timestamps.length-1]), RFIDUpdateAgent.ZONE_OFFSET)
                .isEqual(tsClient.getMaxTime(IRIs.get(0))));
    }

    private void insertTimeSeries() {
        // Insert particle time-series
        List<Class<?>> classes = Collections.nCopies(IRIs.size(), String.class);
        tsClient.initTimeSeries(IRIs, classes, "timeUnit");
        // Insert time-series
    }
}
