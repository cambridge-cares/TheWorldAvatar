package uk.ac.cam.cares.jps.agent.thingspeak;

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

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

/**
 * This test class is to test the Thingspeak input agent with a running KG and postgres database.
 */

@Ignore("Requires both triple store endpoint and postgreSQL database set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")

@Testcontainers
public class ThingspeakInputAgentIntegrationTest {

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

    // Thingspeak Input Agent
    private ThingspeakInputAgent agent;
    // Time series client for connection with KG and database
    private TimeSeriesClient<OffsetDateTime> tsClient;
    
    private RemoteRDBStoreClient rdbStoreClient;

    // Default lists of JSON keys (also defining the type)
    private final String[] keys = {"ppm"};
    // Example prefix for IRIs
    private final String examplePrefix = "example:prefix/api_";
    // IRIs corresponding to the keys
    private ArrayList<String> IRIs;

    // Default list of timestamps
    private final String[] timestamps = new String[4];
    

    // Values created as example readings
    private ArrayList<Double> Values;

    // Readings used by several tests
    JSONObject responseBody;

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
        writePropertyFile(propertiesFile, Collections.singletonList("thingspeak.mappingfolder=TEST_MAPPINGS"));
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
        		 agent = new ThingspeakInputAgent(propertiesFile);
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
        tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class);
        rdbStoreClient = new RemoteRDBStoreClient(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
        agent.setRDBClient(rdbStoreClient);
        agent.setTsClient(tsClient);
    }

    @Before
    public void createExampleReadings() {
    	responseBody = new JSONObject();
        JSONObject jsonobject1 = new JSONObject();
        JSONObject jsonobject2 = new JSONObject();
        JSONObject jsonobject3 = new JSONObject();
        JSONObject jsonobject4 = new JSONObject();
        JSONObject jsonobject5 = new JSONObject();
        JSONArray jsonarray1 = new JSONArray();
        
        timestamps[0] = "2022-11-09T03:05:18";
        timestamps[1] = "2022-11-09T03:06:18";
        timestamps[2] = "2022-11-09T03:07:18";
        timestamps[3] = "2022-11-09T03:08:18";
        Values = new ArrayList<>();
        double value = 621.0 ;
        Values.add(value) ;
        value = 620.0 ;
        Values.add(value) ;
        value = 620.0 ;
        Values.add(value) ;
        value = 621.0 ;
        Values.add(value) ;
        
        jsonobject1.put("created_at", "2022-11-09T03:05:18Z");
        jsonobject1.put("entry_id", 59267);
        jsonobject1.put("field1", "621");
        
        jsonobject3.put("created_at", "2022-11-09T03:06:18Z");
        jsonobject3.put("entry_id", 59268);
        jsonobject3.put("field1", "620");
        
        jsonobject4.put("created_at", "2022-11-09T03:07:18Z");
        jsonobject4.put("entry_id", 59268);
        jsonobject4.put("field1", "620");
        
        jsonobject5.put("created_at", "2022-11-09T03:08:18Z");
        jsonobject5.put("entry_id", 59269);
        jsonobject5.put("field1", "621");
        
        jsonarray1.put(jsonobject1);
        jsonarray1.put(jsonobject3);
        jsonarray1.put(jsonobject4);
        jsonarray1.put(jsonobject5);
        
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
    public void testInitializeTimeSeriesIfNotExists() throws SQLException {
        agent.initializeTimeSeriesIfNotExist();
        // Check that time-series instances were created
        Assert.assertEquals(1, tsClient.countTimeSeries());
        // Check that all IRIs have a time-series attached and that they are attached to the same
        String tsIRI = "";
        
        for (String iri: IRIs) {
        	try(Connection conn = rdbStoreClient.getConnection()){
            Assert.assertTrue(tsClient.checkDataHasTimeSeries(iri, conn));
            if (tsIRI.equals("")) {
                tsIRI = tsClient.getTimeSeriesIRI(iri);
            }
            else {
                Assert.assertEquals(tsIRI, tsClient.getTimeSeriesIRI(iri));
            }
        }
        }
    }

    @Test
    public void testInitializeTimeSeriesIfNotExistsWithExistingTimeSeries() throws SQLException {
        // Insert particle time-series
        ArrayList<String> iris = new ArrayList<>();
        ArrayList<Class<?>> classes = new ArrayList<>();
        for (String key: keys) {
            iris.add(examplePrefix+key);
            classes.add(Double.class);
        }
      	try(Connection conn = rdbStoreClient.getConnection()){
        tsClient.initTimeSeries(iris, classes, "timeUnit", conn);

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
            Assert.assertTrue(tsClient.checkDataHasTimeSeries(iri, conn));
            if (tsIRI.equals("")) {
                tsIRI = tsClient.getTimeSeriesIRI(iri);
            }
            else {
                Assert.assertEquals(tsIRI, tsClient.getTimeSeriesIRI(iri));
            }
        }
      	}
    }

    @Test
    public void testUpdateTimeSeries() throws SQLException {
    	insertTimeSeries();
    	agent.updateData(responseBody);
      	try(Connection conn = rdbStoreClient.getConnection()){
        
        // Update time-series data
        
        // Check that database was updated
        TimeSeries<OffsetDateTime> ts = tsClient.getTimeSeries(IRIs, conn);
        Assert.assertEquals(responseBody.getJSONArray("feeds").length(), ts.getTimes().size());
        // Check that data content is correct
        String iri = IRIs.get(0);
        Assert.assertEquals(Values, ts.getValues(iri));
        
        // Assert timestamps
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[0]), ThingspeakInputAgent.ZONE_OFFSET)
                        .isEqual(tsClient.getMinTime(IRIs.get(0), conn)));
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[timestamps.length-1]), ThingspeakInputAgent.ZONE_OFFSET)
                        .isEqual(tsClient.getMaxTime(IRIs.get(0), conn)));
      	}
    }

    @Test
    public void testUpdateTimeSeriesWithEmptyReadings() throws SQLException {
        insertTimeSeries();
        // Update time-series data (should throw an error)
        try {
            agent.updateData(new JSONObject("{}"));
            Assert.fail();
        }
        catch (IllegalArgumentException e) {
            Assert.assertEquals("Readings can not be empty!", e.getMessage());
        }
    }

    @Test
    public void testUpdateTimeSeriesWithPruning() throws SQLException {
        insertTimeSeries();
        // Add data for gas readings up to last reading
        List<OffsetDateTime> times = new ArrayList<>();
        for(int i = 0; i < timestamps.length ; i++) {
            if (i < (timestamps.length-1)) {
                times.add(OffsetDateTime.of(LocalDateTime.parse(timestamps[i]), ThingspeakInputAgent.ZONE_OFFSET));
            }
        }
        TimeSeries<OffsetDateTime> ts = new TimeSeries<>(times, IRIs,
                Collections.nCopies(IRIs.size(), Values.subList(0, Values.size()-1)));
        try(Connection conn = rdbStoreClient.getConnection()){
        tsClient.addTimeSeriesData(ts, conn);
        }
        // Update data through agent
        agent.updateData(responseBody);
        // Check that database was updated and existing gas data is untouched
        try(Connection conn = rdbStoreClient.getConnection()){
        ts = tsClient.getTimeSeries(IRIs, conn);
        }
        Assert.assertEquals(responseBody.getJSONArray("feeds").length(),ts.getTimes().size());
        // Check that data content is correct
        for (String iri: IRIs) {
            Assert.assertEquals(Values, ts.getValues(iri));
        }
        // Assert timestamps
        try(Connection conn = rdbStoreClient.getConnection()){
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[0]), ThingspeakInputAgent.ZONE_OFFSET)
                .isEqual(tsClient.getMinTime(IRIs.get(0), conn)));
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[timestamps.length-1]), ThingspeakInputAgent.ZONE_OFFSET)
                .isEqual(tsClient.getMaxTime(IRIs.get(0), conn)));
    }
    }

    private void insertTimeSeries() throws SQLException {
    	try(Connection conn = rdbStoreClient.getConnection()){
        List<Class<?>> classes = Collections.nCopies(IRIs.size(), Double.class);
        tsClient.initTimeSeries(IRIs, classes, "timeUnit", conn);
    	}
    }
}
