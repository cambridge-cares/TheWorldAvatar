package uk.ac.cam.cares.jps.agent.fumehood;

import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

/**
 * This test class is to test the Fumehood sash and occupancy agent with a running KG and postgres database.
 */


@Ignore("Requires both triple store endpoint and postgreSQL database set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")
      

@Testcontainers
public class FHSashAndOccupancyAgentIntegrationTest {

    // Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);
    // Create Docker container with postgres 13.3 image from Docker Hub
    @Container
    private final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");

    @Rule
    //temp folder for temp client.properties file
    public TemporaryFolder folder = new TemporaryFolder();

    //FHSashAndOccupancyAgent
    private FHSashAndOccupancyAgent agent;

    //tsClient
    private TimeSeriesClient<OffsetDateTime> tsClient;

    //single key for mocking occupied state data IRI
    private final String occupiedStateKey1 = "FH_01_occupied_state";

    //single key for mocking occupied state data IRI
    private final String occupiedStateKey2 = "FH_02_occupied_state";

    //single key for mocking sash opening data IRI
    private final String sashOpeningKey1 = "FH_01_sash_opening";

    //single key for mocking sash opening data IRI
    private final String sashOpeningKey3 = "FH_03_sash_opening";

    // Example prefix for IRIs
    private final String examplePrefix = "example:prefix/api_";

    //map
    Map<String, List<String>> map1;

    //endpoint
    String endpoint;

    // list of IRIs
    private ArrayList<String> occupiedStateIRIs;
    private ArrayList<String> sashOpeningIRIs;

    //lists of date time
    private List<OffsetDateTime> times;

    // Values created as example readings
    private ArrayList<Double> Values;

    // Readings used by several tests
    JSONObject allReadings;

    //timestamp
    String timestamp;
    
    //list of classes to initialize timeseries
    private List<Class<?>> classes;

    String bgUsername;
    String bgPassword;
    String dbUrl;
    String dbUsername;
    String dbPassword;
 	
    @Before
    public void initializeMockTimeSeriesandAgent() throws IOException {
        // Start the containers
        try {
            // Start Blazegraph container
            blazegraph.start();
            // Start postgreSQL container
            postgres.start();
        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }
        occupiedStateIRIs = new ArrayList<>();
        occupiedStateIRIs.add(examplePrefix+occupiedStateKey1);
        occupiedStateIRIs.add(examplePrefix+occupiedStateKey2);

        sashOpeningIRIs = new ArrayList<>();
        sashOpeningIRIs.add(examplePrefix+sashOpeningKey1);
        sashOpeningIRIs.add(examplePrefix+sashOpeningKey3);
       
        // Create and set time-series client //
        // Set endpoint to the triple store. The host and port are read from the container
        endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        // Default namespace in blazegraph is "kb"
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql";

        // Set up a kb client that points to the location of the triple store
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setUpdateEndpoint(endpoint);
        kbClient.setQueryEndpoint(endpoint);
        
        String propertiesFile = Paths.get(folder.getRoot().toString(), "all.properties").toString();
        //single mock property file to represent the tsClient properties file
        writePropertyFile(propertiesFile, Arrays.asList("db.user=postgres", "db.password=cares1010", "sparql.query.endpoint="+endpoint, "sparql.update.endpoint="+endpoint));
        
        // Initialise TimeSeriesClient client with pre-configured kb client
        tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());

        dbUrl = postgres.getJdbcUrl();
        dbUsername = postgres.getUsername();
        dbPassword = postgres.getPassword();

        //Initialise mock timeseries in triple store and relational database
        classes = new ArrayList<>();
        classes.add(0, Double.class);
        classes.add(1, Double.class);
        tsClient.initTimeSeries(sashOpeningIRIs, classes, OffsetDateTime.class.getSimpleName(), Type.INSTANTANEOUS, null, null);
        tsClient.initTimeSeries(occupiedStateIRIs, classes, OffsetDateTime.class.getSimpleName(), Type.INSTANTANEOUS, null, null);
    }

    @Before
    public void addMockTimeSeriesData() {
        Double value = 1.0;

        Values = new ArrayList<>();
        Values.add(0, value);

        List<List<?>> values = new ArrayList<>();
        values.add(Values);
        values.add(Values);

        long timestampLong = System.currentTimeMillis();
        Date date = new java.util.Date(timestampLong);
        SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        Object ts = sdf.format(date);
        timestamp = ts.toString();
        times = new ArrayList<>();
        times.add(0, convertStringToOffsetDateTime(timestamp));
        TimeSeries<OffsetDateTime> sashTestTimeSeries = new TimeSeries<>(times, sashOpeningIRIs, values);
        TimeSeries<OffsetDateTime> occupiedStateTestTimeSeries = new TimeSeries<>(times, occupiedStateIRIs, values);
        tsClient.addTimeSeriesData(sashTestTimeSeries);
        tsClient.addTimeSeriesData(occupiedStateTestTimeSeries);
    }

    @Before
    public void createMockMaps() throws IOException {
        map1 = new HashMap<>();
        map1.put("FHandWFH", new ArrayList<>());
        map1.put("SashOpeningIRIs", new ArrayList<>());
        map1.put("OccupiedStateIRIs", new ArrayList<>());
        map1.get("FHandWFH").add("FH01 test IRI 1");
        map1.get("FHandWFH").add("FH02 test IRI 2");
        map1.get("FHandWFH").add("FH03 test IRI 3");
        map1.get("SashOpeningIRIs").add(examplePrefix+sashOpeningKey1);
        map1.get("OccupiedStateIRIs").add(examplePrefix+occupiedStateKey1);
        map1.get("SashOpeningIRIs").add("This device does not have a Sash Opening Percentage.");
        map1.get("OccupiedStateIRIs").add(examplePrefix+occupiedStateKey2);
        map1.get("SashOpeningIRIs").add(examplePrefix+sashOpeningKey3);
        map1.get("OccupiedStateIRIs").add("This device does not have a occupied state.");
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
    public void testGetOccupiedStateTsData() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
        agent = new FHSashAndOccupancyAgent();
        Method setTsClientAndRDBClient = FHSashAndOccupancyAgent.class.getDeclaredMethod("setTsClientAndRDBClient", String.class, String.class, String.class, String.class, String.class, String.class, String.class);
        setTsClientAndRDBClient.setAccessible(true);

        setTsClientAndRDBClient.invoke(agent, dbUsername, dbPassword, dbUrl, "bgUsername", "bgPassword", endpoint, endpoint);
        
        Map<String, List<String>> map = new HashMap<>();
    	map = agent.getOccupiedStateTsData(map1);

        Assert.assertEquals("1.0", map.get("OccupiedStateTsData").get(0));
        Assert.assertTrue(!map.get("OccupiedStateTimeStamps").get(0).contains("Not applicable"));
        Assert.assertEquals("1.0", map.get("OccupiedStateTsData").get(1));
        Assert.assertTrue(!map.get("OccupiedStateTimeStamps").get(1).contains("Not applicable"));
        Assert.assertTrue(map.get("OccupiedStateTsData").get(2).contains("This device does not have an occupied state."));
        Assert.assertTrue(map.get("OccupiedStateTimeStamps").get(2).contains("Not applicable"));
        
    }

    @Test
    public void testGetSashOpeningTsData() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException, NoSuchMethodException, SecurityException {
        agent = new FHSashAndOccupancyAgent();
        Method setTsClientAndRDBClient = FHSashAndOccupancyAgent.class.getDeclaredMethod("setTsClientAndRDBClient", String.class, String.class, String.class, String.class, String.class, String.class, String.class);
        setTsClientAndRDBClient.setAccessible(true);

        setTsClientAndRDBClient.invoke(agent, dbUsername, dbPassword, dbUrl, "bgUsername", "bgPassword", endpoint, endpoint);
        
        Map<String, List<String>> map = new HashMap<>();
    	map = agent.getSashOpeningTsData(map1);

        Assert.assertEquals("1.0", map.get("SashOpeningTsData").get(0));
        Assert.assertTrue(!map.get("SashOpeningTimeStamps").get(0).contains("Not applicable"));
        Assert.assertTrue(map.get("SashOpeningTsData").get(1).contains("This device does not have a sash opening."));
        Assert.assertTrue(map.get("SashOpeningTimeStamps").get(1).contains("Not applicable"));
        Assert.assertEquals("1.0", map.get("SashOpeningTsData").get(2));
        Assert.assertTrue(!map.get("SashOpeningTimeStamps").get(2).contains("Not applicable"));
        
    }

    /**
     * Converts a string into a datetime object with zone information using the zone globally define for the agent.
     * @param timestamp The timestamp as string, the format should be equal to 2007-12-03T10:15:30.
     * @return The resulting datetime object.
     */
    private OffsetDateTime convertStringToOffsetDateTime(String timestamp) {
        // Convert first to a local time
        LocalDateTime localTime = LocalDateTime.parse(timestamp);

        // Then add the zone id
        return OffsetDateTime.of(localTime, ZoneOffset.UTC);
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
    	
    
   
    
    
    

