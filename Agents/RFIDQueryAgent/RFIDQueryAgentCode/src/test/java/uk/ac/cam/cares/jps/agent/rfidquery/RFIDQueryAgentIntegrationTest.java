package uk.ac.cam.cares.jps.agent.rfidquery;

import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

/**
 * This test class is to test the RFID Query agent with a running KG and postgres database.
 */


@Ignore("Requires both triple store endpoint and postgreSQL database set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")
        

@Testcontainers
public class RFIDQueryAgentIntegrationTest {

    // Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);
    // Create Docker container with postgres 13.3 image from Docker Hub
    @Container
    private final PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");

    @Rule
    //temp folder for temp client.properties file
    public TemporaryFolder folder = new TemporaryFolder();

    //RFIDQueryAgent
    private RFIDQueryAgent agent;

    //tsClient
    private TimeSeriesClient<OffsetDateTime> tsClient;

    //single key for mocking RFID tag status data IRI
    private final String key = "tag_01_status";

    // Example prefix for IRIs
    private final String examplePrefix = "example:prefix/api_";

    //endpoint
    String endpoint;

    // list of IRIs
    private ArrayList<String> IRIs;
    private ArrayList<String> testIRIs;

    //lists of date time
    private List<OffsetDateTime> times;

    // Values created as example readings
    private ArrayList<String> Values;

    // Readings used by several tests
    JSONObject allReadings;

    //timestamp
    String timestamp;
    
    //list of classes to initialize timeseries
    private List<Class<?>> classes;
 	
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
        IRIs = new ArrayList<>();
        IRIs.add(examplePrefix+key);
       
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
        tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, null, "postgres", "cares1010");
         
        // Configure database access
        tsClient.setRDBClient(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());

        //Initialise mock timeseries in triple store and relational database
        classes = new ArrayList<>();
        classes.add(0, String.class);
        tsClient.initTimeSeries(IRIs, classes, OffsetDateTime.class.getSimpleName());
        
        agent = new RFIDQueryAgent(IRIs.get(0), "10");
    }

    @Before
    public void addMockTimeSeriesData() {
        String value = "Out";

        Values = new ArrayList<>();
        Values.add(0, value);

        List<List<?>> values = new ArrayList<>();
        values.add(Values);

        long timestampLong = System.currentTimeMillis();
        Date date = new java.util.Date(timestampLong);
        SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        Object ts = sdf.format(date);
        timestamp = ts.toString();
        
        testIRIs = new ArrayList<>();
        testIRIs.add(examplePrefix+key);
        times = new ArrayList<>();
        times.add(0, convertStringToOffsetDateTime(timestamp));
        TimeSeries<OffsetDateTime> testTimeSeries = new TimeSeries<>(times, testIRIs, values);
        tsClient.addTimeSeriesData(testTimeSeries);
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
    public void querylatestRFIDStatusFail() throws IOException {
    	try {
    		agent.queryLatestRFIDStatus("wrong_data_iri");
    		Assert.fail();
    	} catch (JPSRuntimeException e) {
    		Assert.assertEquals("Unable to query for latest data!", e.getMessage());
    	}
    }
    
    @Test
    public void queryLatestRFIDStatusSuccess() throws IOException {
        RemoteStoreClient kbClient = new RemoteStoreClient();
    	kbClient.setQueryEndpoint(endpoint);
    	kbClient.setUpdateEndpoint(endpoint);
    	IRIs = new ArrayList<>();
        IRIs.add(examplePrefix+key);

        //create rdbclient
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
        agent.setRDBClient(rdbStoreClient);
        agent.setTsClient(tsClient);
        TimeSeries<OffsetDateTime> timeseries = agent.queryLatestRFIDStatus(IRIs.get(0));

        Assert.assertEquals("Out", timeseries.getValuesAsString(IRIs.get(0)).get(0));
        Assert.assertTrue(timeseries.getTimes().get(0).toString(), true);
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
    	
    
   
    
    
    

