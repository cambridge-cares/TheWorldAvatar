package uk.ac.cam.cares.jps.agent.nusDavisWeatherStation;

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
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.util.*;

@Ignore("Requires both triple store endpoint and postgreSQL database set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")
@Testcontainers
public class NUSDavisWeatherStationInputAgentIntegrationTest {
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

    // NUSDavisWeatherStation Input Agent
    private NUSDavisWeatherStationInputAgent agent;
    // Time series client for connection with KG and database
    private TimeSeriesClient<OffsetDateTime> tsClient;

    // Default lists of JSON keys (also defining the type)

   private final String[] keys = {"temp_in","dew_point","heat_index","wind_chill"};

    // Example prefix for IRIs
    private final String examplePrefix = "example:prefix/api_";
    // IRIs corresponding to the keys
    private ArrayList<String> IRIs;

    // Default list of timestamps
    private final String[] timestamps = {"1558729481","1558829481","1558929481","1559029481"};


    // Values created as example readings
    private ArrayList<Double> weatherValues;

    // Readings used by several tests
    JSONObject weatherDataReadings;

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
        String weatherMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "weather.properties").toString();
        ArrayList<String> weatherMappings = new ArrayList<>();
        IRIs = new ArrayList<>();
        for (String key: keys) {
            weatherMappings.add(key + "="+examplePrefix+key);
            IRIs.add(examplePrefix+key);
        }
        writePropertyFile(weatherMappingFile, weatherMappings);
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writePropertyFile(propertiesFile, Collections.singletonList("nusDavisWeatherStation.mappingfolder=TEST_MAPPINGS"));
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                agent = new NUSDavisWeatherStationInputAgent(propertiesFile);
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
        tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, null, "postgres", "postgres");
        // Configure database access
        tsClient.setRDBClient(postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
        // Set client for agent
        agent.setTsClient(tsClient);
    }
    @Before
    public void createExampleReadings() {
        double value =0.0;
        weatherDataReadings = new JSONObject();
        weatherValues = new ArrayList<>();
        JSONArray sensors= new JSONArray();
        JSONArray data= new JSONArray();
        JSONObject jsObj1= new JSONObject();

        weatherDataReadings.put("stationId",12345);
        jsObj1.put( "lsid",396862);
        jsObj1.put("sensor_type",50);
        jsObj1.put("data_structure_type",2);

        for(int i=0; i<timestamps.length;i++) {
            JSONObject measurements = new JSONObject();
            measurements.put(NUSDavisWeatherStationInputAgent.timestampKey,Long.parseLong(timestamps[i]) );
            for(String key: keys) {
                measurements.put(key, value);
            }
            data.put(measurements);
            weatherValues.add((5.0 * (value - 32.0)) / 9.0);
            value++;
        }
        jsObj1.put("data",data);
        sensors.put(jsObj1);
        weatherDataReadings.put("sensors",sensors);


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
        // Update time-series data
        agent.updateData(weatherDataReadings);
        // Check that database was updated
        TimeSeries<OffsetDateTime> ts = tsClient.getTimeSeries(IRIs);
        JSONArray getSensor=weatherDataReadings.getJSONArray("sensors");
        JSONObject objSensor=getSensor.getJSONObject(0);
        JSONArray getData=objSensor.getJSONArray("data");

        Assert.assertEquals(getData.length(), ts.getTimes().size());

        for (String iri: IRIs) {
            Assert.assertEquals(weatherValues, ts.getValues(iri));
        }
        for (int i=0;i< timestamps.length;++i){

            Long timestamp = Long.parseLong(timestamps[i]);
            Date date = new java.util.Date(timestamp*1000);
            SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
            String dateTime = sdf.format(date);
            timestamps[i]=dateTime;
        }
        // Assert timestamps
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[0]), NUSDavisWeatherStationInputAgent.ZONE_OFFSET)
                .isEqual(tsClient.getMinTime(IRIs.get(0))));
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[timestamps.length-1]), NUSDavisWeatherStationInputAgent.ZONE_OFFSET)
                .isEqual(tsClient.getMaxTime(IRIs.get(0))));
    }

    @Test
    public void testUpdateTimeSeriesWithEmptyReadings() {
        insertTimeSeries();
        // Update time-series data (should throw an error)
        try {
            agent.updateData(new JSONObject());
            Assert.fail();
        }
        catch(Exception e){
            Assert.assertTrue(e.getCause().getMessage().contains("Readings can not be empty!"));
        }
    }

    @Test
    public void testUpdateTimeSeriesWithPruning() {
        insertTimeSeries();
        // Add data for weather readings up to last reading
        List<OffsetDateTime> times = new ArrayList<>();
        for (int i=0;i< timestamps.length;++i){
            Long timestamp = Long.parseLong(timestamps[i]);
            Date date = new java.util.Date(timestamp*1000);
            SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
            String dateTime = sdf.format(date);
            timestamps[i]=dateTime;
        }
        for(int i = 0; i < timestamps.length ; i++) {
            if (i < (timestamps.length-1)) {
                times.add(OffsetDateTime.of(LocalDateTime.parse(timestamps[i]), NUSDavisWeatherStationInputAgent.ZONE_OFFSET));
            }
        }
        TimeSeries<OffsetDateTime> ts = new TimeSeries<>(times, IRIs,
                Collections.nCopies(IRIs.size(), weatherValues.subList(0, weatherValues.size()-1)));
        tsClient.addTimeSeriesData(ts);
        // Update data through agent
        agent.updateData(weatherDataReadings);
        // Check that database was updated and existing data is untouched
        ts = tsClient.getTimeSeries(IRIs);
        JSONArray getSensor=weatherDataReadings.getJSONArray("sensors");
        JSONObject objSensor=getSensor.getJSONObject(0);
        JSONArray getData=objSensor.getJSONArray("data");
        Assert.assertEquals(getData.length(),ts.getTimes().size());
        // Check that data content is correct
        for (String iri: IRIs) {
            Assert.assertEquals(weatherValues, ts.getValues(iri));
        }
        // Assert timestamps
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[0]), NUSDavisWeatherStationInputAgent.ZONE_OFFSET)
                .isEqual(tsClient.getMinTime(IRIs.get(0))));
        Assert.assertTrue(OffsetDateTime.of(LocalDateTime.parse(timestamps[timestamps.length-1]), NUSDavisWeatherStationInputAgent.ZONE_OFFSET)
                .isEqual(tsClient.getMaxTime(IRIs.get(0))));
    }

    private void insertTimeSeries() {
        // Insert weather data time-series
        List<Class<?>> classes = Collections.nCopies(IRIs.size(), Double.class);
        tsClient.initTimeSeries(IRIs, classes, "timeUnit");
    }
}
