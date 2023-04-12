package uk.ac.cam.cares.jps.agent.historicalntuenergy;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;

import com.github.stefanbirkner.systemlambda.SystemLambda;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.logging.Logger;

public class HistoricalNTUEnergyAgentTest {

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
    public void testConstructor() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        // Run constructor on an empty file should give an exception
        writePropertyFile(propertiesFile, new ArrayList<>());
        try {
            new HistoricalNTUEnergyAgent(propertiesFile);
            Assert.fail();
        } catch (IOException e) {
            Assert.assertEquals("The key ntuenergy.mappingfolder cannot be found in the properties file.", e.getMessage());
        }

        // Create a property file with a mapping folder that does not exist
        String folderName = "no_valid_folder";
        writePropertyFile(propertiesFile, Collections.singletonList("ntuenergy.mappingfolder=" + folderName));
        // Run constructor that should give an exception
        try {
            new HistoricalNTUEnergyAgent(propertiesFile);
            Assert.fail();
        } catch (InvalidPropertiesFormatException e) {
            Assert.assertEquals("The properties file does not contain the key ntuenergy.mappingfolder " +
                    "with a path to the folder containing the required JSON key to IRI mappings.", e.getMessage());
        }

        // Create an empty folder
        folderName = "mappings_test";
        File mappingFolder = folder.newFolder(folderName);
        // Create a property file with the empty folder
        folderName = mappingFolder.getCanonicalPath().replace("\\", "/");
        writePropertyFile(propertiesFile, Collections.singletonList("ntuenergy.mappingfolder=TEST_MAPPINGS"));
        // Run constructor that should give an exception
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                new HistoricalNTUEnergyAgent(propertiesFile);
                Assert.fail();
            });
        } catch (Exception e) {
            Assert.assertTrue(e.getMessage().contains("No files in the folder:"));
        }

        // Add mapping files into the empty folder
        // All IRIs set
        String firstMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "firstMapping.properties").toString();
        String[] keys = {"no2_slope", "no2_offset", "uart_prescaled", "temperature", "no2_prescaled", "so2_prescaled", "Humidity"};
        ArrayList<String> mappings = new ArrayList<>();
        for (String key : keys) {
            mappings.add(key + "=example:prefix/api_" + key);
        }
        writePropertyFile(firstMappingFile, mappings);
        // No IRIs set
        String secondMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "secondMapping.properties").toString();
        mappings = new ArrayList<>();
        for (String key : keys) {
            mappings.add(key + "=");
        }
        writePropertyFile(secondMappingFile, mappings);
        // Save the size of the files for assertions later
        long firstMappingFileSize = Files.size(Paths.get(firstMappingFile));
        long secondMappingFileSize = Files.size(Paths.get(secondMappingFile));
        // Create agent
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                HistoricalNTUEnergyAgent agent = new HistoricalNTUEnergyAgent(propertiesFile);
                // Assert that the mappings were set
                Assert.assertEquals(2, agent.getNumberOfTimeSeries());
            });
        } catch (Exception e) {
        }
        // Assert that the mappings were saved back (now bigger file size)
        Assert.assertTrue(Files.size(Paths.get(firstMappingFile)) > firstMappingFileSize);
        Assert.assertTrue(Files.size(Paths.get(secondMappingFile)) > secondMappingFileSize);
    }

    @Test
    public void testGetClassFromJSONKey() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make private method accessible
        Method getClassFromJSONKey = HistoricalNTUEnergyAgent.class.getDeclaredMethod("getClassFromJSONKey", String.class);
        getClassFromJSONKey.setAccessible(true);
        Assert.assertEquals(String.class, getClassFromJSONKey.invoke(agent, "TIME"));
    }

    @Test
    public void testTimeSeriesExistAllIRIsTrue() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Initialize time series
        agent.initializeTimeSeriesIfNotExist();
        // Make method accessible
        Method timeSeriesExist = HistoricalNTUEnergyAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
        timeSeriesExist.setAccessible(true);
        // Empty list should return true
        Assert.assertTrue((Boolean) timeSeriesExist.invoke(agent, new ArrayList<String>()));
        // Should return true as all IRIs are attached
        Assert.assertTrue((Boolean) timeSeriesExist.invoke(agent, energyIRIs));
    }

    @Test
    public void testTimeSeriesExistAllOneIRIFalse() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method timeSeriesExist = HistoricalNTUEnergyAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
        timeSeriesExist.setAccessible(true);
        // Should return false as one IRI is not attached
        Assert.assertFalse((Boolean) timeSeriesExist.invoke(agent, energyIRIs));
        agent.initializeTimeSeriesIfNotExist();
        // Should return true as all IRIs are attached
        Assert.assertTrue((Boolean) timeSeriesExist.invoke(agent, energyIRIs));
    }

    @Test
    public void testUpdateDataExceptions() {
        // Initialize readings
        JSONArray energyReadings = new JSONArray("[]");
        // Using empty readings should throw an exception
        try {
            agent.updateData(energyReadings);
            Assert.fail();
        } catch (IllegalArgumentException e) {
            Assert.assertEquals("Readings can not be empty!", e.getMessage());
        }
    }

    @Test
    public void testJsonArrayToMapEmptyReadings() throws NoSuchMethodException, InvocationTargetException,
            IllegalAccessException {
        JSONArray readings = new JSONArray("[]");
        // Make method accessible
        Method jsonArrayToMap = HistoricalNTUEnergyAgent.class.getDeclaredMethod("jsonArrayToMap", JSONArray.class);
        jsonArrayToMap.setAccessible(true);
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readingsMap = (Map<String, List<?>>) jsonArrayToMap.invoke(agent, readings);
        // The map should be empty
        Assert.assertTrue(readingsMap.isEmpty());
    }

    @Test
    public void testJsonArrayToMapEnergyReadings() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method jsonArrayToMap = HistoricalNTUEnergyAgent.class.getDeclaredMethod("jsonArrayToMap", JSONArray.class);
        jsonArrayToMap.setAccessible(true);
        // Transform the readings
        Map<String, List<?>> readings = (Map<String, List<?>>) jsonArrayToMap.invoke(agent, energyReadings);
        // Check that all keys have a list of the same size as the JSON Array
        for (String key : readings.keySet()) {
            Assert.assertEquals(energyReadings.length(), readings.get(key).size());
        }
        // Check that all keys from the JSON Array have a corresponding entry
        for (Iterator<String> it = energyReadings.getJSONObject(0).keys(); it.hasNext(); ) {
            String key = it.next();
            Assert.assertTrue(readings.containsKey(key));
        }
    }

    @Test
    public void testConvertReadingsToTimeSeries() throws IOException, NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {
        // Create an agent with mappings of small size //
        // Create a folder inside the temporary folder in which the mapping files will be
        File mappingFolder = folder.newFolder("mappings_test");
        // Define three sets of mappings
        String[] generalKeys = {"key1", "key2", "key3"};
        String[] energyKeys = {"gkey1", "gkey2", "gkey3", "gkey4"};
        Map<String, String[]> keys = new HashMap<>();
        keys.put("general", generalKeys);
        keys.put("energy", energyKeys);
        // Create a file for each mapping
        for (String mappingName : keys.keySet()) {
            String filepath = Paths.get(mappingFolder.getCanonicalPath(), mappingName + ".properties").toString();
            try (FileWriter writer = new FileWriter(filepath, false)) {
                for (String key : keys.get(mappingName)) {
                    writer.write(key + "=\n");
                }
            }
        }
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writePropertyFile(propertiesFile, Collections.singletonList("ntuenergy.mappingfolder=TEST_MAPPINGS"));
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                HistoricalNTUEnergyAgent agent = new HistoricalNTUEnergyAgent(propertiesFile);
                // Assert that the mappings were set

                // Create the readings //
                // Energy readings
                String[] energyTimestamps = {"2021-07-11T16:20:00", "2021-07-11T16:25:00", "2021-07-11T16:30:00"};
                Map<String, List<?>> energyReading = new HashMap<>();
                energyReading.put(HistoricalNTUEnergyAgent.timestampKey, Arrays.asList(energyTimestamps));

                // Make method accessible
                Method convertReadingsToTimeSeries = HistoricalNTUEnergyAgent.class.getDeclaredMethod("convertReadingsToTimeSeries", Map.class);
                convertReadingsToTimeSeries.setAccessible(true);

                // Use readings only consisting of times, should give an error as keys are not covered
                try {
                    convertReadingsToTimeSeries.invoke(agent, energyReading);
                    Assert.fail();
                } catch (InvocationTargetException e) {
                    Assert.assertEquals(JPSRuntimeException.class, e.getCause().getClass());
                }
            });
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

    }


}
