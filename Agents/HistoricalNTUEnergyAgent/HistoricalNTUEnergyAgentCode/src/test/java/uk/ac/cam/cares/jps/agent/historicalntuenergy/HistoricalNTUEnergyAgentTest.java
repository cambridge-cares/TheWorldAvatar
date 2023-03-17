package uk.ac.cam.cares.jps.agent.historicalntuenergy;

import com.github.stefanbirkner.systemlambda.SystemLambda;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;
import org.junit.Before;
import org.junit.Rule;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;

import java.io.IOException;
import java.io.File;
import java.io.FileWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.OffsetDateTime;
import java.util.*;

import static org.junit.Assert.*;

public class HistoricalNTUEnergyAgentTest {

    // Temporary folder to place a properties file
    private static final Logger LOGGER = LogManager.getLogger(HistoricalNTUEnergyAgentLauncher.class);
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // The default instance used in the tests
    private HistoricalNTUEnergyAgent testAgent;
    // The mocking instance for the time series client
    @SuppressWarnings("unchecked")
    private final TimeSeriesClient<OffsetDateTime> mockTSClient = (TimeSeriesClient<OffsetDateTime>) Mockito.mock(TimeSeriesClient.class);

    // A default list of IRIs
    private final List<String> iris = Arrays.asList("iri1", "iri2", "iri3");
    // Default list of JSON keys
    private final String[] keys = {"key1", "key2" ,"key3"};
    // Default list of timestamps
    private final String[] timestamps = {"2021-07-11T16:10:00", "2021-07-11T16:15:00", "2021-07-11T16:20:00", "2021-07-11T16:25:00"};

    // Readings used by several tests
    JSONArray energyReadings;

    @Before
    public void initializeAgent() throws IOException {
        // Create a properties file that points to a dummy mapping folder //
        // Create an empty folder
        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Add mapping file into the empty folder
        String mappingFile = Paths.get(mappingFolder.getAbsolutePath(), "example_mapping.properties").toString();
        ArrayList<String> mappings = new ArrayList<>();
        for (String key: keys) {
            mappings.add(key + "=example:prefix/api_" + key);
        }
        writePropertyFile(mappingFile, mappings);
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writePropertyFile(propertiesFile, Collections.singletonList("ntuenergy.mappingfolder=TEST_MAPPINGS"));
        // To create testAgent without an exception being thrown, SystemLambda is used to mock an environment variable
        // To mock the environment variable, a try catch need to be used
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                testAgent = new HistoricalNTUEnergyAgent(propertiesFile);
            });
        }
        // There should not be any exception thrown as the agent is initiated correctly
        catch (Exception e) {
        }
        // Set the mocked time series client
        testAgent.setTsClient(mockTSClient);
    }

    @Before
    public void createExampleReadings() {

        energyReadings = new JSONArray();

        double value = 0.0;
        for(String timestamp: timestamps) {
            JSONObject currentEnergyMeasures = new JSONObject();
            // Put the timestamp in the current reading
            currentEnergyMeasures.put(HistoricalNTUEnergyAgent.timestampKey, timestamp);
            // Put values for each key into the current readings
            for(String key: keys) {
                currentEnergyMeasures.put(key, value);
            }
            energyReadings.put(currentEnergyMeasures);
            value++;
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
        }

        catch (IOException e) {
            Assert.assertEquals("The key ntuenergy.mappingfolder cannot be found in the properties file.", e.getMessage());
        }

        // Create a property file with a mapping folder that does not exist
        String folderName = "no_valid_folder";
        writePropertyFile(propertiesFile, Collections.singletonList("ntuenergy.mappingfolder=" + folderName));
        // Run constructor that should give an exception
        try {
            new HistoricalNTUEnergyAgent(propertiesFile);
            Assert.fail();
        }
        catch (InvalidPropertiesFormatException e) {
            Assert.assertEquals("The properties file does not contain the key ntuenergy.mappingfolder " +
                    "with a path to the folder containing the required JSON key to IRI mappings.", e.getMessage());
        }

        // Create an empty folder
        folderName = "mappings_test";
        File mappingFolder = folder.newFolder(folderName);
        // Create a property file with the empty folder
        folderName = mappingFolder.getCanonicalPath().replace("\\","/");
        writePropertyFile(propertiesFile, Collections.singletonList("ntuenergy.mappingfolder=TEST_MAPPINGS"));
        // Run constructor that should give an exception
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                new HistoricalNTUEnergyAgent(propertiesFile);
                Assert.fail();
            });
        }
        catch (Exception e) {
            Assert.assertTrue(e.getMessage().contains("No files in the folder:"));
        }

        // Add mapping files into the empty folder
        // All IRIs set
        String firstMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "firstMapping.properties").toString();
        String[] keys = {"NEC_P_KW", "NEC_Q_KVAR" ,"CANTEEN_2_P_KW", "CANTEEN_2_Q_KVAR", "SPMS_P_KW", "SPMS_Q_KVAR", "RTP_P_KW"};
        ArrayList<String> mappings = new ArrayList<>();
        for (String key: keys) {
            mappings.add(key + "=example:prefix/api_" + key);
        }
        writePropertyFile(firstMappingFile, mappings);
        // No IRIs set
        String secondMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "secondMapping.properties").toString();
        mappings = new ArrayList<>();
        for (String key: keys) {
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
        }
        catch (Exception e) {
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
        // No specific key should return the string class
        Assert.assertEquals(String.class, getClassFromJSONKey.invoke(testAgent, "TIME"));
        // Readings should return double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "NEC_P_KW"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "NEC_Q_KVAR"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "CANTEEN_2_P_KW"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "CANTEEN_2_Q_KVAR"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "SPMS_P_KW"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "SPMS_Q_KVAR"));
    }

    @Test
    public void testTimeSeriesExistAllIRIsTrue() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method timeSeriesExist = HistoricalNTUEnergyAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
        timeSeriesExist.setAccessible(true);
        // Set the mock to return true for any IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(Mockito.anyString())).thenReturn(true);
        // Empty list should return true
        Assert.assertTrue((Boolean) timeSeriesExist.invoke(testAgent, new ArrayList<String>()));
        // Should return true as all IRIs are attached (based on the mock)
        Assert.assertTrue((Boolean) timeSeriesExist.invoke(testAgent, iris));
        // Check also that the check was invoked for all keys
        for (String iri : iris) {
            Mockito.verify(mockTSClient, Mockito.times(1)).checkDataHasTimeSeries(iri);
        }
    }

    @Test
    public void testTimeSeriesExistAllOneIRIFalse() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method timeSeriesExist = HistoricalNTUEnergyAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
        timeSeriesExist.setAccessible(true);
        // Set the mock to return false on second IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(iris.get(0))).thenReturn(true);
        Mockito.when(mockTSClient.checkDataHasTimeSeries(iris.get(1))).thenReturn(false);
        // Should return false as second IRIs is not attached (based on the mock)
        Assert.assertFalse((Boolean) timeSeriesExist.invoke(testAgent, iris));
    }

    @Test
    public void testTimeSeriesExistAllIRIFalse() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method timeSeriesExist = HistoricalNTUEnergyAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
        timeSeriesExist.setAccessible(true);
        // Set the mock to return false for any IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(Mockito.anyString())).thenReturn(false);
        // Should return false as no IRI is attached (based on the mock)
        Assert.assertFalse((Boolean) timeSeriesExist.invoke(testAgent, iris));
        // Should have returned false after first IRI
        Mockito.verify(mockTSClient, Mockito.times(1)).checkDataHasTimeSeries(iris.get(0));
        Mockito.verify(mockTSClient, Mockito.never()).checkDataHasTimeSeries(iris.get(1));
        Mockito.verify(mockTSClient, Mockito.never()).checkDataHasTimeSeries(iris.get(2));
    }

    @Test
    public void testInitializeTimeSeriesIfNotExistCreateAll() {
        // Set the mock to return false for any IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(Mockito.anyString())).thenReturn(false);
        // Run the initialization method
        testAgent.initializeTimeSeriesIfNotExist();
        // Should have invoked the time series initialization for each mapping
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries()))
                .initTimeSeries(Mockito.anyList(), Mockito.anyList(), Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any());
    }

    @Test
    public void testInitializeTimeSeriesIfNotExistCreateNone() {
        // Set the mock to return true for any IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(Mockito.anyString())).thenReturn(true);
        // Run the initialization method
        testAgent.initializeTimeSeriesIfNotExist();
        // Should not have invoked the time series initialization for any mapping
        Mockito.verify(mockTSClient, Mockito.never())
                .initTimeSeries(Mockito.anyList(), Mockito.anyList(), Mockito.anyString());
    }

    @Test
    public void testUpdateDataExceptions() {
        /*
        // Initialize readings
        JSONArray particleReadings = new JSONArray("[]");
        JSONArray gasReadings = new JSONArray("[]");

        // Using empty readings should throw an exception
        try {
            testAgent.updateData(energyReadings);
            Assert.fail();
        }
        catch (IllegalArgumentException e) {
            Assert.assertEquals("Readings can not be empty!", e.getMessage());
        }

        // Create readings with timestamps and missing keys
        for(String timestamp: timestamps) {
            String json = "{ '" + HistoricalNTUEnergyAgent.timestampKey + "':'" + timestamp + "'}";
            particleReadings.put(new JSONObject(json));
            gasReadings.put(new JSONObject(json));

        }
        // Should trigger an exception due to missing keys
        try {
            testAgent.updateData(energyReadings);
            Assert.fail();
        }
        catch (IllegalArgumentException e) {
            Assert.assertEquals("Readings can not be converted to proper time series!", e.getMessage());
            Assert.assertEquals(NoSuchElementException.class, e.getCause().getClass());
        }
        */
    }

    @Test
    public void testUpdateData() {
        /*
        // Set up the mock client
        // Use a max time that is clearly before any of the example readings
        Mockito.when(mockTSClient.getMaxTime(Mockito.anyString())).thenReturn(OffsetDateTime.parse("1988-07-10T00:50:00+00:00"));
        // Run the update
        testAgent.updateData(energyReadings);
        // Capture the arguments that the add data method was called with
        @SuppressWarnings("unchecked")
        ArgumentCaptor<TimeSeries<OffsetDateTime>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        // Ensure that the update was called for each time series
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries())).addTimeSeriesData(timeSeriesArgument.capture());
        // Ensure that the timeseries objects have the correct structure
        int numIRIs = 0;
        for(TimeSeries<OffsetDateTime> ts: timeSeriesArgument.getAllValues()) {
            // Check that number of timestamps is correct
            Assert.assertEquals(energyReadings.length(), ts.getTimes().size());
            numIRIs = numIRIs + ts.getDataIRIs().size();
        }
        // Number of unique keys in both readings should match the number of IRIs
        Set<String> uniqueKeys = new HashSet<>(particleReadings.getJSONObject(0).keySet());
        uniqueKeys.addAll(gasReadings.getJSONObject(0).keySet());
        // Timestamp key has no match (therefore -1)
        Assert.assertEquals(uniqueKeys.size() - 1, numIRIs);

         */
    }

    @Test
    public void testReadMappingsWithValidFolderAndOneFile() throws IOException {
        /*
        File mappingFile = tempFolder.newFile("test.json");
        FileWriter writer = new FileWriter(mappingFile);
        writer.write("{\"key1\":\"http://example.com/iri1\",\"key2\":\"http://example.com/iri2\"}");
        writer.close();
        ArrayList<JSONKeyToIRIMapper> mappings = agent.readMappings(tempFolder.getRoot().getAbsolutePath());
        assertEquals(1, mappings.size());
        JSONKeyToIRIMapper mapper = mappings.get(0);
        assertNotNull(mapper);
        assertEquals("http://example.com/iri1", mapper.getIRI("key1"));
        assertEquals("http://example.com/iri2", mapper.getIRI("key2"));
         */
    }

    @Test
    public void getNumberOfTimeSeries() {
    }

    @Test
    public void setTsClient() {
    }

    @Test
    public void initializeTimeSeriesIfNotExist() {
    }

    @Test
    public void updateData() {
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