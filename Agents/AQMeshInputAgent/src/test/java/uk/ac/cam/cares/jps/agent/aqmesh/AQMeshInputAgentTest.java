package uk.ac.cam.cares.jps.agent.aqmesh;

import org.json.JSONArray;
import org.json.JSONTokener;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.time.ZonedDateTime;
import java.util.*;

public class AQMeshInputAgentTest {

    // Temporary folder to place a properties file (same file for all potential tests)
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // The default instance used in the tests
    private AQMeshInputAgent testAgent;
    // The mocking instance for the time series client
    @SuppressWarnings("unchecked")
    private final TimeSeriesClient<ZonedDateTime> mockTSClient = (TimeSeriesClient<ZonedDateTime>) Mockito.mock(TimeSeriesClient.class);

    // A default list of IRIs
    private final List<String> iris = Arrays.asList("iri1", "iri2", "iri3");

    @Before
    public void initializeAgent() throws URISyntaxException, IOException {
        // Create a properties file that points to the example/test mapping folder in the resources //
        String mappingFolder = Paths.get(Objects.requireNonNull(getClass().getResource("/mappings"))
                .toURI()).toString().replace("\\","/");
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writePropertyFile(propertiesFile, Collections.singletonList("aqmesh.mappingfolder=" + mappingFolder));
        testAgent = new AQMeshInputAgent(propertiesFile);
        // Set the mocked time series client
        testAgent.setTsClient(mockTSClient);
    }

    @Test
    public void testConstructor() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "aqmesh.properties").toString();
        // Create a property file with a mapping folder that does not exist
        String folderName = "no_valid_folder";
        writePropertyFile(propertiesFile, Collections.singletonList("aqmesh.mappingfolder=" + folderName));
        // Run constructor that should give an exception
        try {
            new AQMeshInputAgent(propertiesFile);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertTrue(e.getMessage().contains("Folder does not exist:"));
            Assert.assertTrue(e.getMessage().contains(folderName));
        }

        // Create an empty folder
        folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Create a property file with the empty folder
        folderName = mappingFolder.getCanonicalPath().replace("\\","/");
        writePropertyFile(propertiesFile, Collections.singletonList("aqmesh.mappingfolder=" + folderName));
        // Run constructor that should give an exception
        try {
            new AQMeshInputAgent(propertiesFile);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertTrue(e.getMessage().contains("No files in the folder:"));
            Assert.assertTrue(e.getMessage().contains(folderName));
        }

        // Add mapping files into the empty folder
        // All IRIs set
        String firstMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "firstMapping.properties").toString();
        String[] keys = {"key1", "key2" ,"key3"};
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
        // Create agent
        AQMeshInputAgent agent = new AQMeshInputAgent(propertiesFile);
        // Assert that the mappings were set
        Assert.assertEquals(2, agent.getNumberOfTimeSeries());
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
    public void testGetClassFromJSONKey() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make private method accessible
        Method getClassFromJSONKey = AQMeshInputAgent.class.getDeclaredMethod("getClassFromJSONKey", String.class);
        getClassFromJSONKey.setAccessible(true);
        // No specific key should return the string class
        Assert.assertEquals(String.class, getClassFromJSONKey.invoke(testAgent, "key"));
        // Interval should return integer class
        Assert.assertEquals(Integer.class, getClassFromJSONKey.invoke(testAgent, "key_p1"));
        // Voltage should return double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "key_voltage"));
        // Environment conditions should be double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "temperature"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "pressure"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "humidity"));
        // Noise readings should be double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "key_noise"));
        // Sensor readings should be double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "key_prescaled"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "key_prescale"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "key_slope"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "key_offset"));
        // Battery low warning and particle modem overlap should be boolean
        Assert.assertEquals(Boolean.class, getClassFromJSONKey.invoke(testAgent, "battery_low"));
        Assert.assertEquals(Boolean.class, getClassFromJSONKey.invoke(testAgent, "particle_modem_overlap"));
    }

    @Test
    public void testTimeSeriesExistAllIRIsTrue() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method timeSeriesExist = AQMeshInputAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
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
        Method timeSeriesExist = AQMeshInputAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
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
        Method timeSeriesExist = AQMeshInputAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
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
                .initTimeSeries(Mockito.anyList(), Mockito.anyList(), Mockito.anyString());
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
    public void testJsonArrayToMapGasReadings() throws IOException, NoSuchMethodException, InvocationTargetException,
            IllegalAccessException, URISyntaxException {
        // Get the example JSON
        JSONArray gasReadings;
        String readingsFile = Paths.get(Objects.requireNonNull(getClass().getResource("/example_gas.json"))
                .toURI()).toString();
        try (InputStream input = new FileInputStream(readingsFile)) {
            JSONTokener tokener = new JSONTokener(input);
            gasReadings = new JSONArray(tokener);
        }
        // Make method accessible
        Method jsonArrayToMap = AQMeshInputAgent.class.getDeclaredMethod("jsonArrayToMap", JSONArray.class);
        jsonArrayToMap.setAccessible(true);
        // Transform the readings
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readings = (Map<String, List<?>>) jsonArrayToMap.invoke(testAgent, gasReadings);
        // Check that all keys have a list of the same size as the JSON Array
        for (String key: readings.keySet()) {
            Assert.assertEquals(gasReadings.length(), readings.get(key).size());
        }
        // Check that all key from the JSON Array have a corresponding entry
        for (Iterator<String> it = gasReadings.getJSONObject(0).keys(); it.hasNext();) {
            String key = it.next();
            Assert.assertTrue(readings.containsKey(key));
        }
    }

    @Test
    public void testJsonArrayToMapParticleReadings() throws IOException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException, URISyntaxException {
        // Get the example JSON
        JSONArray particleReadings;
        String readingsFile = Paths.get(Objects.requireNonNull(getClass().getResource("/example_particle.json"))
                .toURI()).toString();
        try (InputStream input = new FileInputStream(readingsFile)) {
            JSONTokener tokener = new JSONTokener(input);
            particleReadings = new JSONArray(tokener);
        }
        // Make method accessible
        Method jsonArrayToMap = AQMeshInputAgent.class.getDeclaredMethod("jsonArrayToMap", JSONArray.class);
        jsonArrayToMap.setAccessible(true);
        // Transform the readings
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readings = (Map<String, List<?>>) jsonArrayToMap.invoke(testAgent, particleReadings);
        // Check that all keys have a list of the same size as the JSON Array
        for (String key: readings.keySet()) {
            Assert.assertEquals(particleReadings.length(), readings.get(key).size());
        }
        // Check that all key from the JSON Array have a corresponding entry
        for (Iterator<String> it = particleReadings.getJSONObject(0).keys(); it.hasNext();) {
            String key = it.next();
            Assert.assertTrue(readings.containsKey(key));
        }
    }

    @Test
    public void testConvertReadingsToTimeSeries() throws IOException, NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {
        // Create an agent with mappings of small size //
        // Create a folder inside the temporary folder in which the mapping files will be
        File mappingFolder= folder.newFolder("mappings");
        // Define three sets of mappings
        String[] generalKeys = {"key1", "key2", "key3"};
        String[] gasKeys = {"gkey1", "gkey2", "gkey3", "gkey4"};
        String[] particleKeys = {"pkey1", "pkey2"};
        Map<String, String[]> keys = new HashMap<>();
        keys.put("general", generalKeys);
        keys.put("gas", gasKeys);
        keys.put("particle", particleKeys);
        // Create a file for each mapping
        for (String mappingName: keys.keySet()) {
            String filepath = Paths.get(mappingFolder.getCanonicalPath(), mappingName+".properties").toString();
            try(FileWriter writer = new FileWriter(filepath, false)) {
                for (String key: keys.get(mappingName)) {
                    writer.write(key + "=\n");
                }
            }
        }
        // Create a properties file that points to the created mapping folder
        String mappingFolderPath = mappingFolder.getCanonicalPath().replace("\\","/");
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writePropertyFile(propertiesFile, Collections.singletonList("aqmesh.mappingfolder=" + mappingFolderPath));
        // Create agent
        AQMeshInputAgent agent = new AQMeshInputAgent(propertiesFile);

        // Create the readings //
        // Particle readings
        String[] particleTimestamps = {"2021-07-11T16:10:00", "2021-07-11T16:15:00",
                "2021-07-11T16:20:00", "2021-07-11T16:25:00"};
        Map<String, List<?>> particleReadings = new HashMap<>();
        particleReadings.put(AQMeshInputAgent.timestampKey, Arrays.asList(particleTimestamps));
        // Gas readings
        String[] gasTimestamps = {"2021-07-11T16:20:00", "2021-07-11T16:25:00", "2021-07-11T16:30:00"};
        Map<String, List<?>> gasReadings = new HashMap<>();
        gasReadings.put(AQMeshInputAgent.timestampKey, Arrays.asList(gasTimestamps));

        // Make method accessible
        Method convertReadingsToTimeSeries = AQMeshInputAgent.class.getDeclaredMethod("convertReadingsToTimeSeries", Map.class, Map.class);
        convertReadingsToTimeSeries.setAccessible(true);

        // Use readings only consisting of times, should give an error as keys are not covered
        try {
            convertReadingsToTimeSeries.invoke(agent, particleReadings, gasReadings);
            Assert.fail();
        }
        catch (InvocationTargetException e) {
            Assert.assertEquals(NoSuchElementException.class, e.getCause().getClass());
            Assert.assertTrue(e.getCause().getMessage().contains("The key"));
            Assert.assertTrue(e.getCause().getMessage().contains("is not contained in the readings!"));
        }

        // Add actual measures to the readings //
        // Add general information to particle readings
        for(String key: generalKeys) {
            List<String> values = new ArrayList<>();
            for(int i = 0; i < particleTimestamps.length; i++) {
                values.add(String.valueOf(i));
            }
            particleReadings.put(key, values);
        }
        // Add particle information to particle readings
        for(String key: particleKeys) {
            List<Double> values = new ArrayList<>();
            for(int i = 0; i < particleTimestamps.length; i++) {
                values.add((double) i + 0.2);
            }
            particleReadings.put(key, values);
        }
        // Add gas information to gas readings
        for(String key: gasKeys) {
            List<Integer> values = new ArrayList<>();
            for(int i = 0; i < gasTimestamps.length; i++) {
                values.add(i);
            }
            gasReadings.put(key, values);
        }

        // Create time series list from the readings
        List<?> timeSeries = (List<?>) convertReadingsToTimeSeries.invoke(agent, particleReadings, gasReadings);
        // Check that there is a time series for each mapping
        Assert.assertEquals(keys.size(), timeSeries.size());
        // Check content of the time series
        for(Object obj: timeSeries) {
            TimeSeries<?> currentTimeSeries = (TimeSeries<?>) obj;
            // Time series corresponding to gas readings
            if(currentTimeSeries.getTimes().size() == gasTimestamps.length) {
                // Number of IRIs should match the number of keys
                Assert.assertEquals(gasKeys.length, currentTimeSeries.getDataIRIs().size());
                for(String iri: currentTimeSeries.getDataIRIs()) {
                    List<?> values = currentTimeSeries.getValues(iri);
                    // The size of value should match the number of time stamps
                    Assert.assertEquals(gasTimestamps.length, values.size());
                    // Gas readings should be integer
                    Assert.assertEquals(Integer.class, values.get(0).getClass());
                }
            }
            // Time series corresponds to either general information or particle readings
            if(currentTimeSeries.getTimes().size() == particleTimestamps.length) {
                // Time series corresponds to particle readings
                if(currentTimeSeries.getDataIRIs().size() == particleKeys.length) {
                    for(String iri: currentTimeSeries.getDataIRIs()) {
                        List<?> values = currentTimeSeries.getValues(iri);
                        // The size of value should match the number of time stamps
                        Assert.assertEquals(particleTimestamps.length, values.size());
                        // Particle readings should be double
                        Assert.assertEquals(Double.class, values.get(0).getClass());
                    }
                }
                // Time series corresponds to general readings
                else {
                    // Number of IRIs should match the number of keys
                    Assert.assertEquals(generalKeys.length, currentTimeSeries.getDataIRIs().size());
                    for(String iri: currentTimeSeries.getDataIRIs()) {
                        List<?> values = currentTimeSeries.getValues(iri);
                        // The size of value should match the number of time stamps
                        Assert.assertEquals(particleTimestamps.length, values.size());
                        // General readings should be string
                        Assert.assertEquals(String.class, values.get(0).getClass());
                    }
                }
            }
        }
    }

    @Test
    public void testConvertStringToZonedDateTime() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method convertStringToZonedDateTime = AQMeshInputAgent.class.getDeclaredMethod("convertStringToZonedDateTime", String.class);
        convertStringToZonedDateTime.setAccessible(true);
        // Test with a valid string
        String timestamp = "2021-07-11T16:15:00";
        ZonedDateTime time = (ZonedDateTime) convertStringToZonedDateTime.invoke(testAgent, timestamp);
        Assert.assertEquals(2021, time.getYear());
        Assert.assertEquals(7, time.getMonth().getValue());
        Assert.assertEquals(11, time.getDayOfMonth());
        Assert.assertEquals(16, time.getHour());
        Assert.assertEquals(0, time.getOffset().getTotalSeconds());
        Assert.assertEquals("UTC", time.getZone().getId());
    }

    @Test
    public void testPruneTimeSeries() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Initialize time series
        List<String> iris = Arrays.asList("data_int","data_str");
        List<Integer> intValues = new ArrayList<>();
        List<String> stringValues = new ArrayList<>();
        String[] timestamps = {"2021-07-11T16:10:00+00:00", "2021-07-11T16:15:00+00:00",
                "2021-07-11T16:20:00+00:00", "2021-07-11T16:25:00+00:00"};
        List<ZonedDateTime> times = new ArrayList<>();
        for (int i = 0; i < timestamps.length; i++) {
            times.add(ZonedDateTime.parse(timestamps[i]));
            intValues.add(i);
            stringValues.add(String.valueOf(i));
        }
        List<List<?>> values = Arrays.asList(intValues, stringValues);
        TimeSeries<ZonedDateTime> timeSeries = new TimeSeries<>(times, iris, values);
        // Make method accessible
        Method pruneTimeSeries = AQMeshInputAgent.class.getDeclaredMethod("pruneTimeSeries", TimeSeries.class, ZonedDateTime.class);
        pruneTimeSeries.setAccessible(true);

        // Maximum time lies before the smallest time in the time series -> no pruning
        TimeSeries<?> prunedTimeSeries = (TimeSeries<?>) pruneTimeSeries.invoke(testAgent, timeSeries, ZonedDateTime.parse("2021-07-11T15:00:00+00:00"));
        Assert.assertEquals(times.size(), prunedTimeSeries.getTimes().size());
        for (String iri: iris) {
            Assert.assertEquals(timeSeries.getValues(iri), prunedTimeSeries.getValues(iri));
        }

        // Maximum time lies within the time series -> pruning
        prunedTimeSeries = (TimeSeries<?>) pruneTimeSeries.invoke(testAgent, timeSeries, ZonedDateTime.parse("2021-07-11T16:16:00+00:00"));
        Assert.assertEquals(2, prunedTimeSeries.getTimes().size());
        for (String iri: iris) {
            Assert.assertEquals(timeSeries.getValues(iri).subList(2, times.size()), prunedTimeSeries.getValues(iri));
        }

        // Maximum time lies after time series -> prune all
        prunedTimeSeries = (TimeSeries<?>) pruneTimeSeries.invoke(testAgent, timeSeries, ZonedDateTime.parse("2021-07-11T16:30:00+00:00"));
        Assert.assertEquals(0, prunedTimeSeries.getTimes().size());
        for (String iri: iris) {
            Assert.assertEquals(new ArrayList<>(), prunedTimeSeries.getValues(iri));
        }

    }

}
