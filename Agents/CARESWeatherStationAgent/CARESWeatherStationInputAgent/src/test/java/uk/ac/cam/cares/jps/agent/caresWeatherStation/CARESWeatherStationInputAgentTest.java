package uk.ac.cam.cares.jps.agent.caresWeatherStation;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import com.github.stefanbirkner.systemlambda.SystemLambda;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;

public class CARESWeatherStationInputAgentTest {
    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // The default instance used in the tests
    private CARESWeatherStationInputAgent testAgent;
    // The mocking instance for the time series client
    @SuppressWarnings("unchecked")
    private final TimeSeriesClient<OffsetDateTime> mockTSClient = (TimeSeriesClient<OffsetDateTime>) Mockito.mock(TimeSeriesClient.class);

    // A default list of IRIs
    private final List<String> iris = Arrays.asList("iri1", "iri2", "iri3","iri4","iri5","iri6","iri7","iri8","iri9","iri10");
    // Default list of JSON keys

    private final String[] keys = {"temp","dewpt","heatIndex","windChill","windGust","windSpeed","pressure","precipRate", "precipTotal" ,"elev"};
    // Default list of timestamps
    private final String[] timestamps = {"2022-07-11T16:10:00Z", "2022-07-11T16:15:00Z", "2022-07-11T16:20:00Z", "2022-07-11T16:25:00Z"};

    private ArrayList<Double> weatherValues;
    // Readings used by several tests
    JSONObject weatherDataReadings;

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
        writePropertyFile(propertiesFile, Collections.singletonList("caresWeatherStation.mappingfolder=TEST_MAPPINGS"));
        // To create testAgent without an exception being thrown, SystemLambda is used to mock an environment variable
        // To mock the environment variable, a try catch need to be used
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                testAgent = new CARESWeatherStationInputAgent(propertiesFile);
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
        double value =0.0;
        weatherDataReadings = new JSONObject();
        JSONArray jsArr= new JSONArray();
        for(int i=0; i<timestamps.length;i++) {
            JSONObject currentWeatherData = new JSONObject();
            currentWeatherData.put(CARESWeatherStationInputAgent.timestampKey, timestamps[i]);
            JSONObject measurements = new JSONObject();
            for(String key: keys) {
                measurements.put(key, value);
            }
            currentWeatherData.put("metric_si",measurements);
            jsArr.put(i,currentWeatherData);
            value++;
        }
        weatherDataReadings.put("observations",jsArr);
    }


    @Test
    public void testConstructor() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        // Run constructor on an empty file should give an exception
        writePropertyFile(propertiesFile, new ArrayList<>());
        try {
            new CARESWeatherStationInputAgent(propertiesFile);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertEquals("The key caresWeatherStation.mappingfolder cannot be found in the properties file.", e.getMessage());
        }

        // Create a property file with a mapping folder that does not exist
        String folderName = "no_valid_folder";
        writePropertyFile(propertiesFile, Collections.singletonList("caresWeatherStation.mappingfolder=" + folderName));
        // Run constructor that should give an exception
        try {
            new CARESWeatherStationInputAgent(propertiesFile);
            Assert.fail();
        }
        catch (InvalidPropertiesFormatException e) {
            Assert.assertEquals("The properties file does not contain the key caresWeatherStation.mappingfolder "+
                            "with a path to the folder containing the required JSON key to IRI mappings.",e.getMessage());

        }

        // Create an empty folder
        folderName = "mappings_test";
        File mappingFolder = folder.newFolder(folderName);
        // Create a property file with the empty folder
        folderName = mappingFolder.getCanonicalPath().replace("\\","/");
        writePropertyFile(propertiesFile, Collections.singletonList("caresWeatherStation.mappingfolder=TEST_MAPPINGS"));
        // Run constructor that should give an exception
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                new CARESWeatherStationInputAgent(propertiesFile);
                Assert.fail();
            });
        }
        catch (Exception e) {
            Assert.assertTrue(e.getMessage().contains("No files in the folder:"));
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
        // Save the size of the files for assertions later
        long firstMappingFileSize = Files.size(Paths.get(firstMappingFile));
        long secondMappingFileSize = Files.size(Paths.get(secondMappingFile));
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                CARESWeatherStationInputAgent agent = new CARESWeatherStationInputAgent(propertiesFile);
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
        Method getClassFromJSONKey =CARESWeatherStationInputAgent.class.getDeclaredMethod("getClassFromJSONKey", String.class);
        getClassFromJSONKey.setAccessible(true);

        // keys should return double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "tempAvg"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "pressureMax"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "precipTotal"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "heatindexAvg"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "dewptAvg"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "windChill"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "windgustAvg"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "windspeedAvg"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent,"precipRate"));
        Assert.assertEquals(Long.class, getClassFromJSONKey.invoke(testAgent,"epoch"));
        Assert.assertEquals(Integer.class, getClassFromJSONKey.invoke(testAgent,"qcStatus"));
        Assert.assertEquals(String.class, getClassFromJSONKey.invoke(testAgent,"obsTimeLocal"));
    }

    @Test
    public void testTimeSeriesExistAllIRIsTrue() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method timeSeriesExist = CARESWeatherStationInputAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
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
        Method timeSeriesExist = CARESWeatherStationInputAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
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
        Method timeSeriesExist = CARESWeatherStationInputAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
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
        // Initialize readings
        try {
            JSONObject weatherDataReadings = new JSONObject();
            testAgent.updateData(weatherDataReadings);
            Assert.fail();
        }

        catch (Exception e) {
            Assert.assertTrue(e.getCause().getMessage().contains("Readings can not be empty!"));
        }
        // Create readings with timestamps and missing keys
        // Should trigger an exception due to missing keys
        try {
            JSONArray jsArr = weatherDataReadings.getJSONArray("observations");
            for (int i=0;i<jsArr.length();i++) {
                JSONObject currentEntry = jsArr.getJSONObject(i);
                JSONObject obj = currentEntry.getJSONObject("metric_si");
                obj.remove(keys[0]);
                obj.remove(keys[1]);
            }
            testAgent.updateData(weatherDataReadings);
            Assert.fail();
        }
        catch (IllegalArgumentException e) {
            Assert.assertEquals("Readings can not be converted to proper time series!", e.getMessage());
            Assert.assertEquals(NoSuchElementException.class, e.getCause().getClass());
        }
    }

    @Test
    public void testUpdateData() {
        // Set up the mock client
        // Use a max time that is clearly before any of the example readings
        Mockito.when(mockTSClient.getMaxTime(Mockito.anyString())).thenReturn(OffsetDateTime.parse("1988-07-10T00:50:00+00:00"));
        // Run the update
        testAgent.updateData(weatherDataReadings);
        // Capture the arguments that the add data method was called with
        @SuppressWarnings("unchecked")
        ArgumentCaptor<TimeSeries<OffsetDateTime>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        // Ensure that the update was called for each time series
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries())).addTimeSeriesData(timeSeriesArgument.capture());
        // Ensure that the timeseries objects have the correct structure
        int numIRIs = 0;
        JSONArray jsArr = weatherDataReadings.getJSONArray("observations");
        JSONObject currentEntry = jsArr.getJSONObject(0);
        JSONObject obj=currentEntry.getJSONObject("metric_si");
        for(TimeSeries<OffsetDateTime> ts: timeSeriesArgument.getAllValues()) {
            // Check that number of timestamps is correct
            Assert.assertEquals(jsArr.length(), ts.getTimes().size());
            numIRIs = numIRIs + ts.getDataIRIs().size();
        }
        // Number of unique keys in readings should match the number of IRIs
        Set<String> keys = new HashSet<>(obj.keySet());
        keys.addAll(obj.keySet());

        Assert.assertEquals(keys.size(), numIRIs);
    }

    @Test
    public void testUpdateDataNoDataInDatabase() {
        // Set up the mock client
        // The max time is null since no data is in the database yet
        Mockito.when(mockTSClient.getMaxTime(Mockito.anyString())).thenReturn(null);
        // Run the update
        testAgent.updateData(weatherDataReadings);
        // Capture the arguments that the add data method was called with
        @SuppressWarnings("unchecked")
        ArgumentCaptor<TimeSeries<OffsetDateTime>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        // Ensure that the update was called for each time series
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries())).addTimeSeriesData(timeSeriesArgument.capture());
        // Ensure that the timeseries objects have the correct structure
        int numIRIs = 0;
        JSONArray jsArr = weatherDataReadings.getJSONArray("observations");
        JSONObject currentEntry = jsArr.getJSONObject(0);
        JSONObject obj=currentEntry.getJSONObject("metric_si");
        for(TimeSeries<OffsetDateTime> ts: timeSeriesArgument.getAllValues()) {
            // Check that number of timestamps is correct
            Assert.assertEquals(jsArr.length(), ts.getTimes().size());
            numIRIs = numIRIs + ts.getDataIRIs().size();
        }
        // Number of unique keys in readings should match the number of IRIs

        Set<String> keys = new HashSet<>(obj.keySet());
        keys.addAll(obj.keySet());

        Assert.assertEquals(keys.size(), numIRIs);
    }

    @Test
    public void testUpdateDataPrune() {

        JSONArray jsArr1 = weatherDataReadings.getJSONArray("observations");
        JSONObject currentEntry1 = jsArr1.getJSONObject(0);

        String maxTime = currentEntry1.getString(CARESWeatherStationInputAgent.timestampKey).replace("Z","");
        Mockito.when(mockTSClient.getMaxTime(Mockito.anyString())).thenReturn(OffsetDateTime.parse(maxTime+"+00:00"));
        // Run the update
        testAgent.updateData(weatherDataReadings);
        // Capture the arguments that the add data method was called with
        @SuppressWarnings("unchecked")
        ArgumentCaptor<TimeSeries<OffsetDateTime>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        // Ensure that the update was called for each time series
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries())).addTimeSeriesData(timeSeriesArgument.capture());
        // Ensure that the timeseries objects have the correct structure
        int numIRIs = 0;
        for(TimeSeries<OffsetDateTime> ts: timeSeriesArgument.getAllValues()) {

            numIRIs = numIRIs + ts.getDataIRIs().size();
        }
        // Number of unique keys in both readings should match the number of IRIs
        JSONArray jsArr = weatherDataReadings.getJSONArray("observations");
        JSONObject currentEntry = jsArr.getJSONObject(0);

        Set<String> uniqueKeys = new HashSet<>(currentEntry.getJSONObject("metric_si").keySet());

        Assert.assertEquals(uniqueKeys.size() , numIRIs);
    }

    @Test
    public void testUpdateDataPruneAll() {
        // Use a max time that is past max time of readings
        JSONArray jsArr1 = weatherDataReadings.getJSONArray("observations");
        JSONObject currentEntry1 = jsArr1.getJSONObject(jsArr1.length()-1);
        String maxTime = currentEntry1.getString(CARESWeatherStationInputAgent.timestampKey).replace("Z","");

        OffsetDateTime endTime = OffsetDateTime.parse(maxTime+"+00:00");
        Mockito.when(mockTSClient.getMaxTime(Mockito.anyString())).thenReturn(endTime.plusDays(1));
        // Run the update
        testAgent.updateData(weatherDataReadings);
        // Ensure that the update is never called
        Mockito.verify(mockTSClient, Mockito.never()).addTimeSeriesData(Mockito.any());
    }

    @Test
    public void testJsonObjectToMapEmptyReadings() throws NoSuchMethodException, InvocationTargetException,
            IllegalAccessException {
        JSONObject readings = new JSONObject();
        // Make method accessible
        Method jsonObjectToMap = CARESWeatherStationInputAgent.class.getDeclaredMethod("jsonObjectToMap", JSONObject.class);
        jsonObjectToMap.setAccessible(true);
        try {
            @SuppressWarnings("unchecked")
            Map<String, List<?>> readingsMap = (Map<String, List<?>>) jsonObjectToMap.invoke(testAgent, readings);
            Assert.fail();
        }
        catch(Exception e){
            Assert.assertTrue(e.getCause().getMessage().contains("Readings can not be empty!"));
        }
    }

    @Test
    public void testJsonObjectToMap() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method jsonObjectToMap = CARESWeatherStationInputAgent.class.getDeclaredMethod("jsonObjectToMap", JSONObject.class);
        jsonObjectToMap.setAccessible(true);
        // Transform the readings
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readings = (Map<String, List<?>>) jsonObjectToMap.invoke(testAgent, weatherDataReadings);
        // Check that all keys have a list of the same size as the JSON Array

        Assert.assertEquals(keys.length+1, readings.size());

        // Check that all keys from the JSON Array have a corresponding entry
        JSONArray arr=weatherDataReadings.getJSONArray("observations");
        JSONObject obj=arr.getJSONObject(0);
        JSONObject meas=obj.getJSONObject("metric_si");
        for (Iterator<String> it = meas.keys(); it.hasNext();) {
            String key = it.next();
            Assert.assertTrue(readings.containsKey(key));
        }
        Assert.assertTrue(readings.containsKey("obsTimeUtc"));
    }


    @Test
    public void testConvertReadingsToTimeSeries() throws IOException, NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {
        // Create an agent with mappings of small size //
        // Create a folder inside the temporary folder in which the mapping files will be
        File mappingFolder = folder.newFolder("mappings_test");
        // Define three sets of mappings
        String[] generalKeys = {"key1", "key2", "key3"};
        Map<String, String[]> keys = new HashMap<>();
        keys.put("general", generalKeys);

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
        writePropertyFile(propertiesFile, Collections.singletonList("caresWeatherStation.mappingfolder=TEST_MAPPINGS"));
        // Create agent
        //Mock environment variable TEST_MAPPINGS to be equivalent to the file path for the mapping folder
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                CARESWeatherStationInputAgent agent = new CARESWeatherStationInputAgent(propertiesFile);

                String[] Timestamps = {"2021-07-11T16:10:00Z", "2021-07-11T16:15:00Z",
                        "2021-07-11T16:20:00Z", "2021-07-11T16:25:00Z"};
                Map<String, List<?>> timeStampReadings = new HashMap<>();
                Map<String, List<?>> weatherReadings = new HashMap<>();

                // Make method accessible
                Method convertReadingsToTimeSeries = CARESWeatherStationInputAgent.class.getDeclaredMethod("convertReadingsToTimeSeries", Map.class, Map.class);
                convertReadingsToTimeSeries.setAccessible(true);

                // Use readings only consisting of times, should give an error as keys are not covered
                try {
                    // Create the readings //


                    timeStampReadings.put(CARESWeatherStationInputAgent.timestampKey, Arrays.asList(Timestamps));
                    convertReadingsToTimeSeries.invoke(agent, weatherReadings, timeStampReadings);
                    Assert.fail();
                }
                catch (InvocationTargetException e) {
                    Assert.assertEquals(NoSuchElementException.class, e.getCause().getClass());
                    Assert.assertTrue(e.getCause().getMessage().contains("The key"));
                    Assert.assertTrue(e.getCause().getMessage().contains("is not contained in the readings!"));
                }

                for(String key: generalKeys) {
                    List<Double> values = new ArrayList<>();
                    for(int i = 0; i < Timestamps.length; i++) {
                        values.add((double) i + 0.2);
                    }
                    weatherReadings.put(key, values);
                }
                // Create time series list from the readings
                List<?> timeSeries = (List<?>) convertReadingsToTimeSeries.invoke(agent, weatherReadings, timeStampReadings);
                // Check that there is a time series for each mapping
                Assert.assertEquals(keys.size(), timeSeries.size());
                // Check content of the time series
                for(Object obj: timeSeries) {
                    TimeSeries<?> currentTimeSeries = (TimeSeries<?>) obj;
                    if(currentTimeSeries.getTimes().size() == timeStampReadings.get(CARESWeatherStationInputAgent.timestampKey).size()) {
                        // Number of IRIs should match the number of keys
                        Assert.assertEquals(generalKeys.length, currentTimeSeries.getDataIRIs().size());
                        for(String iri: currentTimeSeries.getDataIRIs()) {
                            List<?> values = currentTimeSeries.getValues(iri);
                            // The size of value should match the number of time stamps
                            Assert.assertEquals(timeStampReadings.get(CARESWeatherStationInputAgent.timestampKey).size(), values.size());
                            Assert.assertEquals(Double.class, values.get(0).getClass());
                            // Check values
                            Assert.assertEquals(weatherDataReadings.get(generalKeys[0]), values);
                        }
                    }

                }
            });
        }
        //No exception should be thrown here, this is required in order to use System.lambda to mock the environment variables
        catch (Exception e) {
        }
    }

    @Test
    public void testConvertStringToOffsetDateTime() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method convertStringToOffsetDateTime = CARESWeatherStationInputAgent.class.getDeclaredMethod("convertStringToOffsetDateTime", String.class);
        convertStringToOffsetDateTime.setAccessible(true);
        // Test with a valid string
        String timestamp = "2022-07-11T16:15:00Z";
        OffsetDateTime time = (OffsetDateTime) convertStringToOffsetDateTime.invoke(testAgent, timestamp);
        Assert.assertEquals(2022, time.getYear());
        Assert.assertEquals(7, time.getMonth().getValue());
        Assert.assertEquals(11, time.getDayOfMonth());
        Assert.assertEquals(16, time.getHour());
        Assert.assertEquals(0, time.getOffset().getTotalSeconds());
        Assert.assertEquals(ZoneOffset.UTC, time.getOffset());
    }

    @Test
    public void testPruneTimeSeries() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Initialize time series
        List<String> iris = Arrays.asList("data_int","data_str");
        List<Integer> intValues = new ArrayList<>();
        List<String> stringValues = new ArrayList<>();
        String[] timestamps = {"2022-07-11T16:10:00+00:00Z", "2022-07-11T16:15:00+00:00Z",
                "2022-07-11T16:20:00+00:00Z", "2022-07-11T16:25:00+00:00Z"};
        for (int i=0;i< timestamps.length;++i){
            timestamps[i]=timestamps[i].replace("Z","");
        }
        List<OffsetDateTime> times = new ArrayList<>();
        for (int i = 0; i < timestamps.length; i++) {
            times.add(OffsetDateTime.parse(timestamps[i]));
            intValues.add(i);
            stringValues.add(String.valueOf(i));
        }
        List<List<?>> values = Arrays.asList(intValues, stringValues);
        TimeSeries<OffsetDateTime> timeSeries = new TimeSeries<>(times, iris, values);
        // Make method accessible
        Method pruneTimeSeries = CARESWeatherStationInputAgent.class.getDeclaredMethod("pruneTimeSeries", TimeSeries.class, OffsetDateTime.class);
        pruneTimeSeries.setAccessible(true);

        // Maximum time lies before the smallest time in the time series -> no pruning
        TimeSeries<?> prunedTimeSeries = (TimeSeries<?>) pruneTimeSeries.invoke(testAgent, timeSeries, OffsetDateTime.parse("2022-07-11T15:00:00+00:00"));
        Assert.assertEquals(times.size(), prunedTimeSeries.getTimes().size());
        for (String iri: iris) {
            Assert.assertEquals(timeSeries.getValues(iri), prunedTimeSeries.getValues(iri));
        }

        // Maximum time lies within the time series -> pruning
        prunedTimeSeries = (TimeSeries<?>) pruneTimeSeries.invoke(testAgent, timeSeries, OffsetDateTime.parse("2022-07-11T16:16:00+00:00"));
        Assert.assertEquals(2, prunedTimeSeries.getTimes().size());
        for (String iri: iris) {
            Assert.assertEquals(timeSeries.getValues(iri).subList(2, times.size()), prunedTimeSeries.getValues(iri));
        }

        // Maximum time lies after time series -> prune all
        prunedTimeSeries = (TimeSeries<?>) pruneTimeSeries.invoke(testAgent, timeSeries, OffsetDateTime.parse("2022-07-11T16:30:00+00:00"));
        Assert.assertEquals(0, prunedTimeSeries.getTimes().size());
        for (String iri: iris) {
            Assert.assertEquals(new ArrayList<>(), prunedTimeSeries.getValues(iri));
        }

    }
}