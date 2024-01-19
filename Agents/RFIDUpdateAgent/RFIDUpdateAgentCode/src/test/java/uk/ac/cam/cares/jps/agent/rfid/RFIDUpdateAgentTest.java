package uk.ac.cam.cares.jps.agent.rfid;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import com.github.stefanbirkner.systemlambda.SystemLambda;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;

public class RFIDUpdateAgentTest {

    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // The default instance used in the tests
    private RFIDUpdateAgent testAgent;
    // The mocking instance for the time series client
    @SuppressWarnings("unchecked")
    private final TimeSeriesClient<OffsetDateTime> mockTSClient = (TimeSeriesClient<OffsetDateTime>) Mockito.mock(TimeSeriesClient.class);

    // A default list of IRIs
    private final List<String> iris = Arrays.asList("iri1");
    // Default list of JSON keys
    private final String[] keys = {"tag_12345_status"};
    
    //Default list of timestamps

    // Readings used by several tests
    JSONObject allReadings;

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
        writePropertyFile(propertiesFile, Collections.singletonList("rfid.mappingfolder=TEST_MAPPINGS"));
        // To create testAgent without an exception being thrown, SystemLambda is used to mock an environment variable
        // To mock the environment variable, a try catch need to be used
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
        		 testAgent = new RFIDUpdateAgent(propertiesFile);
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

        allReadings = new JSONObject();
        
        JSONArray tag12345Measurements = new JSONArray();

        
            JSONObject tag12345Measures_01 = new JSONObject();
            JSONObject tag12345Measures_02 = new JSONObject();
            JSONObject tag12345Measures_03 = new JSONObject();
            JSONObject tag12345Measures_04 = new JSONObject();
            
            long ts_01 = 1234560000000L;
            long ts_02 = 1250560000000L;
            long ts_03 = 1266560000000L;
            long ts_04 = 1282560000000L;
            
            tag12345Measures_01.put(RFIDUpdateAgent.timestampKey, ts_01);
            tag12345Measures_01.put("value", "In");
            tag12345Measures_02.put(RFIDUpdateAgent.timestampKey, ts_02);
            tag12345Measures_02.put("value", "Out");
            tag12345Measures_03.put(RFIDUpdateAgent.timestampKey, ts_03);
            tag12345Measures_03.put("value", "In");
            tag12345Measures_04.put(RFIDUpdateAgent.timestampKey, ts_04);
            tag12345Measures_04.put("value", "Out");
            
            
            tag12345Measurements.put(tag12345Measures_04);
            tag12345Measurements.put(tag12345Measures_03);
            tag12345Measurements.put(tag12345Measures_02);
            tag12345Measurements.put(tag12345Measures_01);
            
            
            allReadings.put("tag_12345_status", tag12345Measurements);
    }

    @Test
    public void testConstructor() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        // Run constructor on an empty file should give an exception
        writePropertyFile(propertiesFile, new ArrayList<>());
        try {
            new RFIDUpdateAgent(propertiesFile);
            Assert.fail();
        }
        
        catch (IOException e) {
            Assert.assertEquals("The key rfid.mappingfolder cannot be found in the properties file.", e.getMessage());
        }
       
        // Create a property file with a mapping folder that does not exist
        String folderName = "no_valid_folder";
        writePropertyFile(propertiesFile, Collections.singletonList("rfid.mappingfolder=" + folderName));
        // Run constructor that should give an exception
        try {
            new RFIDUpdateAgent(propertiesFile);
            Assert.fail();
        }
        catch (InvalidPropertiesFormatException e) {
        	Assert.assertEquals("The properties file does not contain the key rfid.mappingfolder " +
                    "with a path to the folder containing the required JSON key to IRI mappings.", e.getMessage());
        }

        // Create an empty folder
        folderName = "mappings_test";
        File mappingFolder = folder.newFolder(folderName);
        // Create a property file with the empty folder
        folderName = mappingFolder.getCanonicalPath().replace("\\","/");
        writePropertyFile(propertiesFile, Collections.singletonList("rfid.mappingfolder=TEST_MAPPINGS"));
        // Run constructor that should give an exception
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
        		new RFIDUpdateAgent(propertiesFile);
        		Assert.fail();
        	 });
        }
        catch (Exception e) {
        	Assert.assertTrue(e.getMessage().contains("No files in the folder:"));
        }

        // Add mapping files into the empty folder
        // All IRIs set
        String firstMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "firstMapping.properties").toString();
        String[] keys = {"tag_12345_status"};
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
        		RFIDUpdateAgent agent = new RFIDUpdateAgent(propertiesFile);
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
        Method getClassFromJSONKey = RFIDUpdateAgent.class.getDeclaredMethod("getClassFromJSONKey", String.class);
        getClassFromJSONKey.setAccessible(true);
        // No specific key should return the string class
        Assert.assertEquals(String.class, getClassFromJSONKey.invoke(testAgent, "ts"));
        // Status should return the String class
        Assert.assertEquals(String.class, getClassFromJSONKey.invoke(testAgent, "tag_12345_status"));
    }
    

    @Test
    public void testTimeSeriesExistAllIRIsTrue() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method timeSeriesExist = RFIDUpdateAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
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
    public void testTimeSeriesExistAllIRIFalse() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method timeSeriesExist = RFIDUpdateAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
        timeSeriesExist.setAccessible(true);
        // Set the mock to return false for any IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(Mockito.anyString())).thenReturn(false);
        // Should return false as no IRI is attached (based on the mock)
        Assert.assertFalse((Boolean) timeSeriesExist.invoke(testAgent, iris));
        // Should have returned false after first IRI
        Mockito.verify(mockTSClient, Mockito.times(1)).checkDataHasTimeSeries(iris.get(0));
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
    public void testUpdateDataException() {
        String key = "tag_12345_status";
       

        // Using empty readings should throw an exception
        try {
        	JSONObject allReadings = new JSONObject();
            testAgent.updateData(allReadings, key);
            Assert.fail();
        }
        //This exception should be thrown by JSONObjectToMapForTimeStamp
        catch (IllegalArgumentException e) {
            Assert.assertEquals("Readings can not be empty!", e.getMessage());
        }
        
    }

    @Test
    public void testUpdateData() {
    	String key = "tag_12345_status";
        // Set up the mock client
        // Use a max time that is clearly before any of the example readings
    	Mockito.when(mockTSClient.getMaxTime(Mockito.anyString())).thenReturn(OffsetDateTime.parse("1970-01-01T00:00:00+00:00"));
        // Run the update
        testAgent.updateData(allReadings, key);
        // Capture the arguments that the add data method was called with
        @SuppressWarnings("unchecked")
        ArgumentCaptor<TimeSeries<OffsetDateTime>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        // Ensure that the update was called for each time series
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries())).addTimeSeriesData(timeSeriesArgument.capture());
        // Ensure that the timeseries objects have the correct structure
        int numIRIs = 0;
        
        for(TimeSeries<OffsetDateTime> ts: timeSeriesArgument.getAllValues()) {
        	
            // Check that number of timestamps is correct
            Assert.assertEquals(allReadings.getJSONArray(keys[0]).length(), ts.getTimes().size());
            numIRIs = numIRIs + ts.getDataIRIs().size();
        }
        // Number of unique keys in readings should match the number of IRIs
        Set<String> uniqueKeys = new HashSet<>(allReadings.keySet());
        uniqueKeys.addAll(allReadings.keySet());
       
        Assert.assertEquals(uniqueKeys.size(), numIRIs);
    }

    @Test
    public void testUpdateDataNoDataInDatabase() {
    	String key = "tag_12345_status";
        // Set up the mock client
        // The max time is null since no data is in the database yet
        Mockito.when(mockTSClient.getMaxTime(Mockito.anyString())).thenReturn(null);
        // Run the update
        testAgent.updateData(allReadings, key);
        // Capture the arguments that the add data method was called with
        @SuppressWarnings("unchecked")
        ArgumentCaptor<TimeSeries<OffsetDateTime>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        // Ensure that the update was called for each time series
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries())).addTimeSeriesData(timeSeriesArgument.capture());
        // Ensure that the timeseries objects have the correct structure
        int numIRIs = 0;
        for(TimeSeries<OffsetDateTime> ts: timeSeriesArgument.getAllValues()) {
            // Check that number of timestamps is correct
            Assert.assertEquals(allReadings.getJSONArray(keys[0]).length(), ts.getTimes().size());
            numIRIs = numIRIs + ts.getDataIRIs().size();
        }
        // Number of unique keys in both readings should match the number of IRIs
        Set<String> uniqueKeys = new HashSet<>(allReadings.keySet());
        uniqueKeys.addAll(allReadings.keySet());
        Assert.assertEquals(uniqueKeys.size(), numIRIs);
    }
    
    @Test
    public void testUpdateDataPrune() {
    	String key = "tag_12345_status";
        // Set up the mock client
        // Use a max time that is overlapping with the readings
        int numEntriesToKeep = 2;
        assert allReadings.getJSONArray(keys[0]).length() > numEntriesToKeep;
        long timestamp = allReadings.getJSONArray(keys[0]).getJSONObject(allReadings.getJSONArray(keys[0]).length()-2)
                .getLong(RFIDUpdateAgent.timestampKey);
        Date date = new java.util.Date(timestamp);
    	SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    	String maxTime = sdf.format(date);
    	
        Mockito.when(mockTSClient.getMaxTime(Mockito.anyString())).thenReturn(OffsetDateTime.parse(maxTime+"+00:00"));
        // Run the update
        testAgent.updateData(allReadings, key);
        // Capture the arguments that the add data method was called with
        @SuppressWarnings("unchecked")
        ArgumentCaptor<TimeSeries<OffsetDateTime>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        // Ensure that the update was called for each time series
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries())).addTimeSeriesData(timeSeriesArgument.capture());
        // Ensure that the timeseries objects have the correct structure
        int numIRIs = 0;
        for(TimeSeries<OffsetDateTime> ts: timeSeriesArgument.getAllValues()) {
            // Check that number of timestamps is correct
            Assert.assertEquals(numEntriesToKeep, ts.getTimes().size());
            numIRIs = numIRIs + ts.getDataIRIs().size();
        }
        // Number of unique keys in both readings should match the number of IRIs
        Set<String> uniqueKeys = new HashSet<>(allReadings.keySet());
       
        Assert.assertEquals(uniqueKeys.size(), numIRIs);
    }
    
    @Test
    public void testUpdateDataPruneAll() {
    	String key = "tag_12345_status";
        // Use a max time that is past max time of readings
    	JSONArray tsAndValues = allReadings.getJSONArray(keys[0]);
    	long timestamp = tsAndValues.getJSONObject(0).getLong("ts");
    	Date date = new java.util.Date(timestamp + 1234);
    	SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    	String maxTime = sdf.format(date);
    	
        OffsetDateTime endTime = OffsetDateTime.parse(maxTime+"+00:00");
        Mockito.when(mockTSClient.getMaxTime(Mockito.anyString())).thenReturn(endTime.plusDays(1));
        // Run the update
        testAgent.updateData(allReadings, key);
        // Ensure that the update is never called
        Mockito.verify(mockTSClient, Mockito.never()).addTimeSeriesData(Mockito.any());
    }

    @Test
    public void testJsonObjectToMapEmptyReadings() throws NoSuchMethodException, InvocationTargetException,
            IllegalAccessException {
        JSONObject readings = new JSONObject();
        String key = "";
        // Make method accessible
        Method jsonObjectToMap = RFIDUpdateAgent.class.getDeclaredMethod("jsonObjectToMap", JSONObject.class, String.class);
        jsonObjectToMap.setAccessible(true);
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readingsMap = (Map<String, List<?>>) jsonObjectToMap.invoke(testAgent, readings, key);
        // The map should be empty
        Assert.assertTrue(readingsMap.isEmpty());
    }
    
    @Test
    public void testJsonObjectToMapForTimeStampEmptyReadings() throws NoSuchMethodException, InvocationTargetException,
            IllegalAccessException {
        JSONObject readings = new JSONObject();
        String key = "";
        // Make method accessible
        Method jsonObjectToMapForTimeStamp = RFIDUpdateAgent.class.getDeclaredMethod("jsonObjectToMapForTimeStamp", JSONObject.class, String.class);
        jsonObjectToMapForTimeStamp.setAccessible(true);
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readingsMap = (Map<String, List<?>>) jsonObjectToMapForTimeStamp.invoke(testAgent, readings, key);
        // The map should be empty
        Assert.assertTrue(readingsMap.isEmpty());
    }

    @Test
    public void testJsonObjectToMap() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method jsonObjectToMap = RFIDUpdateAgent.class.getDeclaredMethod("jsonObjectToMap", JSONObject.class, String.class);
        jsonObjectToMap.setAccessible(true);
        String keyValue = "tag_12345_status";
        // Transform the readings
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readings = (Map<String, List<?>>) jsonObjectToMap.invoke(testAgent, allReadings, keyValue);
        // Check that all keys have a list of the same size as the nested JSON Array
        for (String key: readings.keySet()) {
        	JSONArray tsAndValues = allReadings.getJSONArray(key);
            Assert.assertEquals(tsAndValues.length(), readings.get(key).size());
        }
        Assert.assertEquals(readings.get(keys[0]).get(0), allReadings.getJSONArray(keys[0]).getJSONObject(3).get("value"));
        Assert.assertEquals(readings.get(keys[0]).get(1), allReadings.getJSONArray(keys[0]).getJSONObject(2).get("value"));
        Assert.assertEquals(readings.get(keys[0]).get(2), allReadings.getJSONArray(keys[0]).getJSONObject(1).get("value"));
        Assert.assertEquals(readings.get(keys[0]).get(3), allReadings.getJSONArray(keys[0]).getJSONObject(0).get("value"));
        // Check that all keys from the JSON Object have a corresponding entry
        for (Iterator<String> it = allReadings.keys(); it.hasNext();) {
            String key = it.next();
            Assert.assertTrue(readings.containsKey(key));
        }
    }

    @Test
    public void testJsonObjectToMapForTimeStamp() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
    	String keyValue = "tag_12345_status";
        Method jsonObjectToMapForTimeStamp = RFIDUpdateAgent.class.getDeclaredMethod("jsonObjectToMapForTimeStamp", JSONObject.class, String.class);
        jsonObjectToMapForTimeStamp.setAccessible(true);
        // Transform the readings
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readings = (Map<String, List<?>>) jsonObjectToMapForTimeStamp.invoke(testAgent, allReadings, keyValue);
        // Check that all keys have a list of the same size as the nested JSON Array
        Assert.assertTrue(readings.containsKey(RFIDUpdateAgent.timestampKey));
        long ts_01 = 1234560000000L;
        Date date = new java.util.Date(ts_01);
    	SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    	sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
    	Object ts01 = sdf.format(date);
        long ts_02 = 1250560000000L;
        Date date02 = new java.util.Date(ts_02);
    	SimpleDateFormat sdf02 = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    	sdf02.setTimeZone(TimeZone.getTimeZone("UTC"));
    	Object ts02 = sdf02.format(date02);
        long ts_03 = 1266560000000L;
        Date date03 = new java.util.Date(ts_03);
    	SimpleDateFormat sdf03 = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    	sdf03.setTimeZone(TimeZone.getTimeZone("UTC"));
    	Object ts03 = sdf03.format(date03);
    	long ts_04 = 1282560000000L;
        Date date04 = new java.util.Date(ts_04);
    	SimpleDateFormat sdf04 = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    	sdf04.setTimeZone(TimeZone.getTimeZone("UTC"));
    	Object ts04 = sdf04.format(date04);
        Assert.assertTrue(readings.get(RFIDUpdateAgent.timestampKey).contains(ts01));
        Assert.assertEquals("2009-02-13T21:20:00", readings.get(RFIDUpdateAgent.timestampKey).get(0));
        Assert.assertTrue(readings.get(RFIDUpdateAgent.timestampKey).contains(ts02));
        Assert.assertEquals("2009-08-18T01:46:40", readings.get(RFIDUpdateAgent.timestampKey).get(1));
        Assert.assertTrue(readings.get(RFIDUpdateAgent.timestampKey).contains(ts03));
        Assert.assertEquals("2010-02-19T06:13:20", readings.get(RFIDUpdateAgent.timestampKey).get(2));
        Assert.assertTrue(readings.get(RFIDUpdateAgent.timestampKey).contains(ts04));
        Assert.assertEquals("2010-08-23T10:40:00", readings.get(RFIDUpdateAgent.timestampKey).get(3));
        Assert.assertEquals(allReadings.getJSONArray(keys[0]).length(), readings.get(RFIDUpdateAgent.timestampKey).size());
    }

   @Test
    public void testConvertReadingsToTimeSeries() throws IOException, NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {
        // Create an agent with mappings of small size //
        // Create a folder inside the temporary folder in which the mapping files will be
        File mappingFolder= folder.newFolder("mappings_test");
        // Define three sets of mappings
        String[] allTypesKeys = {"tag_12345_status"};
       
        Map<String, String[]> keys = new HashMap<>();
        keys.put("general", allTypesKeys);
     
        // Create a file for each mapping
        for (String mappingName: keys.keySet()) {
            String filepath = Paths.get(mappingFolder.getCanonicalPath(), mappingName+".properties").toString();
            try(FileWriter writer = new FileWriter(filepath, false)) {
                for (String key: keys.get(mappingName)) {
                    writer.write(key + "=\n");
                }
            }
        }
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writePropertyFile(propertiesFile, Collections.singletonList("rfid.mappingfolder=TEST_MAPPINGS"));
        // Create agent
        //Mock environment variable TEST_MAPPINGS to be equivalent to the file path for the mapping folder
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
        		RFIDUpdateAgent agent = new RFIDUpdateAgent(propertiesFile);
        		// Assert that the mappings were set
        	 
        
       
        String[] Timestamps = {"2021-07-11T16:10:00", "2021-07-11T16:15:00",
                "2021-07-11T16:20:00", "2021-07-11T16:25:00"};
        Map<String, List<?>> timeStampReadings = new HashMap<>();
        Map<String, List<?>> allReadings = new HashMap<>();
        
        // Make method accessible
        Method convertReadingsToTimeSeries = RFIDUpdateAgent.class.getDeclaredMethod("convertReadingsToTimeSeries", Map.class, Map.class);
        convertReadingsToTimeSeries.setAccessible(true);

        // Use readings only consisting of times, should give an error as keys are not covered
        try {
        	 // Create the readings //
            
            
            timeStampReadings.put(RFIDUpdateAgent.timestampKey, Arrays.asList(Timestamps));
            convertReadingsToTimeSeries.invoke(agent, allReadings, timeStampReadings);
            Assert.fail();
        }
        catch (InvocationTargetException e) {
            Assert.assertEquals(NoSuchElementException.class, e.getCause().getClass());
            Assert.assertTrue(e.getCause().getMessage().contains("The key"));
            Assert.assertTrue(e.getCause().getMessage().contains("is not contained in the readings!"));
        }
        
        for(String key: allTypesKeys) {
            List<Double> values = new ArrayList<>();
            for(int i = 0; i < Timestamps.length; i++) {
                values.add((double) i + 0.2);
            }
            allReadings.put(key, values);
        }
        // Create time series list from the readings
        List<?> timeSeries = (List<?>) convertReadingsToTimeSeries.invoke(agent, allReadings, timeStampReadings);
        // Check that there is a time series for each mapping
        Assert.assertEquals(keys.size(), timeSeries.size());
        // Check content of the time series
        for(Object obj: timeSeries) {
            TimeSeries<?> currentTimeSeries = (TimeSeries<?>) obj;
            if(currentTimeSeries.getTimes().size() == timeStampReadings.get(RFIDUpdateAgent.timestampKey).size()) {
                // Number of IRIs should match the number of keys
                Assert.assertEquals(allTypesKeys.length, currentTimeSeries.getDataIRIs().size());
                for(String iri: currentTimeSeries.getDataIRIs()) {
                    List<?> values = currentTimeSeries.getValues(iri);
                    // The size of value should match the number of time stamps
                    Assert.assertEquals(timeStampReadings.get(RFIDUpdateAgent.timestampKey).size(), values.size());
                    Assert.assertEquals(Double.class, values.get(0).getClass());
                    // Check values 
                    Assert.assertEquals(allReadings.get(allTypesKeys[0]), values);
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
        Method convertStringToOffsetDateTime = RFIDUpdateAgent.class.getDeclaredMethod("convertStringToOffsetDateTime", String.class);
        convertStringToOffsetDateTime.setAccessible(true);
        // Test with a valid string
        long ts_01 = 1234560000000L;
        Date date = new java.util.Date(ts_01);
    	SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    	sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
    	Object ts01 = sdf.format(date);
        OffsetDateTime time = (OffsetDateTime) convertStringToOffsetDateTime.invoke(testAgent, ts01.toString());
        Assert.assertEquals(2009, time.getYear());
        Assert.assertEquals(2, time.getMonth().getValue());
        Assert.assertEquals(13, time.getDayOfMonth());
        Assert.assertEquals(21, time.getHour());
        Assert.assertEquals(20, time.getMinute());
        Assert.assertEquals(0, time.getOffset().getTotalSeconds());
        Assert.assertEquals(ZoneOffset.UTC, time.getOffset());
    }

    @Test
    public void testPruneTimeSeries() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Initialize time series
        List<String> iris = Arrays.asList("data_int","data_str");
        List<Integer> intValues = new ArrayList<>();
        List<String> stringValues = new ArrayList<>();
        String[] timestamps = {"2021-07-11T16:10:00+00:00", "2021-07-11T16:15:00+00:00",
                "2021-07-11T16:20:00+00:00", "2021-07-11T16:25:00+00:00"};
        List<OffsetDateTime> times = new ArrayList<>();
        for (int i = 0; i < timestamps.length; i++) {
            times.add(OffsetDateTime.parse(timestamps[i]));
            intValues.add(i);
            stringValues.add(String.valueOf(i));
        }
        List<List<?>> values = Arrays.asList(intValues, stringValues);
        TimeSeries<OffsetDateTime> timeSeries = new TimeSeries<>(times, iris, values);
        // Make method accessible
        Method pruneTimeSeries = RFIDUpdateAgent.class.getDeclaredMethod("pruneTimeSeries", TimeSeries.class, OffsetDateTime.class);
        pruneTimeSeries.setAccessible(true);

        // Maximum time lies before the smallest time in the time series -> no pruning
        TimeSeries<?> prunedTimeSeries = (TimeSeries<?>) pruneTimeSeries.invoke(testAgent, timeSeries, OffsetDateTime.parse("2021-07-11T15:00:00+00:00"));
        Assert.assertEquals(times.size(), prunedTimeSeries.getTimes().size());
        for (String iri: iris) {
            Assert.assertEquals(timeSeries.getValues(iri), prunedTimeSeries.getValues(iri));
        }

        // Maximum time lies within the time series -> pruning
        prunedTimeSeries = (TimeSeries<?>) pruneTimeSeries.invoke(testAgent, timeSeries, OffsetDateTime.parse("2021-07-11T16:16:00+00:00"));
        Assert.assertEquals(2, prunedTimeSeries.getTimes().size());
        for (String iri: iris) {
            Assert.assertEquals(timeSeries.getValues(iri).subList(2, times.size()), prunedTimeSeries.getValues(iri));
        }

        // Maximum time lies after time series -> prune all
        prunedTimeSeries = (TimeSeries<?>) pruneTimeSeries.invoke(testAgent, timeSeries, OffsetDateTime.parse("2021-07-11T16:30:00+00:00"));
        Assert.assertEquals(0, prunedTimeSeries.getTimes().size());
        for (String iri: iris) {
            Assert.assertEquals(new ArrayList<>(), prunedTimeSeries.getValues(iri));
        }

    }

}
