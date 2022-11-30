package uk.ac.cam.cares.jps.agent.thingspeak;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import com.github.stefanbirkner.systemlambda.SystemLambda;
import com.github.tomakehurst.wiremock.common.JsonException;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.SQLException;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;

public class ThingspeakInputAgentTest {

    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // The default instance used in the tests
    private ThingspeakInputAgent testAgent;
    // The mocking instance for the time series client
    @SuppressWarnings("unchecked")
    private final TimeSeriesClient<OffsetDateTime> mockTSClient = (TimeSeriesClient<OffsetDateTime>) Mockito.mock(TimeSeriesClient.class);
    private final RemoteRDBStoreClient mockRDBClient = (RemoteRDBStoreClient) Mockito.mock(RemoteRDBStoreClient.class);

    // A default list of IRIs
    private final List<String> iris = Arrays.asList("iri1", "iri2");
    // Default list of JSON keys
    private final String[] keys = {"Current", "Voltage"};
    
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
        writePropertyFile(propertiesFile, Collections.singletonList("thingspeak.mappingfolder=TEST_MAPPINGS"));
        // To create testAgent without an exception being thrown, SystemLambda is used to mock an environment variable
        // To mock the environment variable, a try catch need to be used
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
        		 testAgent = new ThingspeakInputAgent(propertiesFile);
        	 });
        }
        // There should not be any exception thrown as the agent is initiated correctly
        catch (Exception e) {
        }
        // Set the mocked time series client
        testAgent.setTsClient(mockTSClient);
        testAgent.setRDBClient(mockRDBClient);
    }
   
    @Before
    public void createExampleReadings() {

        allReadings = new JSONObject();
        
        JSONObject jsonobject1 = new JSONObject();
        JSONObject jsonobject2 = new JSONObject();
        JSONObject jsonobject3 = new JSONObject();
        JSONObject jsonobject4 = new JSONObject();
        JSONObject jsonobject5 = new JSONObject();
        JSONArray jsonarray1 = new JSONArray();
        
        jsonobject1.put("created_at", "2022-11-09T03:05:18Z");
        jsonobject1.put("entry_id", 59267);
        jsonobject1.put("field1", Double.valueOf("621.0"));
        jsonobject1.put("field2", Double.valueOf("100.0"));
        
        jsonobject3.put("created_at", "2022-11-09T03:06:18Z");
        jsonobject3.put("entry_id", 59268);
        jsonobject3.put("field1", Double.valueOf("620.0"));
        jsonobject3.put("field2", Double.valueOf("100.0"));
        
        jsonobject4.put("created_at", "2022-11-09T03:07:18Z");
        jsonobject4.put("entry_id", 59268);
        jsonobject4.put("field1", Double.valueOf("620.0"));
        jsonobject4.put("field2", Double.valueOf("100.0"));
        
        jsonobject5.put("created_at", "2022-11-09T03:08:18Z");
        jsonobject5.put("entry_id", 59269);
        jsonobject5.put("field1", Double.valueOf("621.0"));
        jsonobject5.put("field2", Double.valueOf("100.0"));
        
        jsonarray1.put(jsonobject1);
        jsonarray1.put(jsonobject3);
        jsonarray1.put(jsonobject4);
        jsonarray1.put(jsonobject5);
        
        jsonobject2.put("id", 1876219);
        jsonobject2.put("name", "current sensor");
        jsonobject2.put("description", "current test sensor");
        jsonobject2.put("latitude", "0.0");
        jsonobject2.put("longitude","0.0");
        jsonobject2.put("field1", "Current");
        jsonobject2.put("field2", "Voltage");
        jsonobject2.put("created_at", "2022-09-28T15:29:24Z");
        jsonobject2.put("updated_at", "2022-09-28T17:00:37Z");
        jsonobject2.put("last_entry_id", 59267);
        
        allReadings.put("channel", jsonobject2);
        allReadings.put("feeds", jsonarray1);

    }

    @Test
    public void testConstructor() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        // Run constructor on an empty file should give an exception
        writePropertyFile(propertiesFile, new ArrayList<>());
        try {
            new ThingspeakInputAgent(propertiesFile);
            Assert.fail();
        }
        
        catch (IOException e) {
            Assert.assertEquals("The key thingspeak.mappingfolder cannot be found in the properties file.", e.getMessage());
        }
       
        // Create a property file with a mapping folder that does not exist
        String folderName = "no_valid_folder";
        writePropertyFile(propertiesFile, Collections.singletonList("thingspeak.mappingfolder=" + folderName));
        // Run constructor that should give an exception
        try {
            new ThingspeakInputAgent(propertiesFile);
            Assert.fail();
        }
        catch (InvalidPropertiesFormatException e) {
        	Assert.assertEquals("The properties file does not contain the key thingspeak.mappingfolder " +
                    "with a path to the folder containing the required JSON key to IRI mappings.", e.getMessage());
        }

        // Create an empty folder
        folderName = "mappings_test";
        File mappingFolder = folder.newFolder(folderName);
        // Create a property file with the empty folder
        folderName = mappingFolder.getCanonicalPath().replace("\\","/");
        writePropertyFile(propertiesFile, Collections.singletonList("thingspeak.mappingfolder=TEST_MAPPINGS"));
        // Run constructor that should give an exception
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
        		new ThingspeakInputAgent(propertiesFile);
        		Assert.fail();
        	 });
        }
        catch (Exception e) {
        	Assert.assertTrue(e.getMessage().contains("No files in the folder:"));
        }

        // Add mapping files into the empty folder
        // All IRIs set
        String firstMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "firstMapping.properties").toString();
        String[] keys = {"Current", "Voltage"};
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
        		ThingspeakInputAgent agent = new ThingspeakInputAgent(propertiesFile);
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
        Method getClassFromJSONKey = ThingspeakInputAgent.class.getDeclaredMethod("getClassFromJSONKey", String.class);
        getClassFromJSONKey.setAccessible(true);
        // No specific key should return the string class
        Assert.assertEquals(String.class, getClassFromJSONKey.invoke(testAgent, "ts"));
        // Environment conditions should be double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "Current"));
        // Environment conditions should be double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "Voltage"));
    }
    

    @Test
    public void testTimeSeriesExistAllIRIsTrue() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method timeSeriesExist = ThingspeakInputAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
        timeSeriesExist.setAccessible(true);
        // Set the mock to return true for any IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(Mockito.anyString(), Mockito.any())).thenReturn(true);
        // Empty list should return true
        Assert.assertTrue((Boolean) timeSeriesExist.invoke(testAgent, new ArrayList<String>()));
        // Should return true as all IRIs are attached (based on the mock)
        Assert.assertTrue((Boolean) timeSeriesExist.invoke(testAgent, iris));
        // Check also that the check was invoked for all keys
        //Connection is mock as null as there is no real connection set up for this test
        for (String iri : iris) {
            Mockito.verify(mockTSClient, Mockito.times(1)).checkDataHasTimeSeries(iri, null);
            
        }
        
    }

    @Test
    public void testTimeSeriesExistAllOneIRIFalse() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method timeSeriesExist = ThingspeakInputAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
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
        Method timeSeriesExist = ThingspeakInputAgent.class.getDeclaredMethod("timeSeriesExist", List.class);
        timeSeriesExist.setAccessible(true);
        // Set the mock to return false for any IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(Mockito.anyString(), Mockito.any())).thenReturn(false);
        // Should return false as no IRI is attached (based on the mock)
        Assert.assertFalse((Boolean) timeSeriesExist.invoke(testAgent, iris));
        //Connection is mock as null as there is no real connection set up for this test
        Mockito.verify(mockTSClient, Mockito.times(1)).checkDataHasTimeSeries(iris.get(0), null);
        Mockito.verify(mockTSClient, Mockito.never()).checkDataHasTimeSeries(iris.get(1), null);
        
    }

    @Test
    public void testInitializeTimeSeriesIfNotExistCreateAll() throws SQLException {
        // Set the mock to return false for any IRI
        Mockito.when(mockTSClient.checkDataHasTimeSeries(Mockito.anyString(), Mockito.any())).thenReturn(false);
        // Run the initialization method
        testAgent.initializeTimeSeriesIfNotExist();
        // Should have invoked the time series initialization for each mapping
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries()))
                .initTimeSeries(Mockito.anyList(), Mockito.anyList(), Mockito.anyString(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    }

    @Test
    public void testInitializeTimeSeriesIfNotExistCreateNone() throws SQLException {
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
        // Initialize readings
       
        // Using empty readings should throw an exception
        try {
        	JSONObject allReadings = new JSONObject();
            testAgent.updateData(allReadings);
            Assert.fail();
        }
        //This exception should be thrown by JSONObjectToMapForTimeStamp
        catch (IllegalArgumentException e) {
            Assert.assertEquals("Readings can not be empty!", e.getMessage());
        }
        
    }

    @Test
    public void testUpdateData() {
        // Set up the mock client
        // Use a max time that is clearly before any of the example readings
    	Mockito.when(mockTSClient.getMaxTime(Mockito.anyString())).thenReturn(OffsetDateTime.parse("1970-01-01T00:00:00+00:00"));
        // Run the update
        testAgent.updateData(allReadings);
        // Capture the arguments that the add data method was called with
        @SuppressWarnings("unchecked")
        ArgumentCaptor<TimeSeries<OffsetDateTime>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        ArgumentCaptor<Connection> RDBArgument = ArgumentCaptor.forClass(Connection.class);
        // Ensure that the update was called for each time series
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries())).addTimeSeriesData(timeSeriesArgument.capture(), RDBArgument.capture());
        // Ensure that the timeseries objects have the correct structure
        int numIRIs = 0;
        
        for(TimeSeries<OffsetDateTime> ts: timeSeriesArgument.getAllValues()) {
        	
            // Check that number of timestamps is correct
            Assert.assertEquals(allReadings.getJSONArray("feeds").length(), ts.getTimes().size());
            numIRIs = numIRIs + ts.getDataIRIs().size();
        }
    }

    @Test
    public void testUpdateDataNoDataInDatabase() {
        // Set up the mock client
        // The max time is null since no data is in the database yet
        Mockito.when(mockTSClient.getMaxTime(Mockito.anyString(), Mockito.any(Connection.class))).thenReturn(null);
        // Run the update
        testAgent.updateData(allReadings);
        // Capture the arguments that the add data method was called with
        @SuppressWarnings("unchecked")
        ArgumentCaptor<TimeSeries<OffsetDateTime>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        ArgumentCaptor<Connection> RDBArgument = ArgumentCaptor.forClass(Connection.class);
        // Ensure that the update was called for each time series
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries())).addTimeSeriesData(timeSeriesArgument.capture(), RDBArgument.capture());
        // Ensure that the timeseries objects have the correct structure
        int numIRIs = 0;
        for(TimeSeries<OffsetDateTime> ts: timeSeriesArgument.getAllValues()) {
            // Check that number of timestamps is correct
            Assert.assertEquals(allReadings.getJSONArray("feeds").length(), ts.getTimes().size());
            numIRIs = numIRIs + ts.getDataIRIs().size();
        }
    }
    
    @Test
    public void testUpdateDataPrune() {
        // Set up the mock client
        // Use a max time that is overlapping with the readings
        int numEntriesToKeep = 2;
        assert allReadings.getJSONArray("feeds").length() > numEntriesToKeep;
        String timestamp = allReadings.getJSONArray("feeds").getJSONObject(1).getString("created_at");
        String maxTime = timestamp.split("Z")[0];
    	
        Mockito.when(mockTSClient.getMaxTime(Mockito.anyString(), Mockito.any())).thenReturn(OffsetDateTime.parse(maxTime+"+00:00"));
        // Run the update
        testAgent.updateData(allReadings);
        // Capture the arguments that the add data method was called with
        @SuppressWarnings("unchecked")
        ArgumentCaptor<TimeSeries<OffsetDateTime>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        //ArgumentCaptor<Connection> RDBArgument = ArgumentCaptor.forClass(Connection.class);
        // Ensure that the update was called for each time series
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries())).addTimeSeriesData(timeSeriesArgument.capture(), Mockito.any());
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
        // Use a max time that is past max time of readings
    	JSONArray tsAndValues = allReadings.getJSONArray("feeds");
    	String timestamp = tsAndValues.getJSONObject(0).getString("created_at");
    	
        OffsetDateTime endTime = OffsetDateTime.parse(timestamp.split("Z")[0]+"+00:00");
        Mockito.when(mockTSClient.getMaxTime(Mockito.anyString(), Mockito.any(Connection.class))).thenReturn(endTime.plusDays(1));
        // Run the update
        testAgent.updateData(allReadings);
        // Ensure that the update is never called
        Mockito.verify(mockTSClient, Mockito.never()).addTimeSeriesData(Mockito.any(), Mockito.any(Connection.class));
    }

    @Test
    public void testJsonObjectToMapEmptyReadings() throws NoSuchMethodException, InvocationTargetException,
            IllegalAccessException {
        JSONObject readings = new JSONObject();
        try {
        	testAgent.jsonObjectToMap(readings);
        Assert.fail();
        } catch (JSONException e) {
        	Assert.assertEquals(e.getMessage(), "Some keys are missing in the readings!");
        }
        
    }
    
    @Test
    public void testJsonObjectToMapForTimeStampEmptyReadings() throws NoSuchMethodException, InvocationTargetException,
            IllegalAccessException {
        JSONObject readings = new JSONObject();
        try {
        testAgent.jsonObjectToMapForTimeStamp(readings);
        Assert.fail();
        } catch (JSONException e) {
        	Assert.assertEquals(e.getMessage(), "Some keys are missing in the readings!");
        }
    }

    @Test
    public void testJsonObjectToMap() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method jsonObjectToMap = ThingspeakInputAgent.class.getDeclaredMethod("jsonObjectToMap", JSONObject.class);
        jsonObjectToMap.setAccessible(true);
        // Transform the readings
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readings = (Map<String, List<?>>) jsonObjectToMap.invoke(testAgent, allReadings);
        Assert.assertEquals(readings.get(keys[0]).get(0), allReadings.getJSONArray("feeds").getJSONObject(0).get("field1"));
        Assert.assertEquals(readings.get(keys[0]).get(1), allReadings.getJSONArray("feeds").getJSONObject(1).get("field1"));
        Assert.assertEquals(readings.get(keys[0]).get(2), allReadings.getJSONArray("feeds").getJSONObject(2).get("field1"));
        Assert.assertEquals(readings.get(keys[0]).get(3), allReadings.getJSONArray("feeds").getJSONObject(3).get("field1"));
    }

    @Test
    public void testJsonObjectToMapForTimeStamp() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method jsonObjectToMapForTimeStamp = ThingspeakInputAgent.class.getDeclaredMethod("jsonObjectToMapForTimeStamp", JSONObject.class);
        jsonObjectToMapForTimeStamp.setAccessible(true);
        // Transform the readings
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readings = (Map<String, List<?>>) jsonObjectToMapForTimeStamp.invoke(testAgent, allReadings);
        // Check that all keys have a list of the same size as the nested JSON Array
        Assert.assertTrue(readings.containsKey(ThingspeakInputAgent.timestampKey));
        
        String ts01 = "2022-11-09T03:05:18Z";
        String ts02 = "2022-11-09T03:06:18Z";
        String ts03 = "2022-11-09T03:07:18Z";
        String ts04 = "2022-11-09T03:08:18Z";
        Assert.assertTrue(readings.get(ThingspeakInputAgent.timestampKey).contains(ts01));
        Assert.assertEquals("2022-11-09T03:05:18Z", readings.get(ThingspeakInputAgent.timestampKey).get(0));
        Assert.assertTrue(readings.get(ThingspeakInputAgent.timestampKey).contains(ts02));
        Assert.assertEquals("2022-11-09T03:06:18Z", readings.get(ThingspeakInputAgent.timestampKey).get(1));
        Assert.assertTrue(readings.get(ThingspeakInputAgent.timestampKey).contains(ts03));
        Assert.assertEquals("2022-11-09T03:07:18Z", readings.get(ThingspeakInputAgent.timestampKey).get(2));
        Assert.assertTrue(readings.get(ThingspeakInputAgent.timestampKey).contains(ts04));
        Assert.assertEquals("2022-11-09T03:08:18Z", readings.get(ThingspeakInputAgent.timestampKey).get(3));
        Assert.assertEquals(allReadings.getJSONArray("feeds").length(), readings.get(ThingspeakInputAgent.timestampKey).size());
    }

   @Test
    public void testConvertReadingsToTimeSeries() throws IOException, NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {
        // Create an agent with mappings of small size //
        // Create a folder inside the temporary folder in which the mapping files will be
        File mappingFolder= folder.newFolder("mappings_test");
        // Define three sets of mappings
        String[] allTypesKeys = {"Current", "Voltage"};
       
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
        writePropertyFile(propertiesFile, Collections.singletonList("thingspeak.mappingfolder=TEST_MAPPINGS"));
        // Create agent
        //Mock environment variable TEST_MAPPINGS to be equivalent to the file path for the mapping folder
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
        		ThingspeakInputAgent agent = new ThingspeakInputAgent(propertiesFile);
        		// Assert that the mappings were set
        	 
        
       
        String[] Timestamps = {"2022-11-09T03:05:18Z", "2022-11-09T03:06:18Z", "2022-11-09T03:07:18Z", "2022-11-09T03:08:18Z"};
        Map<String, List<?>> timeStampReadings = new HashMap<>();
        Map<String, List<?>> allReadings = new HashMap<>();
        
        // Make method accessible
        Method convertReadingsToTimeSeries = ThingspeakInputAgent.class.getDeclaredMethod("convertReadingsToTimeSeries", Map.class, Map.class);
        convertReadingsToTimeSeries.setAccessible(true);

        // Use readings only consisting of times, should give an error as keys are not covered
        try {
        	 // Create the readings //
            
            
            timeStampReadings.put(ThingspeakInputAgent.timestampKey, Arrays.asList(Timestamps));
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
            if(currentTimeSeries.getTimes().size() == timeStampReadings.get(ThingspeakInputAgent.timestampKey).size()) {
                // Number of IRIs should match the number of keys
                Assert.assertEquals(allTypesKeys.length, currentTimeSeries.getDataIRIs().size());
                for(String iri: currentTimeSeries.getDataIRIs()) {
                    List<?> values = currentTimeSeries.getValues(iri);
                    // The size of value should match the number of time stamps
                    Assert.assertEquals(timeStampReadings.get(ThingspeakInputAgent.timestampKey).size(), values.size());
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
        Method convertStringToOffsetDateTime = ThingspeakInputAgent.class.getDeclaredMethod("convertStringToOffsetDateTime", String.class);
        convertStringToOffsetDateTime.setAccessible(true);
        // Test with a valid string
        String ts_01 = "2022-11-09T03:05:18Z";
        OffsetDateTime time = (OffsetDateTime) convertStringToOffsetDateTime.invoke(testAgent, ts_01);
        Assert.assertEquals(2022, time.getYear());
        Assert.assertEquals(11, time.getMonth().getValue());
        Assert.assertEquals(9, time.getDayOfMonth());
        Assert.assertEquals(03, time.getHour());
        Assert.assertEquals(05, time.getMinute());
        Assert.assertEquals(18, time.getSecond());
        Assert.assertEquals(00, time.getOffset().getTotalSeconds());
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
        Method pruneTimeSeries = ThingspeakInputAgent.class.getDeclaredMethod("pruneTimeSeries", TimeSeries.class, OffsetDateTime.class);
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
