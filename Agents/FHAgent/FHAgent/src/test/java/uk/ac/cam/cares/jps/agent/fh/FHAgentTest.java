package uk.ac.cam.cares.jps.agent.fh;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.skyscreamer.jsonassert.*;

import com.bigdata.service.ndx.pipeline.IndexWriteTask.M;
import com.github.stefanbirkner.systemlambda.SystemLambda;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import wiremock.org.eclipse.jetty.util.ajax.JSON;

import java.io.*;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;



public class FHAgentTest {
    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    // The default instance used in the tests
    private FHAgent testAgent;
    // The mocking instance for the time series client
    @SuppressWarnings("unchecked")
    private final TimeSeriesClient<OffsetDateTime> mockTSClient = (TimeSeriesClient<OffsetDateTime>) Mockito.mock(TimeSeriesClient.class);

    // A default list of IRIs
    private final List<String> iris = Arrays.asList("iri1");
    // Default list of JSON keys
    private final String[] keys = {"occupiedState1", "occupiedState2"};
    
    //Default list of timestamps

    //Mock set of keys from Thingsboard
    List<String> mockKeys = Arrays.asList("avgDist1", "avgDist2");

    // Readings used by several tests
    JSONObject allReadings;
    JSONObject timestampTestReading;
    JSONObject expectedTally;

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

    @Before
    public void initializeAgent() throws IOException {
        // Create a properties file that points to a dummy mapping folder //
        // Create an empty folder
        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Add mapping file into the empty folder
        String mappingFile1 = Paths.get(mappingFolder.getAbsolutePath(), "example_mapping1.properties").toString();
        ArrayList<String> mappings1 = new ArrayList<>();
       
        mappings1.add(keys[0] + "=example:prefix/api_" + keys[0]);
        writePropertyFile(mappingFile1, mappings1);

        String mappingFile2 = Paths.get(mappingFolder.getAbsolutePath(), "example_mapping2.properties").toString();
        ArrayList<String> mappings2 = new ArrayList<>();
        mappings2.add(keys[1] + "=example:prefix/api_" + keys[1]);
        writePropertyFile(mappingFile2, mappings2);
        // Filepath for the properties file
        
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writePropertyFile(propertiesFile, Arrays.asList(new String[]{"thingsboard.mappingfolder=TEST_MAPPINGS", "derivation.mapping=avgDist1:occupiedState1,avgDist2:occupiedState2", "threshold.tally = 170."}));
        // To create testAgent without an exception being thrown, SystemLambda is used to mock an environment variable
        // To mock the environment variable, a try catch need to be used
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
        		 testAgent = new FHAgent(propertiesFile);
        	 });
        }
        // There should not be any exception thrown as the agent is initiated correctly
        catch (Exception e) {
            System.out.println(e);
        }
        // Set the mocked time series client
        testAgent.setTsClient(mockTSClient);
    }   

    @Before
    public void createExampleReadings() {

        allReadings = new JSONObject();
        expectedTally = new JSONObject();
        
        JSONArray avgDistMeasurements1 = new JSONArray();
        JSONArray tallied1 = new JSONArray();

        JSONArray avgDistMeasurements2 = new JSONArray();
        JSONArray tallied2 = new JSONArray();

        Double[] measurements1 = {350.,   350.,  220.,  210., 170.,  175., 165., 150., 155., 150., 150., 165., 180., 350., 350.};
        //tally =               {0,      0,     0,     0,    0.35,  0.20, 0.35, 0.70, 1.15(on), 1.50, 1.85, 2.00, 1.65, 1.30, 1.05}
        Double[] tallies1 =     {0., 0., 0., 0., 0., 0., 0., 0., 1., 1., 1., 1., 1., 1., 1.};

        Double[] measurements2 = {150.,   150.,  150.,  150., 170., 175., 195., 210., 350., 350., 350., 350., 350., 350., 350.};
        //tally =                {0.35,    0.7,  1.05,  1.40, 1.75, 1.60, 1.45, 1.30, 1.15, 1.00, 0.85, 0.70, 0.55, 0.40, 0.25}
        Double[] tallies2 =     {0., 0.,  1.,  1., 1., 1., 1., 1., 1., 1., 0., 0., 0., 0., 0.};

        int onIndex = 8;
        long ts = 1234560000000L;
        long offset = 1500L;

        JSONObject rowTally1 = new JSONObject();
        rowTally1.put(FHAgent.timestampKey, ts );
        rowTally1.put("value", tallies1[0]);

        tallied1.put(rowTally1);

        JSONObject rowTally2 = new JSONObject();
        rowTally2.put(FHAgent.timestampKey, ts+ onIndex *1000);
        rowTally2.put("value", tallies1[onIndex]);

        tallied1.put(rowTally2);


        long ts2 = 1234560000000L + offset;

        JSONObject rowTally3 = new JSONObject();
        rowTally3.put(FHAgent.timestampKey, ts2);
        rowTally3.put("value", tallies2[0]);

        tallied2.put(rowTally3);

        JSONObject rowTally4 = new JSONObject();
        rowTally4.put(FHAgent.timestampKey, ts2+ 2 *1000 );
        rowTally4.put("value", tallies2[2]);

        tallied2.put(rowTally4);

        JSONObject rowTally5 = new JSONObject();
        rowTally5.put(FHAgent.timestampKey, ts2+ 10 *1000 );
        rowTally5.put("value", tallies2[10]);

        tallied2.put(rowTally5);

        for (Double dist: measurements1) {
            JSONObject row = new JSONObject();

            row.put(FHAgent.timestampKey, ts);
            row.put("value", dist);

            avgDistMeasurements1.put(row);
            ts += 1000;
        }

        
        for (Double dist: measurements2) {
            JSONObject row = new JSONObject();

            row.put(FHAgent.timestampKey, ts2);
            row.put("value", dist);

            avgDistMeasurements2.put(row);
            ts2 += 1000;
        }


        allReadings.put("avgDist1",avgDistMeasurements1);
        JSONObject expected1 = new JSONObject();
        expected1.put("occupiedState1", tallied1);
        expectedTally.put("avgDist1", expected1);
        
        allReadings.put("avgDist2",avgDistMeasurements2);
        JSONObject expected2 = new JSONObject();
        expected2.put("occupiedState2", tallied2);
        expectedTally.put("avgDist2", expected2);

        timestampTestReading = new JSONObject();
        timestampTestReading.put("avgDist1",avgDistMeasurements1);

    }

    @Test
    public void testConstructor() throws IOException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        // Run constructor on an empty file should give an exception
        writePropertyFile(propertiesFile, new ArrayList<>());
        try {
            new FHAgent(propertiesFile);
            Assert.fail();
        }
        
        catch (IOException e) {
            Assert.assertEquals("The key thingsboard.mappingfolder cannot be found in the properties file.", e.getMessage());
        }
       
        // Create a property file with a mapping folder that does not exist
        String folderName = "no_valid_folder";
        writePropertyFile(propertiesFile, Collections.singletonList("thingsboard.mappingfolder=" + folderName));
        // Run constructor that should give an exception
        try {
            new FHAgent(propertiesFile);
            Assert.fail();
        }
        catch (InvalidPropertiesFormatException e) {
        	Assert.assertEquals("The properties file does not contain the key thingsboard.mappingfolder " +
                    "with a path to the folder containing the required JSON key to IRI mappings.", e.getMessage());
        }

        // Create an empty folder
        folderName = "mappings_test";
        File mappingFolder = folder.newFolder(folderName);
        // Create a property file with the empty folder
        folderName = mappingFolder.getCanonicalPath().replace("\\","/");
        writePropertyFile(propertiesFile, Collections.singletonList("thingsboard.mappingfolder=TEST_MAPPINGS"));
        // Run constructor that should give an exception
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
        		new FHAgent(propertiesFile);
        		Assert.fail();
        	 });
        }
        catch (Exception e) {
        	Assert.assertTrue(e.getMessage().contains("No files in the folder:"));
        }

        // Add mapping files into the empty folder
        // All IRIs set
        String firstMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "firstMapping.properties").toString();
        String[] keys = {"occupiedState"};
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
        		FHAgent agent = new FHAgent(propertiesFile);
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
        Method getClassFromJSONKey = FHAgent.class.getDeclaredMethod("getClassFromJSONKey", String.class);
        getClassFromJSONKey.setAccessible(true);
        // No specific key should return the string class
        Assert.assertEquals(String.class, getClassFromJSONKey.invoke(testAgent, "ts"));
        // Environment conditions should be double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "avgDist"));

    }


    @Test
    public void testTallyDist () throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Method TallyDist = FHAgent.class.getDeclaredMethod("TallyDist", JSONObject.class, String.class);
        TallyDist.setAccessible(true);

        for (String key : mockKeys){
            JSONObject actual = (JSONObject) TallyDist.invoke(testAgent, allReadings, key);
            JSONAssert.assertEquals(expectedTally.getJSONObject(key), actual, true);
        }

    }

    @Test
    public void testUpdateData() {
        // Set up the mock client
        //Setup mocked TS
        
        List<OffsetDateTime> times = new ArrayList<OffsetDateTime>();
        List<String> dataIRI = new ArrayList<String>();
        List<Boolean> val = new ArrayList<Boolean>();
        List<List<Boolean>> values = new ArrayList<>();

        times.add(OffsetDateTime.parse("1970-01-01T00:00:00+00:00"));
        dataIRI.add("example:prefix/api_occupiedState");
        val.add(false);
        values.add(val);

        TimeSeries<OffsetDateTime> lastData =  new TimeSeries(times, dataIRI, values);

        // Use a max time that is clearly before any of the example readings
    	Mockito.when(mockTSClient.getMaxTime(Mockito.anyString())).thenReturn(OffsetDateTime.parse("1970-01-01T00:00:00+00:00"));
        Mockito.when(testAgent.getTS(Mockito.anyString())).thenReturn(lastData);
        //Mockito.doReturn(lastData).when(testAgent).getTS(Mockito.anyString());
        //Mockito.doReturn(lastData).when(mockTSClient).getTS(Mockito.anyString());
        // Run the update
        testAgent.updateData(allReadings, mockKeys);
        // Capture the arguments that the add data method was called with
        @SuppressWarnings("unchecked")
        ArgumentCaptor<TimeSeries<OffsetDateTime>> timeSeriesArgument = ArgumentCaptor.forClass(TimeSeries.class);
        // Ensure that the update was called for each time series
        Mockito.verify(mockTSClient, Mockito.times(testAgent.getNumberOfTimeSeries())).addTimeSeriesData(timeSeriesArgument.capture());
        // Ensure that the timeseries objects have the correct structure
        int numIRIs = 0;
        int[] argNum = {2,3};

        int tsTestInd = 0;
        for(TimeSeries<OffsetDateTime> ts: timeSeriesArgument.getAllValues()) {
        	
            // Check that number of timestamps is correct
            Assert.assertEquals(argNum[tsTestInd], ts.getTimes().size());
            numIRIs = numIRIs + ts.getDataIRIs().size();
            tsTestInd += 1;
        }
        // Number of unique keys in readings should match the number of IRIs
        Set<String> uniqueKeys = new HashSet<>(allReadings.keySet());
        uniqueKeys.addAll(allReadings.keySet());
       
        Assert.assertEquals(uniqueKeys.size(), numIRIs);
    }

    @Test
    public void testUpdateDataPruneAll() {
        // Use a max time that is past max time of readings
    	JSONArray tsAndValues = allReadings.getJSONArray("avgDist1");
    	long timestamp = tsAndValues.getJSONObject(0).getLong("ts");
    	Date date = new java.util.Date(timestamp + 1234);
    	SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
    	String maxTime = sdf.format(date);
    	
        OffsetDateTime endTime = OffsetDateTime.parse(maxTime+"+00:00");
        Mockito.when(mockTSClient.getMaxTime(Mockito.anyString())).thenReturn(endTime.plusDays(1));
        // Run the update
        testAgent.updateData(allReadings, mockKeys);
        // Ensure that the update is never called
        Mockito.verify(mockTSClient, Mockito.never()).addTimeSeriesData(Mockito.any());
    }

    @Test
    public void testJsonObjectToMapEmptyReadings() throws NoSuchMethodException, InvocationTargetException,
            IllegalAccessException {
        JSONObject readings = new JSONObject("{}");
        // Make method accessible
        Method jsonObjectToMap = FHAgent.class.getDeclaredMethod("jsonObjectToMap", JSONObject.class);
        jsonObjectToMap.setAccessible(true);
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readingsMap = (Map<String, List<?>>) jsonObjectToMap.invoke(testAgent, readings);
        // The map should be empty
        Assert.assertTrue(readingsMap.isEmpty());
    }
    
    @Test
    public void testJsonObjectToMapForTimeStampEmptyReadings() throws NoSuchMethodException, InvocationTargetException,
            IllegalAccessException {
        JSONObject readings = new JSONObject("{}");
        // Make method accessible
        Method jsonObjectToMapForTimeStamp = FHAgent.class.getDeclaredMethod("jsonObjectToMapForTimeStamp", JSONObject.class);
        jsonObjectToMapForTimeStamp.setAccessible(true);
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readingsMap = (Map<String, List<?>>) jsonObjectToMapForTimeStamp.invoke(testAgent, readings);
        // The map should be empty
        Assert.assertTrue(readingsMap.isEmpty());
    }

    @Test
    public void testJsonObjectToMap() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make method accessible
        Method jsonObjectToMap = FHAgent.class.getDeclaredMethod("jsonObjectToMap", JSONObject.class);
        jsonObjectToMap.setAccessible(true);
        // Transform the readings
        for(String key: mockKeys){
            @SuppressWarnings("unchecked")
            Map<String, List<?>> readings = (Map<String, List<?>>) jsonObjectToMap.invoke(testAgent, expectedTally.get(key));
            
            for (String keyReadings: readings.keySet()) {
                JSONArray tsAndValues = expectedTally.getJSONObject(key).getJSONArray(keyReadings);
                Assert.assertEquals(tsAndValues.length(), readings.get(keyReadings).size());

                // Check that all keys have a list of the same size as the nested JSON Array
                for(int ind = 0; ind < readings.get(keyReadings).size(); ind ++){
                    Assert.assertEquals(readings.get(keyReadings).get(ind), expectedTally.getJSONObject(key).getJSONArray(keyReadings).getJSONObject(readings.get(keyReadings).size() -1 -ind).get("value"));
                }
            }
            
            

            // Check that all keys from the JSON Object have a corresponding entry
            for (Iterator<String> it = expectedTally.getJSONObject(key).keys(); it.hasNext();) {
                String keyRead = it.next();
                Assert.assertTrue(readings.containsKey(keyRead));
            }
        }
        
    }

    @Test
    public void testJsonObjectToMapForTimeStamp() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        //TODO test with different timstamp set?
        // Make method accessible
        Method jsonObjectToMapForTimeStamp = FHAgent.class.getDeclaredMethod("jsonObjectToMapForTimeStamp", JSONObject.class);
        jsonObjectToMapForTimeStamp.setAccessible(true);
        // Transform the readings
        
        
        @SuppressWarnings("unchecked")
        Map<String, List<?>> readings = (Map<String, List<?>>) jsonObjectToMapForTimeStamp.invoke(testAgent, timestampTestReading);
        // Check that all keys have a list of the same size as the nested JSON Array
        Assert.assertTrue(readings.containsKey(FHAgent.timestampKey));
        long ts = 1234560000000L + 14 * 1000;
        
        for (int i = readings.size() - 1; i >= 0;i --) {
            Date date = new java.util.Date(ts);
            SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
            Object ts01 = sdf.format(date);
            
            Assert.assertTrue(readings.get(FHAgent.timestampKey).contains(ts01));
            //TODO get retrieve last element instead?
            Assert.assertEquals(ts01, readings.get(FHAgent.timestampKey).get(i));
            ts -= 1000;
        }
        
        

        //Assert.assertEquals(allReadings.getJSONArray(keys[0]).length(), readings.get(FHAgent.timestampKey).size());
    }

   @Test
    public void testConvertReadingsToTimeSeries() throws IOException, NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {
        // Create an agent with mappings of small size //
        // Create a folder inside the temporary folder in which the mapping files will be
        File mappingFolder= folder.newFolder("mappings_test");
        // Define three sets of mappings
        String[] allTypesKeys = {"avgDist1", "avgDist2"};
       
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
        writePropertyFile(propertiesFile, Collections.singletonList("thingsboard.mappingfolder=TEST_MAPPINGS"));
        // Create agent
        //Mock environment variable TEST_MAPPINGS to be equivalent to the file path for the mapping folder
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
        		FHAgent agent = new FHAgent(propertiesFile);
        		// Assert that the mappings were set
        	 
        
       
        String[] Timestamps = {"2021-07-11T16:10:00", "2021-07-11T16:15:00",
                "2021-07-11T16:20:00", "2021-07-11T16:25:00"};
        Map<String, List<?>> timeStampReadings = new HashMap<>();
        Map<String, List<?>> allReadings = new HashMap<>();
        
        // Make method accessible
        Method convertReadingsToTimeSeries = FHAgent.class.getDeclaredMethod("convertReadingsToTimeSeries", Map.class, Map.class);
        convertReadingsToTimeSeries.setAccessible(true);

        // Use readings only consisting of times, should give an error as keys are not covered
        try {
        	 // Create the readings //
            
            
            timeStampReadings.put(FHAgent.timestampKey, Arrays.asList(Timestamps));
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
            if(currentTimeSeries.getTimes().size() == timeStampReadings.get(FHAgent.timestampKey).size()) {
                // Number of IRIs should match the number of keys
                Assert.assertEquals(allTypesKeys.length, currentTimeSeries.getDataIRIs().size());
                for(String iri: currentTimeSeries.getDataIRIs()) {
                    List<?> values = currentTimeSeries.getValues(iri);
                    // The size of value should match the number of time stamps
                    Assert.assertEquals(timeStampReadings.get(FHAgent.timestampKey).size(), values.size());
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
        Method convertStringToOffsetDateTime = FHAgent.class.getDeclaredMethod("convertStringToOffsetDateTime", String.class);
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
        Method pruneTimeSeries = FHAgent.class.getDeclaredMethod("pruneTimeSeries", TimeSeries.class, OffsetDateTime.class);
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

    @Test
    public void testCreateJSONRequest() {
        JSONObject expected = new JSONObject();
        expected.put("timeClass", "INSTANTANEOUS");
        JSONArray testTimestamps = new JSONArray(new String[]{"2009-02-13T21:20:00", "2009-02-13T21:20:01", "2009-02-13T21:20:02"});
        expected.put("ts", testTimestamps);
        JSONArray testValues = new JSONArray(new Double[]{1.,2.,3.});
        String testIRI = "example:prefix/api_"+keys[0];
        JSONObject testValPair = new JSONObject();
        testValPair.put(testIRI, testValues);
        expected.put("values", testValPair);

        Long actualTime = 1234560000000L;
        JSONObject input = new JSONObject();
        JSONArray col = new JSONArray();
        for(int i = 0; i < testTimestamps.length(); i++){
            JSONObject row = new JSONObject();
            row.put("ts", actualTime + 1000 * i);
            row.put("value", testValues.getDouble(i));
            col.put(row);
        }
        input.put(keys[0], col);

        JSONAssert.assertEquals(expected, testAgent.createJSONRequest(input, "INSTANTANEOUS"), true);
    }

}
