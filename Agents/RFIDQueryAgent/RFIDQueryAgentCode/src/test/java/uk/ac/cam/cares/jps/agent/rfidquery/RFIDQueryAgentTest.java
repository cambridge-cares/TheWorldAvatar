package uk.ac.cam.cares.jps.agent.rfidquery;

import org.json.JSONObject;
import org.junit.*;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import java.io.*;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;

public class RFIDQueryAgentTest {
    // The default instance used in the tests
    private RFIDQueryAgent testAgent;

    //list of example readings for Out status
    private ArrayList<String> ValuesOut;

    //list of example readings for In status
    private ArrayList<String> ValuesIn;

    //timestamp for Out status with exceed threshold
    String timestampOutExceedThreshold;

    //timestamp for Out status without exceed threshold
    String timestampOutNonExceedThreshold;

    //timestamp for In status
    String timestampIn;

    //list of IRIs for Out status
    private ArrayList<String> testIRIsOut;

    //list of IRIs for In status
    private ArrayList<String> testIRIsIn;

    //list of timestamps for Out status with exceed threshold
    private List<OffsetDateTime> timesOutExceedThreshold;

    //list of timestamps for Out status without exceed threshold
    private List<OffsetDateTime> timesOutNonExceedThreshold;

    //list of timestamps for In status
    private List<OffsetDateTime> timesIn;

    //single key for mocking RFID tag status data IRI
    private final String key = "tag_01_status";

    // Example prefix for IRIs
    private final String examplePrefix = "example:prefix/api_";

    //timeseries object for Out status with exceed threshold
    TimeSeries<OffsetDateTime> testTimeSeriesOutExceedThreshold;

    //timeseries object for Out status without exceed threshold
    TimeSeries<OffsetDateTime> testTimeSeriesOutNonExceedThreshold;

    //timeseries object for In status
    TimeSeries<OffsetDateTime> testTimeSeriesIn;

    //JSONObject
    JSONObject results;

    @Before
    public void initializeAgent() throws IOException {
        testAgent = new RFIDQueryAgent(examplePrefix+key, "10");
    }
   
    @Before
    public void createOutExampleReadingsWithExceedThresholdTimestamp() {
        String value = "Out";

        ValuesOut = new ArrayList<>();
        ValuesOut.add(0, value);

        List<List<?>> valuesOut = new ArrayList<>();
        valuesOut.add(ValuesOut);

        timestampOutExceedThreshold = "2022-11-09T03:06:18";
        
        testIRIsOut = new ArrayList<>();
        testIRIsOut.add(examplePrefix+key);

        
        timesOutExceedThreshold = new ArrayList<>();
        timesOutExceedThreshold.add(0, convertStringToOffsetDateTime(timestampOutExceedThreshold));
        testTimeSeriesOutExceedThreshold = new TimeSeries<>(timesOutExceedThreshold, testIRIsOut, valuesOut);
    }

    @Before
    public void createOutExampleReadingsWithNonExceedThresholdTimestamp() {
        String value = "Out";

        ValuesOut = new ArrayList<>();
        ValuesOut.add(0, value);

        List<List<?>> valuesOut = new ArrayList<>();
        valuesOut.add(ValuesOut);

        long timestampLong = System.currentTimeMillis();
        Date date = new java.util.Date(timestampLong);
        SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        Object ts = sdf.format(date);

        timestampOutNonExceedThreshold = ts.toString();
        
        testIRIsOut = new ArrayList<>();
        testIRIsOut.add(examplePrefix+key);

        
        timesOutNonExceedThreshold = new ArrayList<>();
        timesOutNonExceedThreshold.add(0, convertStringToOffsetDateTime(timestampOutNonExceedThreshold));
        testTimeSeriesOutNonExceedThreshold = new TimeSeries<>(timesOutNonExceedThreshold, testIRIsOut, valuesOut);
    }

    @Before
    public void createInExampleReadings() {
        String value = "In";

        ValuesIn = new ArrayList<>();
        ValuesIn.add(0, value);

        List<List<?>> valuesIn = new ArrayList<>();
        valuesIn.add(ValuesIn);

        long timestampLong = System.currentTimeMillis();
        Date date = new java.util.Date(timestampLong);
        SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
        Object ts = sdf.format(date);

        timestampIn = ts.toString();
        
        testIRIsIn = new ArrayList<>();
        testIRIsIn.add(examplePrefix+key);

        
        timesIn = new ArrayList<>();
        timesIn.add(0, convertStringToOffsetDateTime(timestampIn));
        testTimeSeriesIn = new TimeSeries<>(timesIn, testIRIsIn, valuesIn);
    }

    @Test
    public void testcheckRFIDStatusOut_ExceedThresholdTrue(){
        long hours = 1;

        results = testAgent.checkRFIDStatusThreshold(testTimeSeriesOutExceedThreshold, examplePrefix+key, hours);

        Assert.assertEquals(true, results.getBoolean("exceedThreshold"));
        Assert.assertEquals("2022-11-09T03:06:18Z", results.getString("timestamp"));
        Assert.assertEquals(examplePrefix+key, results.get("dataIRI"));
    }

    @Test
    public void testcheckRFIDStatusOut_ExceedThresholdFalse(){
        long hours = 1;

        results = testAgent.checkRFIDStatusThreshold(testTimeSeriesOutNonExceedThreshold, examplePrefix+key, hours);

        Assert.assertEquals(false, results.getBoolean("exceedThreshold"));
        Assert.assertEquals(examplePrefix+key, results.get("dataIRI"));
    }

    @Test
    public void testcheckRFIDStatusIn(){
        long hours = 1;

        results = testAgent.checkRFIDStatusThreshold(testTimeSeriesIn, examplePrefix+key, hours);
        
        Assert.assertEquals(false, results.getBoolean("exceedThreshold"));
        Assert.assertEquals(examplePrefix+key, results.get("dataIRI"));
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

}
