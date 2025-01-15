package uk.ac.cam.cares.jps.agent.dashboard.datamodel;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;

import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;

import static org.junit.jupiter.api.Assertions.*;

class MeasureTest {
    private static Measure SAMPLE;
    protected static final String SAMPLE_MEASURE_NAME = "Electricity";
    protected static final String SAMPLE_UNIT = "kWh";
    protected static final String SAMPLE_ONE_ITEM_NAME = "Overhead light 1";
    protected static final String SAMPLE_ONE_DATA_IRI = TestUtils.genInstance("measure");
    protected static final String SAMPLE_ONE_TS_IRI = TestUtils.genTimeSeriesInstance();
    protected static final String SAMPLE_ONE_TS_COL = "column5";
    protected static final String SAMPLE_ONE_TS_TABLE = "v7asn1o2hn";
    protected static final String SAMPLE_ONE_TS_DATABASE = "elec";
    protected static final String SAMPLE_TWO_ITEM_NAME = "Earl Fridge";
    protected static final String SAMPLE_TWO_DATA_IRI = TestUtils.genInstance("measure");
    protected static final String SAMPLE_TWO_TS_IRI = TestUtils.genTimeSeriesInstance();
    protected static final String SAMPLE_TWO_TS_COL = "column2";
    protected static final String SAMPLE_TWO_TS_TABLE = "021br71ik";
    protected static final String SAMPLE_TWO_TS_DATABASE = "fridge";

    @BeforeEach
    void setup() {
        SAMPLE = new Measure(SAMPLE_MEASURE_NAME, SAMPLE_UNIT);
    }

    @Test
    void testGetName() {
        // Setup
        SAMPLE.addTimeSeriesIris(SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_DATA_IRI, SAMPLE_ONE_TS_IRI);
        // Execute method and verify results
        assertEquals(SAMPLE_MEASURE_NAME, SAMPLE.getName());
    }

    @Test
    void testGetUnit_AvailableUnit() {
        // Setup
        SAMPLE.addTimeSeriesIris(SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_DATA_IRI, SAMPLE_ONE_TS_IRI);
        // Execute method and verify results
        assertEquals(SAMPLE_UNIT, SAMPLE.getUnit());
    }

    @Test
    void testGetUnit_NoUnit() {
        // Setup
        Measure sample = new Measure(SAMPLE_MEASURE_NAME, null);
        sample.addTimeSeriesIris(SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_DATA_IRI, SAMPLE_ONE_TS_IRI);
        // Execute method and verify results
        assertEquals(null, sample.getUnit());
    }

    @Test
    void testAddTimeSeriesIris_and_GetAllTimeSeriesIris_OneSet() {
        // Execute method
        SAMPLE.addTimeSeriesIris(SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_DATA_IRI, SAMPLE_ONE_TS_IRI);
        Queue<String[]> results = SAMPLE.getAllTimeSeriesIris();
        // Verify results
        verifyTimeSeriesIriResult(results, genExpectedItemAndIriMappings(
                new String[]{SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_DATA_IRI, SAMPLE_ONE_TS_IRI}
        ));
    }

    @Test
    void testAddTimeSeriesIris_and_GetAllTimeSeriesIris_MultipleSets() {
        // Execute method
        SAMPLE.addTimeSeriesIris(SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_DATA_IRI, SAMPLE_ONE_TS_IRI);
        SAMPLE.addTimeSeriesIris(SAMPLE_TWO_ITEM_NAME, SAMPLE_TWO_DATA_IRI, SAMPLE_TWO_TS_IRI);
        Queue<String[]> results = SAMPLE.getAllTimeSeriesIris();
        // Verify results
        verifyTimeSeriesIriResult(results, genExpectedItemAndIriMappings(
                new String[]{SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_DATA_IRI, SAMPLE_ONE_TS_IRI},
                new String[]{SAMPLE_TWO_ITEM_NAME, SAMPLE_TWO_DATA_IRI, SAMPLE_TWO_TS_IRI}
        ));
    }

    @Test
    void testAddTimeSeriesMetadata_and_GetTimeSeriesData_OneSet() {
        // Execute method
        SAMPLE.addTimeSeriesMetadata(SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_TS_COL, SAMPLE_ONE_TS_TABLE, SAMPLE_ONE_TS_DATABASE);
        Queue<String[]> results = SAMPLE.getTimeSeriesData();
        // Verify results
        verifyTimeSeriesMetadataResult(results, genExpectedItemAndColumnMappings(
                new String[]{SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_TS_COL}
        ));
    }

    @Test
    void testAddTimeSeriesMetadata_and_GetTimeSeriesData_MultipleSets() {
        // Execute method
        SAMPLE.addTimeSeriesMetadata(SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_TS_COL, SAMPLE_ONE_TS_TABLE, SAMPLE_ONE_TS_DATABASE);
        SAMPLE.addTimeSeriesMetadata(SAMPLE_TWO_ITEM_NAME, SAMPLE_TWO_TS_COL, SAMPLE_TWO_TS_TABLE, SAMPLE_TWO_TS_DATABASE);
        Queue<String[]> results = SAMPLE.getTimeSeriesData();
        // Verify results
        verifyTimeSeriesMetadataResult(results, genExpectedItemAndColumnMappings(
                new String[]{SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_TS_COL},
                new String[]{SAMPLE_TWO_ITEM_NAME, SAMPLE_TWO_TS_COL}
        ));
    }

    @Test
    void testGetTimeSeriesTable() {
        // Setup
        SAMPLE.addTimeSeriesMetadata(SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_TS_COL, SAMPLE_ONE_TS_TABLE, SAMPLE_ONE_TS_DATABASE);
        SAMPLE.addTimeSeriesMetadata(SAMPLE_TWO_ITEM_NAME, SAMPLE_TWO_TS_COL, SAMPLE_TWO_TS_TABLE, SAMPLE_TWO_TS_DATABASE);
        // Execute method and verify results
        assertEquals(SAMPLE_ONE_TS_TABLE, SAMPLE.getTimeSeriesTable()); // Table should follow the first iteration
    }

    @Test
    void testGetTimeSeriesDatabase() {
        // Setup
        SAMPLE.addTimeSeriesMetadata(SAMPLE_ONE_ITEM_NAME, SAMPLE_ONE_TS_COL, SAMPLE_ONE_TS_TABLE, SAMPLE_ONE_TS_DATABASE);
        SAMPLE.addTimeSeriesMetadata(SAMPLE_TWO_ITEM_NAME, SAMPLE_TWO_TS_COL, SAMPLE_TWO_TS_TABLE, SAMPLE_TWO_TS_DATABASE);
        // Execute method and verify results
        assertEquals(SAMPLE_ONE_TS_DATABASE, SAMPLE.getTimeSeriesDatabase()); // Database should follow the first iteration
    }

    public static Map<String, String[]> genExpectedItemAndIriMappings(String[]... itemIriDataArray) {
        Queue<String[]> expectedItemAndIriQueue = new ArrayDeque<>();
        for (String[] itemIriData : itemIriDataArray) {
            String item = itemIriData[0];
            String dataIri = itemIriData[1];
            String timeSeriesIri = itemIriData[2];
            expectedItemAndIriQueue.offer(new String[]{item, dataIri, timeSeriesIri});
        }
        return genExpectedItemAndIriMappings(expectedItemAndIriQueue);
    }

    public static Map<String, String[]> genExpectedItemAndIriMappings(Queue<String[]> itemIriDataQueue) {
        Map<String, String[]> expectedItemAndIriMappings = new HashMap<>();
        itemIriDataQueue.forEach(itemIriData -> {
            String item = itemIriData[0];
            String dataIri = itemIriData[1];
            String timeSeriesIri = itemIriData[2];
            expectedItemAndIriMappings.put(item, new String[]{dataIri, timeSeriesIri});
        });
        return expectedItemAndIriMappings;
    }

    public static void verifyTimeSeriesIriResult(Queue<String[]> timeSeriesIriResults, Map<String, String[]> expectedItemAndIriMappings) {
        assertEquals(expectedItemAndIriMappings.keySet().size(), timeSeriesIriResults.size());
        while (!timeSeriesIriResults.isEmpty()) {
            String[] timeSeriesIris = timeSeriesIriResults.poll();
            String[] expectedIri = expectedItemAndIriMappings.get(timeSeriesIris[0]);
            assertEquals(expectedIri[0], timeSeriesIris[1]); // Data IRI
            assertEquals(expectedIri[1], timeSeriesIris[2]); // Time Series IRI
        }
    }

    public static Map<String, String> genExpectedItemAndColumnMappings(String[]... itemColDataArray) {
        Queue<String[]> expectedItemAndColQueue = new ArrayDeque<>();
        for (String[] itemColData : itemColDataArray) {
            String item = itemColData[0];
            String column = itemColData[1];
            expectedItemAndColQueue.offer(new String[]{item, column});
        }
        return genExpectedItemAndColumnMappings(expectedItemAndColQueue);
    }

    public static Map<String, String> genExpectedItemAndColumnMappings(Queue<String[]> itemColDataArray) {
        Map<String, String> expectedItemAndColMappings = new HashMap<>();
        itemColDataArray.forEach(itemColData -> {
            String item = itemColData[0];
            String column = itemColData[1];
            expectedItemAndColMappings.put(item,column);
        });
        return expectedItemAndColMappings;
    }

    public static void verifyTimeSeriesMetadataResult(Queue<String[]> timeSeriesMetadataResults, Map<String, String> expectedItemAndColMappings) {
        while (!timeSeriesMetadataResults.isEmpty()) {
            String[] timeSeriesResults = timeSeriesMetadataResults.poll();
            String expectedCol = expectedItemAndColMappings.get(timeSeriesResults[0]);
            assertEquals(expectedCol, timeSeriesResults[1]); // RDB column
        }
    }
}