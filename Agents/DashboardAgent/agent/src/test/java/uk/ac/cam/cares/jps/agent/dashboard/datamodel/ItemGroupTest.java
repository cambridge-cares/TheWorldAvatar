package uk.ac.cam.cares.jps.agent.dashboard.datamodel;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class ItemGroupTest {
    protected static final String DATABASE_KEY = "db";
    protected static final String TABLE_KEY = "table";
    private static ItemGroup SAMPLE;
    private static final String HEAT_MEASURE_NAME = "Heat";
    private static final String SAMPLE_HEAT_ITEM_NAME = "Overhead light 1";
    private static final String SAMPLE_HEAT_DATA_IRI = TestUtils.genInstance("measure");
    private static final String SAMPLE_HEAT_TS_IRI = TestUtils.genTimeSeriesInstance();

    @BeforeEach
    void setup() {
        SAMPLE = new ItemGroup();
    }

    @Test
    void testAddMeasure_and_GetMeasure() {
        // Execute method
        SAMPLE.addMeasure(MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_UNIT, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_DATA_IRI, MeasureTest.SAMPLE_ONE_TS_IRI);
        Measure results = SAMPLE.getMeasure(MeasureTest.SAMPLE_MEASURE_NAME);
        // Verify results
        MeasureTest.verifyTimeSeriesIriResult(results.getAllTimeSeriesIris(), MeasureTest.genExpectedItemAndIriMappings(
                new String[]{MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_DATA_IRI, MeasureTest.SAMPLE_ONE_TS_IRI}
        ));
    }

    @Test
    void testAddMeasure_and_GetMeasures_SingleAddition() {
        // Execute method
        SAMPLE.addMeasure(MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_UNIT, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_DATA_IRI, MeasureTest.SAMPLE_ONE_TS_IRI);
        Queue<Measure> results = SAMPLE.getMeasures();
        // Verify results
        assertEquals(1, results.size());
        verifyItemGroupMeasures(results, genExpectedMeasureItemMappings(
                new String[]{MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_DATA_IRI, MeasureTest.SAMPLE_ONE_TS_IRI}
        ));
    }

    @Test
    void testAddMeasure_and_GetMeasures_MultipleItemsSingleMeasure() {
        // Execute method
        SAMPLE.addMeasure(MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_UNIT, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_DATA_IRI, MeasureTest.SAMPLE_ONE_TS_IRI);
        SAMPLE.addMeasure(MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_UNIT, MeasureTest.SAMPLE_TWO_ITEM_NAME, MeasureTest.SAMPLE_TWO_DATA_IRI, MeasureTest.SAMPLE_TWO_TS_IRI);
        Queue<Measure> results = SAMPLE.getMeasures();
        // Verify results
        assertEquals(1, results.size()); // Duplicate measure names will be ignored
        verifyItemGroupMeasures(results, genExpectedMeasureItemMappings(
                new String[]{MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_DATA_IRI, MeasureTest.SAMPLE_ONE_TS_IRI},
                new String[]{MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_TWO_ITEM_NAME, MeasureTest.SAMPLE_TWO_DATA_IRI, MeasureTest.SAMPLE_TWO_TS_IRI}
        ));
    }

    @Test
    void testAddMeasure_and_GetMeasures_SingleItemMultipleMeasures() {
        // Execute method
        SAMPLE.addMeasure(MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_UNIT, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_DATA_IRI, MeasureTest.SAMPLE_ONE_TS_IRI);
        SAMPLE.addMeasure(HEAT_MEASURE_NAME, MeasureTest.SAMPLE_UNIT, SAMPLE_HEAT_ITEM_NAME, SAMPLE_HEAT_DATA_IRI, SAMPLE_HEAT_TS_IRI);
        Queue<Measure> results = SAMPLE.getMeasures();
        // Verify results
        assertEquals(2, results.size());
        verifyItemGroupMeasures(results, genExpectedMeasureItemMappings(
                new String[]{MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_DATA_IRI, MeasureTest.SAMPLE_ONE_TS_IRI},
                new String[]{HEAT_MEASURE_NAME, SAMPLE_HEAT_ITEM_NAME, SAMPLE_HEAT_DATA_IRI, SAMPLE_HEAT_TS_IRI}
        ));
    }

    @Test
    void testAddTimeSeries_and_GetMeasures_SingleAddition() {
        // Set up
        SAMPLE.addMeasure(MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_UNIT, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_DATA_IRI, MeasureTest.SAMPLE_ONE_TS_IRI);
        // Execute method
        SAMPLE.addTimeSeries(MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_TS_COL, MeasureTest.SAMPLE_ONE_TS_TABLE, MeasureTest.SAMPLE_ONE_TS_DATABASE);
        Queue<Measure> results = SAMPLE.getMeasures();
        // Verify results
        assertEquals(1, results.size()); // Duplicate measure names will be ignored
        verifyItemGroupTimeSeriesMeasures(results, genExpectedMeasureTimeSeriesMappings(
                new String[]{MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_TS_COL, MeasureTest.SAMPLE_ONE_TS_TABLE, MeasureTest.SAMPLE_ONE_TS_DATABASE}
        ));
    }

    @Test
    void testAddTimeSeries_and_GetMeasures_MultipleItemsSingleMeasure() {
        // Set up
        SAMPLE.addMeasure(MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_UNIT, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_DATA_IRI, MeasureTest.SAMPLE_ONE_TS_IRI);
        // Execute method
        SAMPLE.addTimeSeries(MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_TS_COL, MeasureTest.SAMPLE_ONE_TS_TABLE, MeasureTest.SAMPLE_ONE_TS_DATABASE);
        SAMPLE.addTimeSeries(MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_TWO_ITEM_NAME, MeasureTest.SAMPLE_TWO_TS_COL, MeasureTest.SAMPLE_TWO_TS_TABLE, MeasureTest.SAMPLE_TWO_TS_DATABASE);
        Queue<Measure> results = SAMPLE.getMeasures();
        // Verify results
        assertEquals(1, results.size()); // Duplicate measure names will be ignored
        verifyItemGroupTimeSeriesMeasures(results, genExpectedMeasureTimeSeriesMappings(
                new String[]{MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_ONE_ITEM_NAME, MeasureTest.SAMPLE_ONE_TS_COL, MeasureTest.SAMPLE_ONE_TS_TABLE, MeasureTest.SAMPLE_ONE_TS_DATABASE},
                new String[]{MeasureTest.SAMPLE_MEASURE_NAME, MeasureTest.SAMPLE_TWO_ITEM_NAME, MeasureTest.SAMPLE_TWO_TS_COL, MeasureTest.SAMPLE_ONE_TS_TABLE, MeasureTest.SAMPLE_ONE_TS_DATABASE}
        ));
    }

    public static Map<String, Map<String, String[]>> genExpectedMeasureItemMappings(String[]... itemIriDataArray) {
        Map<String, Map<String, String[]>> expectedMeasureItemMappings = new HashMap<>();
        for (String[] itemIriData : itemIriDataArray) {
            String measure = itemIriData[0];
            String item = itemIriData[1];
            String dataIri = itemIriData[2];
            String timeSeriesIri = itemIriData[3];
            expectedMeasureItemMappings.computeIfAbsent(measure, k -> new HashMap<>())
                    .putAll(MeasureTest.genExpectedItemAndIriMappings(new String[]{item, dataIri, timeSeriesIri}));
        }
        return expectedMeasureItemMappings;
    }

    public static void verifyItemGroupMeasures(Queue<Measure> groupMeasureResults, Map<String, Map<String, String[]>> expectedMeasureItemMappings) {
        while (!groupMeasureResults.isEmpty()) {
            Measure currentMeasure = groupMeasureResults.poll();
            MeasureTest.verifyTimeSeriesIriResult(currentMeasure.getAllTimeSeriesIris(),
                    expectedMeasureItemMappings.get(currentMeasure.getName())
            );
        }
    }

    public static Map<String, Map<String, String>> genExpectedMeasureTimeSeriesMappings(String[]... timeSeriesDataArray) {
        Map<String, Map<String, String>> expectedMeasureTimeSeriesMappings = new HashMap<>();
        for (String[] timeSeriesData : timeSeriesDataArray) {
            String measure = timeSeriesData[0];
            String item = timeSeriesData[1];
            String column = timeSeriesData[2];
            String table = timeSeriesData[3];
            String database = timeSeriesData[4];
            Map<String, String> expectedNestedMappings = expectedMeasureTimeSeriesMappings.computeIfAbsent(measure, k -> new HashMap<>());
            expectedNestedMappings.putAll(MeasureTest.genExpectedItemAndColumnMappings(new String[]{item, column}));
            // Note that table and database will follow the first set of value inputs and ignore others for the same measure
            expectedNestedMappings.putIfAbsent(TABLE_KEY, table);
            expectedNestedMappings.putIfAbsent(DATABASE_KEY, database);
        }
        return expectedMeasureTimeSeriesMappings;
    }

    public static void verifyItemGroupTimeSeriesMeasures(Queue<Measure> groupMeasureResults, Map<String, Map<String, String>> expectedMeasureTimeSeriesMappings) {
        while (!groupMeasureResults.isEmpty()) {
            Measure currentMeasure = groupMeasureResults.poll();
            Map<String, String> expectedNestedMappings = expectedMeasureTimeSeriesMappings.get(currentMeasure.getName());
            // Note that table and database will follow the first set of value inputs
            assertEquals(expectedNestedMappings.get(TABLE_KEY), currentMeasure.getTimeSeriesTable());
            assertEquals(expectedNestedMappings.get(DATABASE_KEY), currentMeasure.getTimeSeriesDatabase());
            MeasureTest.verifyTimeSeriesMetadataResult(currentMeasure.getTimeSeriesData(), expectedNestedMappings);
        }
    }
}