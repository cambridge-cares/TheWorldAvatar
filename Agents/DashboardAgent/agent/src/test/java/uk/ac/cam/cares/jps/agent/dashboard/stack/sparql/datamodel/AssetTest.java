package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.datamodel;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;

import java.util.Queue;

import static org.junit.jupiter.api.Assertions.*;

class AssetTest {
    private static final String ASSET_NAME = "Lamp1";
    private static final String ASSET_TYPE = "Lamp";
    private static final String MEASURE_ONE_NAME = "Electricity Consumption";
    private static final String MEASURE_ONE_UNIT = "kW";
    private static final String MEASURE_TWO_NAME = "On off state";
    private static final String MEASURE_TWO_UNIT = null;
    private static String MEASURE_ONE_IRI;
    private static String MEASURE_ONE_TS_IRI;
    private static String MEASURE_TWO_IRI;
    private static String MEASURE_TWO_TS_IRI;

    @BeforeAll
    static void setup() {
        MEASURE_ONE_IRI = TestUtils.genInstance("ElectricityConsumption");
        MEASURE_ONE_TS_IRI = TestUtils.genTimeSeriesInstance();
        MEASURE_TWO_IRI = TestUtils.genInstance("State");
        MEASURE_TWO_TS_IRI = TestUtils.genTimeSeriesInstance();
    }

    @Test
    void testAddMeasure() {
        // Initialise object
        Asset sample = new Asset(ASSET_NAME, ASSET_TYPE, MEASURE_ONE_NAME, MEASURE_ONE_UNIT, MEASURE_ONE_IRI, MEASURE_ONE_TS_IRI);
        // Execute method
        sample.addMeasure(MEASURE_TWO_NAME, MEASURE_TWO_UNIT, MEASURE_TWO_IRI, MEASURE_TWO_TS_IRI);
        // Verify it has been added properly
        Queue<String[]> results = sample.getAssetData();
        assertEquals(2, results.size()); // Two measure arrays should be available in the queue
        // For the first measure
        String[] measure = results.poll();
        // If unit is null, the corresponding unit stored should be null as well
        verifyAssetMeasureArrayContents(ASSET_TYPE, MEASURE_TWO_NAME, MEASURE_TWO_IRI, MEASURE_TWO_TS_IRI, null, measure);
        // For the second measure
        measure = results.poll();
        verifyAssetMeasureArrayContents(ASSET_TYPE, MEASURE_ONE_NAME, MEASURE_ONE_IRI, MEASURE_ONE_TS_IRI, MEASURE_ONE_UNIT, measure);
    }

    @Test
    void testGetAssetName() {
        // Initialise object
        Asset sample = new Asset(ASSET_NAME, ASSET_TYPE, MEASURE_ONE_NAME, MEASURE_ONE_UNIT, MEASURE_ONE_IRI, MEASURE_ONE_TS_IRI);
        // Execute and test method
        assertEquals(ASSET_NAME, sample.getAssetName());
    }

    @Test
    void testGetAssetData() {
        // Initialise object
        Asset sample = new Asset(ASSET_NAME, ASSET_TYPE, MEASURE_ONE_NAME, MEASURE_ONE_UNIT, MEASURE_ONE_IRI, MEASURE_ONE_TS_IRI);
        // Execute method to retrieve the simple measure
        Queue<String[]> results = sample.getAssetData();
        // Test results
        assertEquals(1, results.size()); // Only one measure array should be available in the queue
        String[] measure = results.poll();
        verifyAssetMeasureArrayContents(ASSET_TYPE, MEASURE_ONE_NAME, MEASURE_ONE_IRI, MEASURE_ONE_TS_IRI, MEASURE_ONE_UNIT, measure);
    }

    protected static void verifyAssetMeasureArrayContents(String assetType, String measureName, String measureIRI, String measureTSIri, String measureUnit, String[] measure) {
        assertEquals(5, measure.length); // Verify that there are 5 measures included
        assertEquals(measureName, measure[0]);
        assertEquals(measureIRI, measure[1]);
        assertEquals(measureTSIri, measure[2]);
        assertEquals(measureUnit, measure[3]);
        assertEquals(assetType, measure[4]);
    }
}