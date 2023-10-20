package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.datamodel;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.Map;
import java.util.Queue;

import static org.junit.jupiter.api.Assertions.*;

class OrganisationTest {
    private static final String ASSET_LAMP_ONE_NAME = "Lamp1";
    private static final String ASSET_LAMP_TWO_NAME = "Lamp2";
    private static final String ASSET_LAMP_TYPE = "Lamp";
    private static final String ASSET_FRIDGE_ONE_NAME = "F1";
    private static final String ASSET_FRIDGE_TYPE = "Fridge";
    private static final String ROOM_ONE_NAME = "Bedroom";
    private static final String ROOM_TWO_NAME = "Kitchen";
    private static final String MEASURE_ELEC_NAME = "Electricity Consumption";
    private static final String MEASURE_ELEC_CONCEPT = "ElectricityConsumption";
    private static final String MEASURE_ELEC_UNIT = "kW";
    private static final String MEASURE_STATE_NAME = "On off state";
    private static final String MEASURE_STATE_CONCEPT = "State";
    private static final String MEASURE_STATE_UNIT = null;
    private static final String THRESHOLD_NAME = "Scale";
    private static final String THRESHOLD_MIN = "1";
    private static final String THRESHOLD_MAX = "5";

    @Test
    void testGetAllMeasure() {
        // Set up IRIs
        String assetElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String assetElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String roomElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String roomElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation(ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, assetElectricityMeasureIri, assetElectricityTimeSeriesIri);
        sample.addRoom(ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, roomElectricityMeasureIri, roomElectricityTimeSeriesIri);
        // Execute method
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        // Test results
        assertEquals(2, results.size()); // Only one asset and room expected
        // Retrieve and verify contents of asset
        Queue<String[]> assetMetadataResult = results.get(ASSET_LAMP_ONE_NAME);
        assertEquals(1, assetMetadataResult.size()); // Only one measure expected
        String[] metadata = assetMetadataResult.poll();
        AssetTest.verifyAssetMeasureArrayContents(ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, assetElectricityMeasureIri, assetElectricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
        // Retrieve and verify contents of room
        Queue<String[]> roomMetadataResult = results.get(ROOM_ONE_NAME);
        assertEquals(1, roomMetadataResult.size()); // Only one measure expected
        metadata = roomMetadataResult.poll();
        RoomTest.verifyRoomMeasureArrayContents(MEASURE_ELEC_NAME, roomElectricityMeasureIri, roomElectricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
    }

    @Test
    void testGetAllMeasuresForFirstAsset() {
        // Set up IRIs
        String electricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String electricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation(ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, electricityMeasureIri, electricityTimeSeriesIri);
        // Execute method
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        // Test results
        assertEquals(1, results.size()); // Only one asset expected
        for (String assetKey : results.keySet()) {
            assertEquals(ASSET_LAMP_ONE_NAME, assetKey); // Asset key should be asset name
            Queue<String[]> assetMetadataResult = results.get(assetKey);
            assertEquals(1, assetMetadataResult.size()); // Only one measure expected
            // Verifies its contents
            String[] metadata = assetMetadataResult.poll();
            AssetTest.verifyAssetMeasureArrayContents(ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, electricityMeasureIri, electricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
        }
    }

    @Test
    void testGetAllMeasuresForFirstRoom() {
        // Set up IRIs
        String electricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String electricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation(ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, electricityMeasureIri, electricityTimeSeriesIri);
        // Execute method
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        // Test results
        assertEquals(1, results.size()); // Only one room expected
        for (String assetKey : results.keySet()) {
            assertEquals(ROOM_ONE_NAME, assetKey); // Room key should be room name
            Queue<String[]> assetMetadataResult = results.get(assetKey);
            assertEquals(1, assetMetadataResult.size()); // Only one measure expected
            // Verifies its contents
            String[] metadata = assetMetadataResult.poll();
            RoomTest.verifyRoomMeasureArrayContents(MEASURE_ELEC_NAME, electricityMeasureIri, electricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
        }
    }

    @Test
    void testGetAllMeasuresNoThreshold() {
        // Set up IRIs
        String roomElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String roomElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation(ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, roomElectricityMeasureIri, roomElectricityTimeSeriesIri);
        // Execute method
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        // Retrieve and test results
        assertEquals(1, results.size()); // Do not expect threshold to be available
        assertFalse(results.containsKey(StringHelper.THRESHOLD_KEY));
    }

    @Test
    void testAddAssetForSecondAsset() {
        // Set up IRIs
        String lampOneElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String lampOneElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String lampTwoElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String lampTwoElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation(ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, lampOneElectricityMeasureIri, lampOneElectricityTimeSeriesIri);
        // Execute method
        sample.addAsset(ASSET_LAMP_TWO_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, lampTwoElectricityMeasureIri, lampTwoElectricityTimeSeriesIri);
        // Retrieve and test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(2, results.size()); // Two assets expected
        // Retrieve and verify contents of first asset
        Queue<String[]> assetMetadataResult = results.get(ASSET_LAMP_ONE_NAME);
        assertEquals(1, assetMetadataResult.size()); // Only one measure expected
        String[] metadata = assetMetadataResult.poll();
        AssetTest.verifyAssetMeasureArrayContents(ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, lampOneElectricityMeasureIri, lampOneElectricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
        // Retrieve and verify contents of second asset
        assetMetadataResult = results.get(ASSET_LAMP_TWO_NAME);
        assertEquals(1, assetMetadataResult.size()); // Only one measure expected
        metadata = assetMetadataResult.poll();
        AssetTest.verifyAssetMeasureArrayContents(ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, lampTwoElectricityMeasureIri, lampTwoElectricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
    }

    @Test
    void testAddAssetForSecondMeasureOfSameAsset() {
        // Set up IRIs
        String lampElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String lampElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String lampStateMeasureIri = TestUtils.genInstance(MEASURE_STATE_CONCEPT);
        String lampStateTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation(ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, lampElectricityMeasureIri, lampElectricityTimeSeriesIri);
        // Execute method
        sample.addAsset(ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_STATE_NAME, MEASURE_STATE_UNIT, lampStateMeasureIri, lampStateTimeSeriesIri);
        // Retrieve and test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(1, results.size()); // Only one asset expected
        Queue<String[]> assetMetadataResult = results.get(ASSET_LAMP_ONE_NAME);
        assertEquals(2, assetMetadataResult.size()); // Two measures expected
        // Verifies its contents
        String[] metadata = assetMetadataResult.poll();
        AssetTest.verifyAssetMeasureArrayContents(ASSET_LAMP_TYPE, MEASURE_STATE_NAME, lampStateMeasureIri, lampStateTimeSeriesIri, MEASURE_STATE_UNIT, metadata);
        metadata = assetMetadataResult.poll();
        AssetTest.verifyAssetMeasureArrayContents(ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, lampElectricityMeasureIri, lampElectricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
    }

    @Test
    void testAddAssetForComplexMixOfMeasureAndAsset() {
        // Set up IRIs
        String lampOneElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String lampOneElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String lampTwoElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String lampTwoElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String fridgeElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String fridgeElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String fridgeStateMeasureIri = TestUtils.genInstance(MEASURE_STATE_CONCEPT);
        String fridgeStateTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation(ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, lampOneElectricityMeasureIri, lampOneElectricityTimeSeriesIri);
        // Execute method
        sample.addAsset(ASSET_LAMP_TWO_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, lampTwoElectricityMeasureIri, lampTwoElectricityTimeSeriesIri);
        sample.addAsset(ASSET_FRIDGE_ONE_NAME, ASSET_FRIDGE_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, fridgeElectricityMeasureIri, fridgeElectricityTimeSeriesIri);
        sample.addAsset(ASSET_FRIDGE_ONE_NAME, ASSET_FRIDGE_TYPE, MEASURE_STATE_NAME, MEASURE_STATE_UNIT, fridgeStateMeasureIri, fridgeStateTimeSeriesIri);
        // Retrieve and test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(3, results.size()); // Three assets expected
        // Retrieve and verify contents of first asset
        Queue<String[]> assetMetadataResult = results.get(ASSET_LAMP_ONE_NAME);
        assertEquals(1, assetMetadataResult.size()); // Only one measure expected
        String[] metadata = assetMetadataResult.poll();
        AssetTest.verifyAssetMeasureArrayContents(ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, lampOneElectricityMeasureIri, lampOneElectricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
        // Retrieve and verify contents of second asset
        assetMetadataResult = results.get(ASSET_LAMP_TWO_NAME);
        assertEquals(1, assetMetadataResult.size()); // Only one measure expected
        metadata = assetMetadataResult.poll();
        AssetTest.verifyAssetMeasureArrayContents(ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, lampTwoElectricityMeasureIri, lampTwoElectricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
        // Retrieve and verify contents of third asset
        assetMetadataResult = results.get(ASSET_FRIDGE_ONE_NAME);
        assertEquals(2, assetMetadataResult.size()); // Two measures expected
        metadata = assetMetadataResult.poll();
        AssetTest.verifyAssetMeasureArrayContents(ASSET_FRIDGE_TYPE, MEASURE_STATE_NAME, fridgeStateMeasureIri, fridgeStateTimeSeriesIri, MEASURE_STATE_UNIT, metadata);
        metadata = assetMetadataResult.poll();
        AssetTest.verifyAssetMeasureArrayContents(ASSET_FRIDGE_TYPE, MEASURE_ELEC_NAME, fridgeElectricityMeasureIri, fridgeElectricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
    }

    @Test
    void testAddRoomForSecondRoom() {
        // Set up IRIs
        String electricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String electricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String roomStateMeasureIri = TestUtils.genInstance(MEASURE_STATE_CONCEPT);
        String roomStateTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation(ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, electricityMeasureIri, electricityTimeSeriesIri);
        // Execute method
        sample.addRoom(ROOM_TWO_NAME, MEASURE_STATE_NAME, MEASURE_STATE_UNIT, roomStateMeasureIri, roomStateTimeSeriesIri);
        // Test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(2, results.size()); // Two rooms expected
        // Retrieve and verify contents of first room
        Queue<String[]> roomMetadataResult = results.get(ROOM_ONE_NAME);
        assertEquals(1, roomMetadataResult.size()); // Only one measure expected
        String[] metadata = roomMetadataResult.poll();
        RoomTest.verifyRoomMeasureArrayContents(MEASURE_ELEC_NAME, electricityMeasureIri, electricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
        // Retrieve and verify contents of second room
        roomMetadataResult = results.get(ROOM_TWO_NAME);
        assertEquals(1, roomMetadataResult.size()); // Only one measure expected
        metadata = roomMetadataResult.poll();
        RoomTest.verifyRoomMeasureArrayContents(MEASURE_STATE_NAME, roomStateMeasureIri, roomStateTimeSeriesIri, MEASURE_STATE_UNIT, metadata);
    }

    @Test
    void testAddRoomForSecondMeasureOfSameRoom() {
        // Set up IRIs
        String roomElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String roomElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String roomStateMeasureIri = TestUtils.genInstance(MEASURE_STATE_CONCEPT);
        String roomStateTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation(ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, roomElectricityMeasureIri, roomElectricityTimeSeriesIri);
        // Execute method
        sample.addRoom(ROOM_ONE_NAME, MEASURE_STATE_NAME, MEASURE_STATE_UNIT, roomStateMeasureIri, roomStateTimeSeriesIri);
        // Retrieve and test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(1, results.size()); // Only one room expected
        Queue<String[]> roomMetadataResult = results.get(ROOM_ONE_NAME);
        assertEquals(2, roomMetadataResult.size()); // Two measures expected
        // Verifies its contents
        String[] metadata = roomMetadataResult.poll();
        RoomTest.verifyRoomMeasureArrayContents(MEASURE_STATE_NAME, roomStateMeasureIri, roomStateTimeSeriesIri, MEASURE_STATE_UNIT, metadata);
        metadata = roomMetadataResult.poll();
        RoomTest.verifyRoomMeasureArrayContents(MEASURE_ELEC_NAME, roomElectricityMeasureIri, roomElectricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
    }

    @Test
    void testAddThresholds() {
        // Set up IRIs
        String roomElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String roomElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation(ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, roomElectricityMeasureIri, roomElectricityTimeSeriesIri);
        // Execute method
        sample.addThresholds(THRESHOLD_NAME, THRESHOLD_MIN, THRESHOLD_MAX);
        // Retrieve and test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(2, results.size()); // Expect two results - one for room, and one for threshold
        assertTrue(results.containsKey(StringHelper.THRESHOLD_KEY));
        Queue<String[]> thresholdMetadataResult = results.get(StringHelper.THRESHOLD_KEY);
        assertEquals(1, thresholdMetadataResult.size()); // One threshold expected
        String[] metadata = thresholdMetadataResult.poll();
        assertEquals(THRESHOLD_NAME, metadata[0]);
        assertEquals(THRESHOLD_MIN, metadata[1]);
        assertEquals(THRESHOLD_MAX, metadata[2]);
    }

    @Test
    void testAddThresholds_NoDuplicate() {
        // Set up IRIs
        String roomElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String roomElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation(ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, roomElectricityMeasureIri, roomElectricityTimeSeriesIri);
        // Execute method twice
        sample.addThresholds(THRESHOLD_NAME, THRESHOLD_MIN, THRESHOLD_MAX);
        sample.addThresholds(THRESHOLD_NAME, THRESHOLD_MIN, THRESHOLD_MAX);
        // Retrieve and test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(2, results.size()); // Expect two results - one for room, and one for threshold
        assertTrue(results.containsKey(StringHelper.THRESHOLD_KEY));
        Queue<String[]> thresholdMetadataResult = results.get(StringHelper.THRESHOLD_KEY);
        assertEquals(1, thresholdMetadataResult.size()); // One threshold expected due to duplication
        String[] metadata = thresholdMetadataResult.poll();
        assertEquals(THRESHOLD_NAME, metadata[0]);
        assertEquals(THRESHOLD_MIN, metadata[1]);
        assertEquals(THRESHOLD_MAX, metadata[2]);
    }
}