package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.datamodel;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class OrganisationTest {
    private static final String FACILITY_NAME = "Home";
    private static final String ASSET_LAMP_ONE_NAME = "Lamp1";
    private static final String ASSET_LAMP_TWO_NAME = "Lamp2";
    private static final String ASSET_LAMP_TYPE = "Lamp";
    private static final String ASSET_FRIDGE_ONE_NAME = "F1";
    private static final String ASSET_FRIDGE_TYPE = "Fridge";
    private static final String ROOM_ONE_NAME = "Bedroom";
    private static final String ROOM_TWO_NAME = "Kitchen";
    private static final String SYSTEM_ONE_NAME = "HVAC";
    private static final String SYSTEM_TWO_NAME = "Emergency (AREA)";
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
    void testGetAllMeasure_AssetAndRoom() {
        // Set up IRIs
        String assetElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String assetElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String roomElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String roomElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation();
        sample.addAsset(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, assetElectricityMeasureIri, assetElectricityTimeSeriesIri);
        sample.addRoom(FACILITY_NAME, ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, roomElectricityMeasureIri, roomElectricityTimeSeriesIri);
        // Execute method
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        // Test results
        assertEquals(3, results.size()); // Expect only one asset and room, and their facility
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
        verifyFacilityQueueResults(results, new String[]{FACILITY_NAME}, new String[]{FACILITY_NAME, ASSET_LAMP_ONE_NAME, ROOM_ONE_NAME});
    }

    @Test
    void testGetAllMeasures_FirstAssetOnly() {
        // Set up IRIs
        String electricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String electricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation();
        sample.addAsset(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, electricityMeasureIri, electricityTimeSeriesIri);
        // Execute method
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        // Test results
        assertEquals(2, results.size()); // Only one asset and its facility expected
        verifyFacilityQueueResults(results, new String[]{FACILITY_NAME}, new String[]{FACILITY_NAME, ASSET_LAMP_ONE_NAME});
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
    void testGetAllMeasures_FirstRoomOnly() {
        // Set up IRIs
        String electricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String electricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation();
        sample.addRoom(FACILITY_NAME, ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, electricityMeasureIri, electricityTimeSeriesIri);
        // Execute method
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        // Test results
        assertEquals(2, results.size()); // Only one room and its facility expected
        verifyFacilityQueueResults(results, new String[]{FACILITY_NAME}, new String[]{FACILITY_NAME, ROOM_ONE_NAME});
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
    void testGetAllMeasures_FirstSystemOnly() {
        // Set up IRIs
        String electricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String electricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation();
        sample.addSystem(FACILITY_NAME, SYSTEM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, electricityMeasureIri, electricityTimeSeriesIri);
        // Execute method
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        // Test results
        assertEquals(2, results.size()); // Only one system and its facility expected
        verifyFacilityQueueResults(results, new String[]{FACILITY_NAME}, new String[]{FACILITY_NAME, SYSTEM_ONE_NAME});
        for (String system : results.keySet()) {
            assertEquals(SYSTEM_ONE_NAME, system); // Room key should be room name
            Queue<String[]> systemMetadataResult = results.get(system);
            assertEquals(1, systemMetadataResult.size()); // Only one measure expected
            // Verifies its contents
            String[] metadata = systemMetadataResult.poll();
            TechnicalSystemTest.verifySystemMeasureArrayContents(MEASURE_ELEC_NAME, electricityMeasureIri, electricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
        }
    }

    @Test
    void testGetAllMeasures_NoThresholdForRoom() {
        // Set up IRIs
        String roomElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String roomElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation();
        sample.addRoom(FACILITY_NAME, ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, roomElectricityMeasureIri, roomElectricityTimeSeriesIri);
        // Execute method
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        // Retrieve and test results
        assertEquals(2, results.size()); // Do not expect threshold to be available
        assertFalse(results.containsKey(StringHelper.THRESHOLD_KEY));
    }

    @Test
    void testAddAsset_AddSecondAsset() {
        // Set up IRIs
        String lampOneElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String lampOneElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String lampTwoElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String lampTwoElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation();
        // Execute method
        sample.addAsset(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, lampOneElectricityMeasureIri, lampOneElectricityTimeSeriesIri);
        sample.addAsset(FACILITY_NAME, ASSET_LAMP_TWO_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, lampTwoElectricityMeasureIri, lampTwoElectricityTimeSeriesIri);
        // Retrieve and test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(3, results.size()); // Two assets and their facility expected
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
    void testAddAsset_SecondMeasureOfSameAsset() {
        // Set up IRIs
        String lampElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String lampElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String lampStateMeasureIri = TestUtils.genInstance(MEASURE_STATE_CONCEPT);
        String lampStateTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation();
        // Execute method
        sample.addAsset(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, lampElectricityMeasureIri, lampElectricityTimeSeriesIri);
        sample.addAsset(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_STATE_NAME, MEASURE_STATE_UNIT, lampStateMeasureIri, lampStateTimeSeriesIri);
        // Retrieve and test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(2, results.size()); // Only one asset and its facility  expected
        Queue<String[]> assetMetadataResult = results.get(ASSET_LAMP_ONE_NAME);
        assertEquals(2, assetMetadataResult.size()); // Two measures expected
        // Verifies its contents
        String[] metadata = assetMetadataResult.poll();
        AssetTest.verifyAssetMeasureArrayContents(ASSET_LAMP_TYPE, MEASURE_STATE_NAME, lampStateMeasureIri, lampStateTimeSeriesIri, MEASURE_STATE_UNIT, metadata);
        metadata = assetMetadataResult.poll();
        AssetTest.verifyAssetMeasureArrayContents(ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, lampElectricityMeasureIri, lampElectricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
    }

    @Test
    void testAddAsset_ComplexMixOfMeasureAndAsset() {
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
        Organisation sample = new Organisation();
        // Execute method
        sample.addAsset(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, lampOneElectricityMeasureIri, lampOneElectricityTimeSeriesIri);
        sample.addAsset(FACILITY_NAME, ASSET_LAMP_TWO_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, lampTwoElectricityMeasureIri, lampTwoElectricityTimeSeriesIri);
        sample.addAsset(FACILITY_NAME, ASSET_FRIDGE_ONE_NAME, ASSET_FRIDGE_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, fridgeElectricityMeasureIri, fridgeElectricityTimeSeriesIri);
        sample.addAsset(FACILITY_NAME, ASSET_FRIDGE_ONE_NAME, ASSET_FRIDGE_TYPE, MEASURE_STATE_NAME, MEASURE_STATE_UNIT, fridgeStateMeasureIri, fridgeStateTimeSeriesIri);
        // Retrieve and test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(4, results.size()); // Three assets and their facility expected
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
    void testAddRoom_SecondRoom() {
        // Set up IRIs
        String electricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String electricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String roomStateMeasureIri = TestUtils.genInstance(MEASURE_STATE_CONCEPT);
        String roomStateTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation();
        // Execute method
        sample.addRoom(FACILITY_NAME, ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, electricityMeasureIri, electricityTimeSeriesIri);
        sample.addRoom(FACILITY_NAME, ROOM_TWO_NAME, MEASURE_STATE_NAME, MEASURE_STATE_UNIT, roomStateMeasureIri, roomStateTimeSeriesIri);
        // Test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(3, results.size()); // Two rooms and their facility expected
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
    void testAddRoom_SecondMeasureOfSameRoom() {
        // Set up IRIs
        String roomElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String roomElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String roomStateMeasureIri = TestUtils.genInstance(MEASURE_STATE_CONCEPT);
        String roomStateTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation();
        // Execute method
        sample.addRoom(FACILITY_NAME, ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, roomElectricityMeasureIri, roomElectricityTimeSeriesIri);
        sample.addRoom(FACILITY_NAME, ROOM_ONE_NAME, MEASURE_STATE_NAME, MEASURE_STATE_UNIT, roomStateMeasureIri, roomStateTimeSeriesIri);
        // Retrieve and test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(2, results.size()); // Only one room and its facility expected
        Queue<String[]> roomMetadataResult = results.get(ROOM_ONE_NAME);
        assertEquals(2, roomMetadataResult.size()); // Two measures expected
        // Verifies its contents
        String[] metadata = roomMetadataResult.poll();
        RoomTest.verifyRoomMeasureArrayContents(MEASURE_STATE_NAME, roomStateMeasureIri, roomStateTimeSeriesIri, MEASURE_STATE_UNIT, metadata);
        metadata = roomMetadataResult.poll();
        RoomTest.verifyRoomMeasureArrayContents(MEASURE_ELEC_NAME, roomElectricityMeasureIri, roomElectricityTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
    }

    @Test
    void testAddSystem_SecondSystem() {
        // Set up IRIs
        String systemOneElecMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String systemOneElecTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        String systemTwoElecMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String systemTwoElecTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation();
        // Execute method
        sample.addSystem(FACILITY_NAME, SYSTEM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, systemOneElecMeasureIri, systemOneElecTimeSeriesIri);
        sample.addSystem(FACILITY_NAME, SYSTEM_TWO_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, systemTwoElecMeasureIri, systemTwoElecTimeSeriesIri);
        // Test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(3, results.size()); // Two systems and their facility expected
        // Retrieve and verify contents of first room
        Queue<String[]> systemMetadataResult = results.get(SYSTEM_ONE_NAME);
        assertEquals(1, systemMetadataResult.size()); // Only one measure expected
        String[] metadata = systemMetadataResult.poll();
        TechnicalSystemTest.verifySystemMeasureArrayContents(MEASURE_ELEC_NAME, systemOneElecMeasureIri, systemOneElecTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
        // Retrieve and verify contents of second room
        systemMetadataResult = results.get(SYSTEM_TWO_NAME);
        assertEquals(1, systemMetadataResult.size()); // Only one measure expected
        metadata = systemMetadataResult.poll();
        TechnicalSystemTest.verifySystemMeasureArrayContents(MEASURE_ELEC_NAME, systemTwoElecMeasureIri, systemTwoElecTimeSeriesIri, MEASURE_ELEC_UNIT, metadata);
    }

    @Test
    void testAddThresholds() {
        // Set up IRIs
        String roomElectricityMeasureIri = TestUtils.genInstance(MEASURE_ELEC_CONCEPT);
        String roomElectricityTimeSeriesIri = TestUtils.genTimeSeriesInstance();
        // Initialise object
        Organisation sample = new Organisation();
        // Execute method
        sample.addRoom(FACILITY_NAME, ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, roomElectricityMeasureIri, roomElectricityTimeSeriesIri);
        sample.addThresholds(THRESHOLD_NAME, THRESHOLD_MIN, THRESHOLD_MAX);
        // Retrieve and test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(3, results.size()); // Expect three results - one for room, one for facility, and one for threshold
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
        Organisation sample = new Organisation();
        // Execute method twice
        sample.addRoom(FACILITY_NAME, ROOM_ONE_NAME, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, roomElectricityMeasureIri, roomElectricityTimeSeriesIri);
        sample.addThresholds(THRESHOLD_NAME, THRESHOLD_MIN, THRESHOLD_MAX);
        sample.addThresholds(THRESHOLD_NAME, THRESHOLD_MIN, THRESHOLD_MAX);
        // Retrieve and test results
        Map<String, Queue<String[]>> results = sample.getAllMeasures();
        assertEquals(3, results.size()); // Expect two results - one for room, one for facility, and one for threshold
        assertTrue(results.containsKey(StringHelper.THRESHOLD_KEY));
        Queue<String[]> thresholdMetadataResult = results.get(StringHelper.THRESHOLD_KEY);
        assertEquals(1, thresholdMetadataResult.size()); // One threshold expected due to duplication
        String[] metadata = thresholdMetadataResult.poll();
        assertEquals(THRESHOLD_NAME, metadata[0]);
        assertEquals(THRESHOLD_MIN, metadata[1]);
        assertEquals(THRESHOLD_MAX, metadata[2]);
    }

    public static void verifyFacilityQueueResults(Map<String, Queue<String[]>> results, String[] expectedFacilityNames, String[]... expectedItemNames) {
        // Map the expected item names to their facility
        Map<String, String[]> facilityItemMapping = new HashMap<>();
        // Ensure that expectedItemNames has a facility name at the first position
        for (String[] expectedGroupData : expectedItemNames) {
            facilityItemMapping.put(expectedGroupData[0], expectedGroupData);
        }
        // Retrieve the facility item
        Queue<String[]> facilities = results.get(StringHelper.FACILITY_KEY);
        // Verify that the number of facilities correspond to the facility queue
        assertEquals(expectedFacilityNames.length, facilities.size());
        List<String> expectedFacilityNameList = Arrays.asList(expectedFacilityNames);
        // For each facility, verify its results
        while (!facilities.isEmpty()) {
            // Retrieve the only variable
            String[] facilityMetadata = facilities.poll();
            assertTrue(expectedFacilityNameList.contains(facilityMetadata[0])); // Ensure first metadata is a facility name
            List<String> expectedItems = Arrays.asList(facilityItemMapping.get(facilityMetadata[0]));
            // Ensure that the length includes all the items plus one for the facility name itself
            assertEquals(expectedItems.size(), facilityMetadata.length);
            // Verify that the corresponding metadata follows
            for (int i = 1; i < expectedItemNames.length; i++) {
                assertTrue(expectedItems.contains(facilityMetadata[i]));
            }
        }
        results.remove(StringHelper.FACILITY_KEY);
    }
}