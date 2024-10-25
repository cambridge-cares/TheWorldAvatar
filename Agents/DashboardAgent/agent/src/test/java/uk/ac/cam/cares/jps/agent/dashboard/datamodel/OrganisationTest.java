package uk.ac.cam.cares.jps.agent.dashboard.datamodel;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class OrganisationTest {
    private static final String ORGANISATION_NAME = "Example Firm";
    private static final String FACILITY_NAME = "Home";
    private static final String ASSET_LAMP_TYPE = "Lamp";
    private static final String ASSET_LAMP_ONE_NAME = "Lamp1";
    private static final String ASSET_LAMP_ONE_DATA_IRI = TestUtils.genInstance("measure");
    private static final String ASSET_LAMP_ONE_TS_IRI = TestUtils.genTimeSeriesInstance();
    private static final String ASSET_LAMP_TWO_NAME = "Lamp2";
    private static final String ASSET_LAMP_TWO_DATA_IRI = TestUtils.genInstance("measure");
    private static final String ASSET_LAMP_TWO_TS_IRI = TestUtils.genTimeSeriesInstance();
    private static final String ROOM_ONE_NAME = "Bedroom";
    private static final String ROOM_ONE_DATA_IRI = TestUtils.genInstance("measure");
    private static final String ROOM_ONE_TS_IRI = TestUtils.genTimeSeriesInstance();
    private static final String SYSTEM_ONE_NAME = "HVAC";
    private static final String SYSTEM_ONE_DATA_IRI = TestUtils.genInstance("measure");
    private static final String SYSTEM_ONE_TS_IRI = TestUtils.genTimeSeriesInstance();
    private static final String MEASURE_ELEC_NAME = "Electricity Consumption";
    private static final String MEASURE_ELEC_UNIT = "kWh";
    private static final String MEASURE_HEAT_NAME = "Heat";
    private static final String MEASURE_HEAT_UNIT = null;
    private static final String THRESHOLD_MIN = "1";
    private static final String THRESHOLD_MAX = "5";

    @Test
    void testGetName() {
        // Initialise object
        Organisation sample = new Organisation(ORGANISATION_NAME);
        // Execute method and verify results
        assertEquals(ORGANISATION_NAME, sample.getName());
    }

    @Test
    void testAddFacilityItem_and_GetFacilities_SingleFacility() {
        // Initialise object
        Organisation sample = new Organisation(ORGANISATION_NAME);
        // Execute methods
        sample.addFacilityItem(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ASSET_LAMP_ONE_DATA_IRI, ASSET_LAMP_ONE_TS_IRI);
        Queue<String> facilities = sample.getFacilities();
        // Execute method and verify results
        assertEquals(1, facilities.size());
        assertEquals(FACILITY_NAME, facilities.poll());
    }

    @Test
    void testAddFacilityItem_and_GetFacilities_DuplicateFacility() {
        // Initialise object
        Organisation sample = new Organisation(ORGANISATION_NAME);
        // Execute methods
        sample.addFacilityItem(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ASSET_LAMP_ONE_DATA_IRI, ASSET_LAMP_ONE_TS_IRI);
        sample.addFacilityItem(FACILITY_NAME, ROOM_ONE_NAME, StringHelper.ROOM_KEY, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ROOM_ONE_DATA_IRI, ROOM_ONE_TS_IRI);
        Queue<String> facilities = sample.getFacilities();
        // Execute method and verify results
        assertEquals(1, facilities.size()); // Duplicates should still only have one facility
        assertEquals(FACILITY_NAME, facilities.poll());
    }

    @Test
    void testAddFacilityItem_and_GetFacilities_MultipleFacilities() {
        // Initialise object
        String facilityTwo = "Test Facility";
        Organisation sample = new Organisation(ORGANISATION_NAME);
        // Execute methods
        sample.addFacilityItem(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ASSET_LAMP_ONE_DATA_IRI, ASSET_LAMP_ONE_TS_IRI);
        sample.addFacilityItem(facilityTwo, ROOM_ONE_NAME, StringHelper.ROOM_KEY, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ROOM_ONE_DATA_IRI, ROOM_ONE_TS_IRI);
        Queue<String> facilities = sample.getFacilities();
        // Execute method and verify results
        assertEquals(2, facilities.size());
        while (!facilities.isEmpty()) {
            String facility = facilities.poll();
            assertTrue(Objects.equals(FACILITY_NAME, facility) || Objects.equals(facilityTwo, facility));
        }
    }

    @Test
    void testGetAllItemGroups_MultipleItemsOfDifferentGroups() {
        // Initialise object
        Organisation sample = new Organisation(ORGANISATION_NAME);
        sample.addFacilityItem(FACILITY_NAME, SYSTEM_ONE_NAME, StringHelper.SYSTEM_KEY, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, SYSTEM_ONE_DATA_IRI, SYSTEM_ONE_TS_IRI);
        sample.addFacilityItem(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ASSET_LAMP_ONE_DATA_IRI, ASSET_LAMP_ONE_TS_IRI);
        sample.addFacilityItem(FACILITY_NAME, ROOM_ONE_NAME, StringHelper.ROOM_KEY, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ROOM_ONE_DATA_IRI, ROOM_ONE_TS_IRI);
        // Execute methods
        List<String> itemGroups = sample.getAllItemGroups();
        // Verify results
        assertEquals(3, itemGroups.size());
        assertEquals(StringHelper.ROOM_KEY, itemGroups.get(0)); // Room key will always be first
        assertEquals(StringHelper.SYSTEM_KEY, itemGroups.get(1)); // System key will always be second
        assertEquals(ASSET_LAMP_TYPE, itemGroups.get(2));
    }

    @Test
    void testGetAllItemGroups_MultipleItemsOfSameGroup() {
        // Initialise object
        Organisation sample = new Organisation(ORGANISATION_NAME);
        sample.addFacilityItem(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ASSET_LAMP_ONE_DATA_IRI, ASSET_LAMP_ONE_TS_IRI);
        sample.addFacilityItem(FACILITY_NAME, ASSET_LAMP_TWO_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ASSET_LAMP_ONE_DATA_IRI, ASSET_LAMP_ONE_TS_IRI);
        // Execute methods
        List<String> itemGroups = sample.getAllItemGroups();
        // Verify results
        assertEquals(1, itemGroups.size());
        assertEquals(ASSET_LAMP_TYPE, itemGroups.get(0));
    }

    @Test
    void testGetFacilityItemInventory_SingleFacility() {
        // Initialise object
        Organisation sample = new Organisation(ORGANISATION_NAME);
        sample.addFacilityItem(FACILITY_NAME, ROOM_ONE_NAME, StringHelper.ROOM_KEY, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ROOM_ONE_DATA_IRI, ROOM_ONE_TS_IRI);
        // Execute methods
        Queue<String[]> itemInventory = sample.getFacilityItemInventory(StringHelper.ROOM_KEY);
        // Verify results
        assertEquals(1, itemInventory.size());
        while (!itemInventory.isEmpty()) {
            String[] currentItem = itemInventory.poll();
            assertEquals(ROOM_ONE_NAME, currentItem[0]);
            assertEquals(FACILITY_NAME, currentItem[1]);
        }
    }

    @Test
    void testGetFacilityItemInventory_MultipleFacilities() {
        // Initialise object
        String facilityTwo = "Test Facility";
        Organisation sample = new Organisation(ORGANISATION_NAME);
        sample.addFacilityItem(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ASSET_LAMP_ONE_DATA_IRI, ASSET_LAMP_ONE_TS_IRI);
        sample.addFacilityItem(facilityTwo, ASSET_LAMP_TWO_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ASSET_LAMP_ONE_DATA_IRI, ASSET_LAMP_ONE_TS_IRI);
        // Execute methods
        Queue<String[]> itemInventory = sample.getFacilityItemInventory(ASSET_LAMP_TYPE);
        // Verify results
        assertEquals(2, itemInventory.size());
        while (!itemInventory.isEmpty()) {
            String[] currentItem = itemInventory.poll();
            assertTrue(Objects.equals(ASSET_LAMP_ONE_NAME, currentItem[0]) || Objects.equals(ASSET_LAMP_TWO_NAME, currentItem[0]));
            assertTrue(Objects.equals(FACILITY_NAME, currentItem[1]) || Objects.equals(facilityTwo, currentItem[1]));
        }
    }

    @Test
    void testGetAllMeasureNames_and_GetMeasure() {
        // Initialise object
        Organisation sample = new Organisation(ORGANISATION_NAME);
        sample.addFacilityItem(FACILITY_NAME, ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ASSET_LAMP_ONE_DATA_IRI, ASSET_LAMP_ONE_TS_IRI);
        sample.addFacilityItem(FACILITY_NAME, ASSET_LAMP_TWO_NAME, ASSET_LAMP_TYPE, MEASURE_HEAT_NAME, MEASURE_HEAT_UNIT, ASSET_LAMP_TWO_DATA_IRI, ASSET_LAMP_TWO_TS_IRI);
        // Execute methods
        verifyOrganisationModel(sample, genExpectedOrgOutputs(false,
                new String[]{ASSET_LAMP_ONE_NAME, ASSET_LAMP_TYPE, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ASSET_LAMP_ONE_DATA_IRI, ASSET_LAMP_ONE_TS_IRI},
                new String[]{ASSET_LAMP_TWO_NAME, ASSET_LAMP_TYPE, MEASURE_HEAT_NAME, MEASURE_HEAT_UNIT, ASSET_LAMP_TWO_DATA_IRI, ASSET_LAMP_TWO_TS_IRI}
        ));
    }

    @Test
    void testAddThresholds_and_GetThresholds() {
        // Initialise object
        Organisation sample = new Organisation(ORGANISATION_NAME);
        sample.addFacilityItem(FACILITY_NAME, ROOM_ONE_NAME, StringHelper.ROOM_KEY, MEASURE_ELEC_NAME, MEASURE_ELEC_UNIT, ROOM_ONE_DATA_IRI, ROOM_ONE_TS_IRI);
        // Execute methods
        sample.addThresholds(FACILITY_NAME, StringHelper.ROOM_KEY, MEASURE_ELEC_NAME, THRESHOLD_MIN, THRESHOLD_MAX);
        Queue<Threshold> thresholds = sample.getThresholds(StringHelper.ROOM_KEY);
        // Verify results
        assertEquals(1, thresholds.size());
        ThresholdTest.verifyThresholds(thresholds.poll(), ThresholdTest.genExpectedThresholds(new String[]{MEASURE_ELEC_NAME, THRESHOLD_MIN, THRESHOLD_MAX}));
    }

    public static Map<String, Map<String, Queue<String[]>>> genExpectedOrgOutputs(boolean reqTSMetadata, String[]... itemMetadata) {
        Map<String, Map<String, Queue<String[]>>> itemMappings = new HashMap<>();
        for (String[] item : itemMetadata) {
            String itemName = item[0];
            String itemGroup = item[1];
            String measureName = item[2];
            String unit = item[3];
            String dataIri = item[4];
            String timeSeriesIri = item[5];
            // Time series metadata defaults to empty strings if not required
            String col = "";
            String table = "";
            String database = "";
            if (reqTSMetadata) {
                col = item[6];
                table = item[7];
                database = item[8];
            }
            // Organisation model follows the following format:
            // -itemGroup1:
            //  --measure1: DataIri, TimeSeriesIri, Col, Table, Database
            //  --measure2: DataIri, TimeSeriesIri, Col, Table, Database
            itemMappings.computeIfAbsent(itemGroup, k -> new HashMap<>())
                    .computeIfAbsent(measureName, k -> new ArrayDeque<>())
                    .offer(new String[]{itemName, unit, dataIri, timeSeriesIri, col, table, database});
        }
        return itemMappings;
    }

    public static void verifyOrganisationModel(Organisation result, Map<String, Map<String, Queue<String[]>>> expectedOrganisationModel) {
        verifyOrganisationModel(result, expectedOrganisationModel, new HashMap<>());
    }

    public static void verifyOrganisationModel(Organisation result, Map<String, Map<String, Queue<String[]>>> expectedOrganisationModel, Map<String, String[]> expectedThresholdMappings) {
        // Execute methods
        expectedOrganisationModel.keySet().forEach(group -> {
            Set<String> measures = result.getAllMeasureNames(group);
            Map<String, Queue<String[]>> expectedMeasureItemMappings = expectedOrganisationModel.get(group);
            Queue<Threshold> thresholdResults = result.getThresholds(group);
            // Verify the number of measure for this item group is the same
            assertEquals(expectedMeasureItemMappings.keySet().size(), measures.size());
            measures.forEach(measure -> {
                if (!thresholdResults.isEmpty()) {
                    ThresholdTest.verifyThresholds(thresholdResults.poll(), expectedThresholdMappings);
                }
                Measure currentMeasure = result.getMeasure(group, measure);
                Queue<String[]> expectedItemMetadataQueue = expectedMeasureItemMappings.get(measure);
                Queue<String[]> expectedItemIriTestMeasureQueue = new ArrayDeque<>();
                Queue<String[]> expectedItemColTestMeasureQueue = new ArrayDeque<>();
                while (!expectedItemMetadataQueue.isEmpty()) {
                    String[] expectedItemMetadata = expectedItemMetadataQueue.poll();
                    assertEquals(expectedItemMetadata[1], currentMeasure.getUnit()); // Unit
                    String item = expectedItemMetadata[0];
                    String dataIri = expectedItemMetadata[2];
                    String timeSeriesIri = expectedItemMetadata[3];
                    expectedItemIriTestMeasureQueue.offer(new String[]{item, dataIri, timeSeriesIri});
                    if (!expectedItemMetadata[4].isEmpty()) {
                        String column = expectedItemMetadata[4];
                        assertEquals(expectedItemMetadata[5], currentMeasure.getTimeSeriesTable());  // Table
                        assertEquals(expectedItemMetadata[6], currentMeasure.getTimeSeriesDatabase());  // Database
                        expectedItemColTestMeasureQueue.offer(new String[]{item, column});
                    }
                }
                MeasureTest.verifyTimeSeriesIriResult(currentMeasure.getAllTimeSeriesIris(), MeasureTest.genExpectedItemAndIriMappings(expectedItemIriTestMeasureQueue));
                if (!expectedItemColTestMeasureQueue.isEmpty()) {
                    MeasureTest.verifyTimeSeriesMetadataResult(currentMeasure.getTimeSeriesData(), MeasureTest.genExpectedItemAndColumnMappings(expectedItemColTestMeasureQueue));
                }
            });
        });
    }
}