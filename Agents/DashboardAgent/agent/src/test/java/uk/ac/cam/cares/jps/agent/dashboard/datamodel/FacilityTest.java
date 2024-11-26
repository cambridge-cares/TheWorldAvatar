package uk.ac.cam.cares.jps.agent.dashboard.datamodel;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class FacilityTest {
    private static final String FACILITY_NAME = "Apartment unit";
    private static final String ROOM_ONE_NAME = "kitchen";
    private static final String ROOM_TWO_NAME = "Bedroom";
    private static final String SYSTEM_ONE_NAME = "Emergency";
    private static final String ELECTRICITY_MEASURE = "electricity";
    private static final String ELECTRICITY_MIN_THRESHOLD = "5";
    private static final String ELECTRICITY_MAX_THRESHOLD = "100";

    @Test
    void testGetName() {
        // Initialise object
        Facility sample = new Facility(FACILITY_NAME);
        // Execute and test method
        assertEquals(FACILITY_NAME, sample.getName());
    }

    @Test
    void testAddItem_and_GetItemNames_MultipleItemsOfSameGroup() {
        // Initialise object
        Facility sample = new Facility(FACILITY_NAME);
        // Verify initial state
        Queue<String> groups = sample.getItemGroups();
        assertEquals(0, groups.size()); // Nothing should be available
        // Execute method
        sample.addItem(ROOM_ONE_NAME, StringHelper.ROOM_KEY);
        sample.addItem(ROOM_TWO_NAME, StringHelper.ROOM_KEY);
        // Test results
        // Verify results and execute test methods
        verifyFacilityModel(sample, genExpectedFacilityOutputs(
                new String[]{ROOM_ONE_NAME, StringHelper.ROOM_KEY},
                new String[]{ROOM_TWO_NAME, StringHelper.ROOM_KEY}
        ));
    }

    @Test
    void testAddItem_and_GetItemNames_DuplicateItems() {
        // Initialise object
        Facility sample = new Facility(FACILITY_NAME);
        // Execute method
        sample.addItem(ROOM_ONE_NAME, StringHelper.ROOM_KEY);
        sample.addItem(ROOM_ONE_NAME, StringHelper.ROOM_KEY);
        // Verify results and execute test methods
        verifyFacilityModel(sample, genExpectedFacilityOutputs(
                new String[]{ROOM_ONE_NAME, StringHelper.ROOM_KEY},
                new String[]{ROOM_ONE_NAME, StringHelper.ROOM_KEY}
        ));
    }

    @Test
    void testAddThresholds_and_GetThresholds() {
        // Initialise object
        Facility sample = new Facility(FACILITY_NAME);
        // Execute method
        sample.addThresholds(StringHelper.ROOM_KEY, ELECTRICITY_MEASURE, ELECTRICITY_MIN_THRESHOLD, ELECTRICITY_MAX_THRESHOLD);
        Threshold results = sample.getThresholds(StringHelper.ROOM_KEY);
        // Test results
        ThresholdTest.verifyThresholds(results, ThresholdTest.genExpectedThresholds(
                        new String[]{ELECTRICITY_MEASURE, ELECTRICITY_MIN_THRESHOLD, ELECTRICITY_MAX_THRESHOLD}
                )
        );
    }

    @Test
    void testGetItemGroups() {
        // Initialise object
        Facility sample = new Facility(FACILITY_NAME);
        sample.addItem(ROOM_ONE_NAME, StringHelper.ROOM_KEY);
        sample.addItem(SYSTEM_ONE_NAME, StringHelper.SYSTEM_KEY);
        // Execute method
        verifyFacilityModel(sample, genExpectedFacilityOutputs(
                new String[]{ROOM_ONE_NAME, StringHelper.ROOM_KEY},
                new String[]{SYSTEM_ONE_NAME, StringHelper.SYSTEM_KEY}
        ));
    }

    public static Map<String, Queue<String>> genExpectedFacilityOutputs(String[]... itemNameGroupPairs) {
        Map<String, Queue<String>> itemMappings = new HashMap<>();
        Set<String> existingNames = new HashSet<>();
        for (String[] itemNameGroupPair : itemNameGroupPairs) {
            String itemName = itemNameGroupPair[0];
            String itemGroup = itemNameGroupPair[1];
            if (!existingNames.contains(itemName)) {
                existingNames.add(itemName);
                itemMappings.computeIfAbsent(itemGroup, k -> new ArrayDeque<>()).offer(itemName);
            }
        }
        return itemMappings;
    }

    public static void verifyFacilityModel(Facility result, Map<String, Queue<String>> expectedOutputs) {
        Queue<String> groups = result.getItemGroups();
        assertEquals(expectedOutputs.keySet().size(), groups.size()); // Groups are equivalent to outputs
        groups.forEach(group -> {
            Queue<String> items = result.getItemNames(group);
            Queue<String> expectedItems = expectedOutputs.get(group);
            while (!items.isEmpty()) {
                assertEquals(expectedItems.poll(), items.poll());
            }
        });
    }
}