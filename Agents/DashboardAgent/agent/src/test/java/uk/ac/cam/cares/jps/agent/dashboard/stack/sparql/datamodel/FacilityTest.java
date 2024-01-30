package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.datamodel;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class FacilityTest {
    private static final String FACILITY_NAME = "Apartment unit";
    private static final String ROOM_ONE_NAME = "kitchen";
    private static final String ROOM_TWO_NAME = "Bedroom";
    private static final String ASSET_ONE_NAME = "Lamp";
    private static final String ASSET_TWO_NAME = "Fridge";

    @Test
    void testGetFacilityName() {
        // Initialise object
        Facility sample = new Facility(FACILITY_NAME, ROOM_ONE_NAME);
        // Execute and test method
        assertEquals(FACILITY_NAME, sample.getFacilityName());
    }

    @Test
    void testAddItem() {
        // Initialise object
        Facility sample = new Facility(FACILITY_NAME, ROOM_ONE_NAME);
        // Execute method
        sample.addItem(ROOM_TWO_NAME);
        // Verify it has been added properly
        String[] results = sample.getFacilityData();
        // Test results
        assertEquals(3, results.length); // The array should have three items
        assertEquals(FACILITY_NAME, results[0]);
        assertTrue(results[1].equals(ROOM_ONE_NAME) || results[1].equals(ROOM_TWO_NAME));
        assertTrue(results[2].equals(ROOM_ONE_NAME) || results[2].equals(ROOM_TWO_NAME));

        // Execute method again to add more items
        sample.addItem(ASSET_ONE_NAME);
        sample.addItem(ASSET_TWO_NAME);
        // Verify it has been added properly
        results = sample.getFacilityData();
        assertEquals(5, results.length); // The array should have five items
        assertEquals(FACILITY_NAME, results[0]);
    }

    @Test
    void testAddItem_Duplicate() {
        // Initialise object
        Facility sample = new Facility(FACILITY_NAME, ROOM_ONE_NAME);
        // Execute method
        sample.addItem(ROOM_ONE_NAME);
        // Test results
        String[] results = sample.getFacilityData();
        assertEquals(2, results.length); // The array should only have two items as it is a duplicate
        assertEquals(FACILITY_NAME, results[0]);
        assertEquals(ROOM_ONE_NAME, results[1]);
    }

    @Test
    void testGetFacilityData() {
        // Initialise object
        Facility sample = new Facility(FACILITY_NAME, ROOM_ONE_NAME);
        // Execute method
        String[] results = sample.getFacilityData();
        // Test results
        assertEquals(2, results.length); // The array should only have two items
        assertEquals(FACILITY_NAME, results[0]);
        assertEquals(ROOM_ONE_NAME, results[1]);
    }
}