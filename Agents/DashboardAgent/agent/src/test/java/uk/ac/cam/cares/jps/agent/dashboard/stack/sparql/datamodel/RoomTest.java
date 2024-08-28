package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.datamodel;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;

import java.util.Queue;

import static org.junit.jupiter.api.Assertions.*;

class RoomTest {
    private static final String ROOM_NAME = "Lamp1";
    private static final String MEASURE_ONE_NAME = "Electricity Consumption";
    private static final String MEASURE_ONE_UNIT = "kW";
    private static final String MEASURE_TWO_NAME = "Ambient Relative Humidity";
    private static final String MEASURE_TWO_UNIT = null;
    private static String measureOneIri;
    private static String measureOneTsIri;
    private static String measureTwoIri;
    private static String measureTwoTsIri;

    @BeforeAll
    static void setup() {
        measureOneIri = TestUtils.genInstance("ElectricityConsumption");
        measureOneTsIri = TestUtils.genTimeSeriesInstance();
        measureTwoIri = TestUtils.genInstance("Humidity");
        measureTwoTsIri = TestUtils.genTimeSeriesInstance();
    }

    @Test
    void testAddMeasure() {
        // Initialise object
        Room sample = new Room(ROOM_NAME, MEASURE_ONE_NAME, MEASURE_ONE_UNIT, measureOneIri, measureOneTsIri);
        // Execute method
        sample.addMeasure(MEASURE_TWO_NAME, MEASURE_TWO_UNIT, measureTwoIri, measureTwoTsIri);
        // Verify it has been added properly
        Queue<String[]> results = sample.getRoomData();
        assertEquals(2, results.size()); // Two measure arrays should be available in the queue
        // For the first measure
        String[] measure = results.poll();
        // If unit is null, the corresponding unit stored should be null as well
        verifyRoomMeasureArrayContents(MEASURE_TWO_NAME, measureTwoIri, measureTwoTsIri, null, measure);
        // For the second measure
        measure = results.poll();
        verifyRoomMeasureArrayContents(MEASURE_ONE_NAME, measureOneIri, measureOneTsIri, MEASURE_ONE_UNIT, measure);
    }

    @Test
    void testGetRoomName() {
        // Initialise object
        Room sample = new Room(ROOM_NAME, MEASURE_ONE_NAME, MEASURE_ONE_UNIT, measureOneIri, measureOneTsIri);
        // Execute and test method
        assertEquals(ROOM_NAME, sample.getRoomName());
    }

    @Test
    void testGetRoomData() {
        // Initialise object
        Room sample = new Room(ROOM_NAME, MEASURE_ONE_NAME, MEASURE_ONE_UNIT, measureOneIri, measureOneTsIri);
        // Execute method to retrieve the simple measure
        Queue<String[]> results = sample.getRoomData();
        // Test results
        assertEquals(1, results.size()); // Only one measure array should be available in the queue
        String[] measure = results.poll();
        verifyRoomMeasureArrayContents(MEASURE_ONE_NAME, measureOneIri, measureOneTsIri, MEASURE_ONE_UNIT, measure);
    }

    protected static void verifyRoomMeasureArrayContents(String measureName, String measureIRI, String measureTSIri, String measureUnit, String[] measure) {
        assertEquals(5, measure.length); // Verify that there are 4 measures included
        assertEquals(measureName, measure[0]);
        assertEquals(measureIRI, measure[1]);
        assertEquals(measureTSIri, measure[2]);
        assertEquals(measureUnit, measure[3]);
        assertEquals("Rooms", measure[4]);
    }
}