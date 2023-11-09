package uk.ac.cam.cares.jps.agent.dashboard.stack.sparql.datamodel;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;

import java.util.Queue;

import static org.junit.jupiter.api.Assertions.assertEquals;

class TechnicalSystemTest {
    private static final String SYSTEM_NAME = "Emergency (Area) ";
    private static final String MEASURE_ONE_NAME = "Electricity Consumption";
    private static final String MEASURE_ONE_UNIT = "kW";
    private static final String MEASURE_TWO_NAME = "Thermal Consumption";
    private static final String MEASURE_TWO_UNIT = null;
    private static String MEASURE_ONE_IRI;
    private static String MEASURE_ONE_TS_IRI;
    private static String MEASURE_TWO_IRI;
    private static String MEASURE_TWO_TS_IRI;

    @BeforeAll
    static void setup() {
        MEASURE_ONE_IRI = TestUtils.genInstance("ElectricityConsumption");
        MEASURE_ONE_TS_IRI = TestUtils.genTimeSeriesInstance();
        MEASURE_TWO_IRI = TestUtils.genInstance("ThermalConsumption");
        MEASURE_TWO_TS_IRI = TestUtils.genTimeSeriesInstance();
    }

    @Test
    void testAddMeasure() {
        // Initialise object
        TechnicalSystem sample = new TechnicalSystem(SYSTEM_NAME, MEASURE_ONE_NAME, MEASURE_ONE_UNIT, MEASURE_ONE_IRI, MEASURE_ONE_TS_IRI);
        // Execute method
        sample.addMeasure(MEASURE_TWO_NAME, MEASURE_TWO_UNIT, MEASURE_TWO_IRI, MEASURE_TWO_TS_IRI);
        // Verify it has been added properly
        Queue<String[]> results = sample.getData();
        assertEquals(2, results.size()); // Two measure arrays should be available in the queue
        // For the first measure
        String[] measure = results.poll();
        // If unit is null, the corresponding unit stored should be null as well
        verifySystemMeasureArrayContents(MEASURE_TWO_NAME, MEASURE_TWO_IRI, MEASURE_TWO_TS_IRI, null, measure);
        // For the second measure
        measure = results.poll();
        verifySystemMeasureArrayContents(MEASURE_ONE_NAME, MEASURE_ONE_IRI, MEASURE_ONE_TS_IRI, MEASURE_ONE_UNIT, measure);
    }

    @Test
    void testGetName() {
        // Initialise object
        TechnicalSystem sample = new TechnicalSystem(SYSTEM_NAME, MEASURE_ONE_NAME, MEASURE_ONE_UNIT, MEASURE_ONE_IRI, MEASURE_ONE_TS_IRI);
        // Execute and test method
        assertEquals(SYSTEM_NAME, sample.getName());
    }

    @Test
    void testGetData() {
        // Initialise object
        TechnicalSystem sample = new TechnicalSystem(SYSTEM_NAME, MEASURE_ONE_NAME, MEASURE_ONE_UNIT, MEASURE_ONE_IRI, MEASURE_ONE_TS_IRI);
        // Execute method to retrieve the simple measure
        Queue<String[]> results = sample.getData();
        // Test results
        assertEquals(1, results.size()); // Only one measure array should be available in the queue
        String[] measure = results.poll();
        verifySystemMeasureArrayContents(MEASURE_ONE_NAME, MEASURE_ONE_IRI, MEASURE_ONE_TS_IRI, MEASURE_ONE_UNIT, measure);
    }

    protected static void verifySystemMeasureArrayContents(String measureName, String measureIRI, String measureTSIri, String measureUnit, String[] measure) {
        assertEquals(5, measure.length); // Verify that there are 5 measures included
        assertEquals(measureName, measure[0]);
        assertEquals(measureIRI, measure[1]);
        assertEquals(measureTSIri, measure[2]);
        assertEquals(measureUnit, measure[3]);
        assertEquals("systems", measure[4]);
    }
}