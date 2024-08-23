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
    private static String measureOneIri;
    private static String measureOneTsIri;
    private static String measureTwoIri;
    private static String measureTwoTsIri;

    @BeforeAll
    static void setup() {
        measureOneIri = TestUtils.genInstance("ElectricityConsumption");
        measureOneTsIri = TestUtils.genTimeSeriesInstance();
        measureTwoIri = TestUtils.genInstance("ThermalConsumption");
        measureTwoTsIri = TestUtils.genTimeSeriesInstance();
    }

    @Test
    void testAddMeasure() {
        // Initialise object
        TechnicalSystem sample = new TechnicalSystem(SYSTEM_NAME, MEASURE_ONE_NAME, MEASURE_ONE_UNIT, measureOneIri, measureOneTsIri);
        // Execute method
        sample.addMeasure(MEASURE_TWO_NAME, MEASURE_TWO_UNIT, measureTwoIri, measureTwoTsIri);
        // Verify it has been added properly
        Queue<String[]> results = sample.getData();
        assertEquals(2, results.size()); // Two measure arrays should be available in the queue
        // For the first measure
        String[] measure = results.poll();
        // If unit is null, the corresponding unit stored should be null as well
        verifySystemMeasureArrayContents(MEASURE_TWO_NAME, measureTwoIri, measureTwoTsIri, null, measure);
        // For the second measure
        measure = results.poll();
        verifySystemMeasureArrayContents(MEASURE_ONE_NAME, measureOneIri, measureOneTsIri, MEASURE_ONE_UNIT, measure);
    }

    @Test
    void testGetName() {
        // Initialise object
        TechnicalSystem sample = new TechnicalSystem(SYSTEM_NAME, MEASURE_ONE_NAME, MEASURE_ONE_UNIT, measureOneIri, measureOneTsIri);
        // Execute and test method
        assertEquals(SYSTEM_NAME, sample.getName());
    }

    @Test
    void testGetData() {
        // Initialise object
        TechnicalSystem sample = new TechnicalSystem(SYSTEM_NAME, MEASURE_ONE_NAME, MEASURE_ONE_UNIT, measureOneIri, measureOneTsIri);
        // Execute method to retrieve the simple measure
        Queue<String[]> results = sample.getData();
        // Test results
        assertEquals(1, results.size()); // Only one measure array should be available in the queue
        String[] measure = results.poll();
        verifySystemMeasureArrayContents(MEASURE_ONE_NAME, measureOneIri, measureOneTsIri, MEASURE_ONE_UNIT, measure);
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