package uk.ac.cam.cares.jps.agent.dashboard.datamodel;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

public class ThresholdTest {
    private static Threshold SAMPLE;
    private static final String ELECTRICITY_MEASURE = "electricity";
    private static final String ELECTRICITY_MIN_THRESHOLD = "5";
    private static final String ELECTRICITY_MAX_THRESHOLD = "100";

    @BeforeEach
    void setup() {
        SAMPLE = new Threshold();
    }

    @Test
    void testAddThreshold_and_GetThreshold() {
        SAMPLE.addThreshold(ELECTRICITY_MEASURE, ELECTRICITY_MIN_THRESHOLD, ELECTRICITY_MAX_THRESHOLD);
        verifyThresholds(SAMPLE, genExpectedThresholds(new String[]{ELECTRICITY_MEASURE, ELECTRICITY_MIN_THRESHOLD, ELECTRICITY_MAX_THRESHOLD}));
    }

    @Test
    void testGetThreshold_MissingThreshold() {
        verifyThresholds(SAMPLE, new HashMap<>());
    }

    @Test
    void testContains() {
        SAMPLE.addThreshold(ELECTRICITY_MEASURE, ELECTRICITY_MIN_THRESHOLD, ELECTRICITY_MAX_THRESHOLD);
        assertTrue(SAMPLE.contains(ELECTRICITY_MEASURE));
        assertFalse(SAMPLE.contains("Invalid"));
    }

    public static Map<String, String[]> genExpectedThresholds(String[]... expectedMinMaxThresholds) {
        Map<String, String[]> thresholds = new HashMap<>();
        for (String[] threshold : expectedMinMaxThresholds) {
            String measureName = threshold[0];
            String minThreshold = threshold[1];
            String maxThreshold = threshold[2];
            thresholds.computeIfAbsent(measureName, k -> new String[]{minThreshold, maxThreshold});
        }
        return thresholds;
    }

    public static void verifyThresholds(Threshold result, Map<String, String[]> expectedThresholdMappings) {
        if (!expectedThresholdMappings.isEmpty()) {
            expectedThresholdMappings.keySet().forEach(measure -> {
                String[] thresholds = result.getThreshold(measure);
                String[] expectedThresholds = expectedThresholdMappings.get(measure);
                assertEquals(expectedThresholds[0], thresholds[0]);
                assertEquals(expectedThresholds[1], thresholds[1]);
            });
        }
    }
}