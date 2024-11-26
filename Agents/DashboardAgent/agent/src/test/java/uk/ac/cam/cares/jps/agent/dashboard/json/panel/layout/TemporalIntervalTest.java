package uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout;

import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class TemporalIntervalTest {

    @Test
    void testGetMonthMap() {
        Map<String, String> result = TemporalInterval.getMonthMap();
        assertEquals(12, result.size());
        assertEquals("1", result.get("January"));
        assertEquals("2", result.get("February"));
        assertEquals("3", result.get("March"));
        assertEquals("4", result.get("April"));
        assertEquals("5", result.get("May"));
        assertEquals("6", result.get("June"));
        assertEquals("7", result.get("July"));
        assertEquals("8", result.get("August"));
        assertEquals("9", result.get("September"));
        assertEquals("10", result.get("October"));
        assertEquals("11", result.get("November"));
        assertEquals("12", result.get("December"));
    }
}