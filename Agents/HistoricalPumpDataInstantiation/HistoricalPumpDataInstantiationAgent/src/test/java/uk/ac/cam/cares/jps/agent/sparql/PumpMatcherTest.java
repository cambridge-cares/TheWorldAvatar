package uk.ac.cam.cares.jps.agent.sparql;

import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class PumpMatcherTest {
    @Test
    void testGenMappings() {
        Map<String, String> pumpMap = PumpMatcher.genMappings();
        assertTrue(pumpMap.size() == 17);
    }
}