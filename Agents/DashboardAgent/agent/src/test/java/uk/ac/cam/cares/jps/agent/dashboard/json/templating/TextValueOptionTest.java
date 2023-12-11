package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class TextValueOptionTest {
    private static final String ALL_LABEL = "All";
    private static final String ALL_VALUE = "$__all";
    private static final String ASSET = "T-01";

    @Test
    void testConstructForAllOption() {
        // Construct the object
        TextValueOption option = new TextValueOption(true, ALL_LABEL, ALL_VALUE);
        // Execute the method
        String result = option.construct();
        // Test outputs
        assertEquals(genExpectedJsonSyntax(true, ALL_LABEL, ALL_VALUE), result);
    }

    @Test
    void testConstructForAssetOption() {
        // Construct the object
        TextValueOption option = new TextValueOption(false, ASSET, ASSET);
        // Execute the method
        String result = option.construct();
        // Test outputs
        assertEquals(genExpectedJsonSyntax(false, ASSET, ASSET), result);
    }

    private static String genExpectedJsonSyntax(boolean isSelected, String label, String value) {
        return "{" +
                "\"selected\": " + isSelected + "," +
                // Display text
                "\"text\": \"" + label + "\"," +
                // Value for processing in Grafana
                "\"value\": \"" + value + "\"" +
                "}";
    }
}