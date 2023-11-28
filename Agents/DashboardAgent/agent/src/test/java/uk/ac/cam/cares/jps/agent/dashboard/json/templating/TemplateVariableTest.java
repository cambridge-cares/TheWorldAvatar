package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class TemplateVariableTest {
    private static final String VARIABLE_NAME = "MEASURE ASSET";
    private static final String EXPECTED_VARIABLE_NAME = "measureasset";
    private static final Integer DASHBOARD_DISPLAY_OPTION = 0;

    @Test
    void testGenCommonJson_IncludeAllOption() {
        // Construct the object
        TemplateVariable variable = new TemplateVariable(VARIABLE_NAME, DASHBOARD_DISPLAY_OPTION, true);
        // Execute the method
        StringBuilder result = variable.genCommonJson();
        // Test outputs
        assertEquals(genExpectedCommonJsonBase(EXPECTED_VARIABLE_NAME, DASHBOARD_DISPLAY_OPTION, true), result.toString());
    }

    @Test
    void testGenCommonJson_ExcludeAllOption() {
        // Construct the object
        TemplateVariable variable = new TemplateVariable(VARIABLE_NAME, DASHBOARD_DISPLAY_OPTION, false);
        // Execute the method
        StringBuilder result = variable.genCommonJson();
        // Test outputs
        assertEquals(genExpectedCommonJsonBase(EXPECTED_VARIABLE_NAME, DASHBOARD_DISPLAY_OPTION, false), result.toString());
    }

    @Test
    void testConstruct_InvalidThrow() {
        // Construct the object
        TemplateVariable variable = new TemplateVariable(VARIABLE_NAME, DASHBOARD_DISPLAY_OPTION, true);
        // Execute the method and ensure the right error is thrown
        UnsupportedOperationException thrownError = assertThrows(UnsupportedOperationException.class, () -> variable.construct());
        // Test if error message thrown is accurate
        assertEquals("Construct() method is not supported for TemplateVariable. Please use their implementation classes instead!", thrownError.getMessage());
    }

    protected static String genExpectedCommonJsonBase(String varName, Integer dashboardDisplayOption, boolean includeAllOption) {
        StringBuilder results = new StringBuilder();
        results.append("{\"current\": {\"selected\": false,\"text\": [\"All\"],\"value\": [\"$__all\"]},\"name\": \"")
                .append(varName).append("\",\"includeAll\": ").append(includeAllOption).append(",\"multi\": true,\"hide\": ").append(dashboardDisplayOption)
                .append(",\"skipUrlSync\": false,");
        return results.toString();
    }
}