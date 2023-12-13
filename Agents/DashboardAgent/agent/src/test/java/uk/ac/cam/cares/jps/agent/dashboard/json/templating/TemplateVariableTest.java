package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class TemplateVariableTest {
    private static final String VARIABLE_NAME = "MEASURE ASSET";
    private static final String EXPECTED_VARIABLE_NAME = "measureasset";
    private static final Integer DASHBOARD_DISPLAY_OPTION = 0;

    @Test
    void testSetDefaultSelectionTextValue() {
        // Setup
        String sampleText = "default";
        // Construct the object
        TemplateVariable variable = new TemplateVariable(VARIABLE_NAME, DASHBOARD_DISPLAY_OPTION, true, true);
        // Execute the method
        variable.setDefaultSelectedTextValue(sampleText);
        String result = variable.genCommonJson();
        // Test outputs
        assertEquals(genExpectedCommonJsonBase(EXPECTED_VARIABLE_NAME, sampleText, DASHBOARD_DISPLAY_OPTION, true, true), result);
    }

    @Test
    void testGenCommonJson_AllTrueBooleans() {
        // Construct the object
        TemplateVariable variable = new TemplateVariable(VARIABLE_NAME, DASHBOARD_DISPLAY_OPTION, true, true);
        // Execute the method
        String result = variable.genCommonJson();
        // Test outputs
        assertEquals(genExpectedCommonJsonBase(EXPECTED_VARIABLE_NAME, "", DASHBOARD_DISPLAY_OPTION, true, true), result);
    }

    @Test
    void testGenCommonJson_AllFalseBooleans() {
        // Construct the object
        TemplateVariable variable = new TemplateVariable(VARIABLE_NAME, DASHBOARD_DISPLAY_OPTION, false, false);
        // Execute the method
        String result = variable.genCommonJson();
        // Test outputs
        assertEquals(genExpectedCommonJsonBase(EXPECTED_VARIABLE_NAME, "", DASHBOARD_DISPLAY_OPTION, false, false), result);
    }

    @Test
    void testConstruct_InvalidThrow() {
        // Construct the object
        TemplateVariable variable = new TemplateVariable(VARIABLE_NAME, DASHBOARD_DISPLAY_OPTION, true, true);
        // Execute the method and ensure the right error is thrown
        UnsupportedOperationException thrownError = assertThrows(UnsupportedOperationException.class, () -> variable.construct());
        // Test if error message thrown is accurate
        assertEquals("Construct() method is not supported for TemplateVariable. Please use their implementation classes instead!", thrownError.getMessage());
    }

    protected static String genExpectedCommonJsonBase(String varName, String defaultVal, Integer dashboardDisplayOption, boolean isMultiValue, boolean includeAllOption) {
        String defaultSelectedText = "All";
        String defaultSelectedValue = "$__all";
        if (!defaultVal.isEmpty()) {
            defaultSelectedText = defaultVal;
            defaultSelectedValue = defaultVal;
        }
        return "{\"current\": {\"selected\": false,\"text\": [\"" + defaultSelectedText +
                "\"],\"value\": [\"" + defaultSelectedValue + "\"]},\"name\": \"" +
                varName + "\",\"includeAll\": " + includeAllOption + ",\"multi\":" + isMultiValue + ",\"hide\": " + dashboardDisplayOption +
                ",\"skipUrlSync\": false,";
    }
}