package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import static org.junit.jupiter.api.Assertions.*;

class CustomVariableTest {
    private static final String[] FACILITIES = new String[]{"F1", "F2", "F3"};
    private static final String DESCRIPTION = "custom variable filter";
    private static final String EXPECTED_TYPE_NAME = "Facilities";
    private static final Integer DASHBOARD_DISPLAY_OPTION = 0;

    @Test
    void testConstruct_OptionalConstructor() {
        // Construct the object through the standard constructor
        CustomVariable variable = new CustomVariable(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION, true).toString(), result);
    }

    @Test
    void testConstruct_IncludeAllOption() {
        // Construct the object through the standard constructor
        CustomVariable variable = new CustomVariable(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION, true);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION, true).toString(), result);
    }

    @Test
    void testConstruct_ExcludeAllOption() {
        // Construct the object through the standard constructor
        CustomVariable variable = new CustomVariable(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION, false);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION, false).toString(), result);
    }

    public static StringBuilder genExpectedCustomVariableSyntax(String varName, String description, String[] assets, Integer dashboardDisplayOption, boolean includeAllOption) {
        StringBuilder results = new StringBuilder();
        results.append(TemplateVariableTest.genExpectedCommonJsonBase(StringHelper.formatVariableName(varName), dashboardDisplayOption, includeAllOption))
                .append("\"label\": \"").append(varName).append("\",")
                .append("\"description\": \"").append(description).append("\",")
                .append("\"options\": [");
        boolean isDefault = true;
        if (includeAllOption) {
            results.append("{\"selected\": true,\"text\": \"All\",\"value\": \"$__all\"},");
            isDefault = false;
        }
        results.append(genFilterOptionsForArrays(assets, isDefault))
                .append("],\"query\": \"").append(genSimpleQueryForArrays(assets))
                .append("\",\"queryValue\": \"\",\"type\": \"custom\"}");
        return results;
    }

    private static StringBuilder genFilterOptionsForArrays(String[] facilities, boolean isDefault) {
        StringBuilder results = new StringBuilder();
        for (String facility : facilities) {
            // Append comma before if it is not the only value
            if (results.length() != 0) results.append(",");
            results.append("{\"selected\": ").append(isDefault).append(",\"text\": \"").append(facility).append("\",\"value\": \"").append(facility).append("\"}");
            if (isDefault) isDefault = false;
        }
        return results;
    }

    private static StringBuilder genSimpleQueryForArrays(String[] facilities) {
        StringBuilder results = new StringBuilder();
        for (String facility : facilities) {
            // Append comma before if it is not the only value
            if (results.length() != 0) results.append(",");
            results.append(facility);
        }
        return results;
    }
}