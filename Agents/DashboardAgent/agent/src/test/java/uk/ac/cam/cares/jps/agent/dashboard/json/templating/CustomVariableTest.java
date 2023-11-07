package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class CustomVariableTest {
    private static final String[] FACILITIES = new String[]{"F1", "F2", "F3"};
    private static final String EXPECTED_TYPE_NAME = "Facilities";
    private static final Integer DASHBOARD_DISPLAY_OPTION = 0;

    @Test
    void testConstruct() {
        // Construct the object through the standard constructor
        CustomVariable variable = new CustomVariable(EXPECTED_TYPE_NAME, FACILITIES, DASHBOARD_DISPLAY_OPTION);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_TYPE_NAME, FACILITIES, DASHBOARD_DISPLAY_OPTION).toString(), result);
    }

    public static StringBuilder genExpectedCustomVariableSyntax(String varName, String[] assets, Integer dashboardDisplayOption) {
        StringBuilder results = new StringBuilder();
        results.append(TemplateVariableTest.genExpectedCommonJsonBase(varName.toLowerCase(), dashboardDisplayOption))
                .append("\"label\": \"").append(varName).append("\",")
                .append("\"description\": \"Default filters to view the specified facilities and their associated measures.\",")
                .append("\"options\": [{\"selected\": true,\"text\": \"All\",\"value\": \"$__all\"},").append(genFilterOptionsForArrays(assets))
                .append("],\"query\": \"").append(genSimpleQueryForArrays(assets))
                .append("\",\"queryValue\": \"\",\"type\": \"custom\"}");
        return results;
    }

    private static StringBuilder genFilterOptionsForArrays(String[] facilities) {
        StringBuilder results = new StringBuilder();
        for (String facility : facilities) {
            // Append comma before if it is not the only value
            if (results.length() != 0) results.append(",");
            results.append("{\"selected\": false,\"text\": \"").append(facility).append("\",\"value\": \"").append(facility).append("\"}");
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