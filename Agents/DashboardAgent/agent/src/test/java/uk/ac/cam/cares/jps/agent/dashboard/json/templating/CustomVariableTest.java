package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class CustomVariableTest {
    private static final String[] ASSET_NAMES = new String[]{"T1", "T2", "T3"};
    private static final String EXPECTED_ASSET_TYPE = "Table";
    private static final Integer DASHBOARD_DISPLAY_OPTION = 0;

    @Test
    void testConstruct() {
        // Construct the object through the standard constructor
        CustomVariable variable = new CustomVariable(EXPECTED_ASSET_TYPE, ASSET_NAMES, DASHBOARD_DISPLAY_OPTION);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_ASSET_TYPE, ASSET_NAMES, DASHBOARD_DISPLAY_OPTION).toString(), result);
    }

    public static StringBuilder genExpectedCustomVariableSyntax(String varName, String[] assets, Integer dashboardDisplayOption) {
        StringBuilder results = new StringBuilder();
        results.append(TemplateVariableTest.genExpectedCommonJsonBase(varName.toLowerCase(), dashboardDisplayOption))
                .append("\"label\": \"");
        // Only append the varName in label if it is not the generic asset class
        if (!varName.equals("Assets")) results.append(varName).append(" ");
        results.append("Assets\",")
                .append("\"description\": \"Default filters for the ");
        // Only append the varName in description if it is not the generic asset class
        if (!varName.equals("Assets")) results.append(varName).append(" ");
        results.append("Assets\",")
                .append("\"options\": [{\"selected\": true,\"text\": \"All\",\"value\": \"$__all\"},").append(genFilterOptionsForArrays(assets))
                .append("],\"query\": \"").append(genSimpleQueryForArrays(assets))
                .append("\",\"queryValue\": \"\",\"type\": \"custom\"}");
        return results;
    }

    private static StringBuilder genFilterOptionsForArrays(String[] assets) {
        StringBuilder results = new StringBuilder();
        for (String asset : assets) {
            // Append comma before if it is not the only value
            if (results.length() != 0) results.append(",");
            results.append("{\"selected\": false,\"text\": \"").append(asset).append("\",\"value\": \"").append(asset).append("\"}");
        }
        return results;
    }

    private static StringBuilder genSimpleQueryForArrays(String[] assets) {
        StringBuilder results = new StringBuilder();
        for (String asset : assets) {
            // Append comma before if it is not the only value
            if (results.length() != 0) results.append(",");
            results.append(asset);
        }
        return results;
    }
}