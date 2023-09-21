package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

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
        String itemLabel = "";
        // Only append the varName in label if it is not the generic asset class
        if (!varName.equals("Assets")) itemLabel = varName;
        // Only append the assets if it is not a room
        if (!varName.equals(StringHelper.ROOM_KEY)) itemLabel += " Assets";
        // Generate syntax
        results.append(TemplateVariableTest.genExpectedCommonJsonBase(varName.toLowerCase(), dashboardDisplayOption))
                .append("\"label\": \"").append(itemLabel).append("\",")
                .append("\"description\": \"Default filters for the ").append(itemLabel).append("\",")
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