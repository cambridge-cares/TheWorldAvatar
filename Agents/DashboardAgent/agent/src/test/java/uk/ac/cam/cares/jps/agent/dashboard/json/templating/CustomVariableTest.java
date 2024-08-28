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
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION, true, true).toString(), result);
    }

    @Test
    void testConstruct_IncludeAllOption() {
        // Construct the object through the standard constructor
        CustomVariable variable = new CustomVariable(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION, true, true);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION, true, true).toString(), result);
    }

    @Test
    void testConstruct_ExcludeAllOption() {
        // Construct the object through the standard constructor
        CustomVariable variable = new CustomVariable(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION, false, false);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION, false, false).toString(), result);
    }

    @Test
    void testConstruct_IsMultiValueButDoNotSelectAll() {
        // Construct the object through the standard constructor
        CustomVariable variable = new CustomVariable(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION, true, false);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES, DASHBOARD_DISPLAY_OPTION, true, false).toString(), result);
    }

    public static String genExpectedCustomVariableSyntax(String varName, String description, String[] assets, Integer dashboardDisplayOption, boolean isMultiValue, boolean includeAllOption) {
        String jsonBase;
        String selectedAllOption;
        String filterOptions;
        if (includeAllOption) {
            selectedAllOption = "{\"selected\": true,\"text\": \"All\",\"value\": \"$__all\"},";
            String[] filterOptionsResult = genFilterOptionsForArrays(assets, false);
            jsonBase = TemplateVariableTest.genExpectedCommonJsonBase(StringHelper.formatVariableName(varName), filterOptionsResult[1], dashboardDisplayOption, isMultiValue, includeAllOption);
            filterOptions = filterOptionsResult[0];
        } else {
            String[] filterOptionsResult = genFilterOptionsForArrays(assets, true);
            jsonBase = TemplateVariableTest.genExpectedCommonJsonBase(StringHelper.formatVariableName(varName), filterOptionsResult[1], dashboardDisplayOption, isMultiValue, includeAllOption);
            selectedAllOption = "";
            filterOptions = filterOptionsResult[0];
        }
        return jsonBase +
                "\"label\": \"" + varName + "\"," +
                "\"description\": \"" + description + "\"," +
                "\"options\": [" + selectedAllOption +
                filterOptions +
                "],\"query\": \"" + genSimpleQueryForArrays(assets) +
                "\",\"queryValue\": \"\",\"type\": \"custom\"}";
    }

    private static String[] genFilterOptionsForArrays(String[] facilities, boolean isDefault) {
        StringBuilder defaultValue = new StringBuilder();
        StringBuilder results = new StringBuilder();
        for (String facility : facilities) {
            // Append comma before if it is not the only value
            if (results.length() != 0) results.append(",");
            results.append("{\"selected\": ").append(isDefault).append(",\"text\": \"").append(facility).append("\",\"value\": \"").append(facility).append("\"}");
            if (isDefault) {
                isDefault = false;
                defaultValue.append(facility);
            }
        }
        return new String[]{results.toString(), defaultValue.toString()};
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