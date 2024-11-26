package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class CustomVariableTest {
    private static ArrayDeque<String> FACILITIES;
    private static List<String> FACILITIES_LIST;
    private static final String FACILITY_ONE = "F1";
    private static final String FACILITY_TWO = "F2";
    private static final String FACILITY_THREE = "F3";
    private static final String DESCRIPTION = "custom variable filter";
    private static final String EXPECTED_TYPE_NAME = "Facilities";
    private static final Integer DASHBOARD_DISPLAY_OPTION = 0;

    @BeforeAll
    static void setup() {
        FACILITIES = new ArrayDeque<>();
        FACILITIES.offer(FACILITY_ONE);
        FACILITIES.offer(FACILITY_TWO);
        FACILITIES.offer(FACILITY_THREE);
        FACILITIES_LIST = new ArrayList<>(FACILITIES.clone());
    }

    @Test
    void testConstruct_OptionalConstructor() {
        // Construct the object through the standard constructor
        CustomVariable variable = new CustomVariable(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES.clone(), DASHBOARD_DISPLAY_OPTION);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES_LIST, DASHBOARD_DISPLAY_OPTION, true, true), result);
    }

    @Test
    void testConstruct_IncludeAllOption() {
        // Construct the object through the standard constructor
        CustomVariable variable = new CustomVariable(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES.clone(), DASHBOARD_DISPLAY_OPTION, true, true);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES_LIST, DASHBOARD_DISPLAY_OPTION, true, true), result);
    }

    @Test
    void testConstruct_ExcludeAllOption() {
        // Construct the object through the standard constructor
        CustomVariable variable = new CustomVariable(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES.clone(), DASHBOARD_DISPLAY_OPTION, false, false);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES_LIST, DASHBOARD_DISPLAY_OPTION, false, false), result);
    }

    @Test
    void testConstruct_IsMultiValueButDoNotSelectAll() {
        // Construct the object through the standard constructor
        CustomVariable variable = new CustomVariable(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES.clone(), DASHBOARD_DISPLAY_OPTION, true, false);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedCustomVariableSyntax(EXPECTED_TYPE_NAME, DESCRIPTION, FACILITIES_LIST, DASHBOARD_DISPLAY_OPTION, true, false), result);
    }

    public static String genExpectedCustomVariableSyntax(String varName, String description, List<String> itemNames, Integer dashboardDisplayOption, boolean isMultiValue, boolean includeAllOption) {
        String jsonBase;
        String selectedAllOption;
        String filterOptions;
        if (includeAllOption) {
            selectedAllOption = "{\"selected\": true,\"text\": \"All\",\"value\": \"$__all\"},";
            String[] filterOptionsResult = genFilterOptionsForArrays(itemNames, false);
            jsonBase = TemplateVariableTest.genExpectedCommonJsonBase(StringHelper.formatVariableName(varName), filterOptionsResult[1], dashboardDisplayOption, isMultiValue, includeAllOption);
            filterOptions = filterOptionsResult[0];
        } else {
            String[] filterOptionsResult = genFilterOptionsForArrays(itemNames, true);
            jsonBase = TemplateVariableTest.genExpectedCommonJsonBase(StringHelper.formatVariableName(varName), filterOptionsResult[1], dashboardDisplayOption, isMultiValue, includeAllOption);
            selectedAllOption = "";
            filterOptions = filterOptionsResult[0];
        }
        return jsonBase +
                "\"label\": \"" + varName + "\"," +
                "\"description\": \"" + description + "\"," +
                "\"options\": [" + selectedAllOption +
                filterOptions +
                "],\"query\": \"" + genSimpleQueryForArrays(itemNames) +
                "\",\"queryValue\": \"\",\"type\": \"custom\"}";
    }

    private static String[] genFilterOptionsForArrays(List<String> itemNames, boolean isDefault) {
        StringBuilder defaultValue = new StringBuilder();
        StringBuilder results = new StringBuilder();
        for (String itemName : itemNames) {
            // Append comma before if it is not the only value
            if (results.length() != 0) results.append(",");
            results.append("{\"selected\": ").append(isDefault).append(",\"text\": \"").append(itemName).append("\",\"value\": \"").append(itemName).append("\"}");
            if (isDefault) {
                isDefault = false;
                defaultValue.append(itemName);
            }
        }
        return new String[]{results.toString(), defaultValue.toString()};
    }

    private static StringBuilder genSimpleQueryForArrays(List<String> itemNames) {
        StringBuilder results = new StringBuilder();
        for (String itemName : itemNames) {
            // Append comma before if it is not the only value
            if (results.length() != 0) results.append(",");
            results.append(itemName);
        }
        return results;
    }
}
