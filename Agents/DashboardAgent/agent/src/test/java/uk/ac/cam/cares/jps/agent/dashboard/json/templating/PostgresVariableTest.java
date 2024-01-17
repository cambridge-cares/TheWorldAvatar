package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class PostgresVariableTest {
    private static final String EXPECTED_ASSET_TYPE = "Fridge";
    private static final String EXPECTED_MEASURE = "Energy Consumption";
    private static final String EXPECTED_VARIABLE = "Reference Month";
    private static final String DATABASE_ID = "nhsaf781rh";
    private static final Map<String, List<String>> FACILITY_ITEM_MAPPING = new HashMap<>();
    private static final String FACILITY_ONE = "Home";
    private static final String FACILITY_ONE_ASSET = "Bed";
    private static final String FACILITY_TWO = "Bakery";
    private static final String FACILITY_TWO_ASSET = "Oven";
    private static final String FACILITY_TWO_ROOM = "Kitchen";
    private static final List<String[]> ASSET_TS_COL_LIST = new ArrayList<>();
    private static final String[] TEST_SET1 = new String[]{"F1", "column1"};
    private static final String[] TEST_SET2 = new String[]{"F2", "column2"};
    private static final String[] TEST_SET3 = new String[]{"F3", "column3"};
    private static final Map<String, String> SAMPLE_KEY_VALUE_PAIRS = new HashMap<>();
    private static final String PAIR_ONE_KEY = "Jan";
    private static final String PAIR_ONE_VALUE = "1";
    private static final String PAIR_TWO_KEY = "Feb";
    private static final String PAIR_TWO_VALUE = "2";

    @BeforeAll
    static void genTestAssetMeasureList() {
        FACILITY_ITEM_MAPPING.put(FACILITY_ONE, List.of(FACILITY_ONE_ASSET));
        FACILITY_ITEM_MAPPING.put(FACILITY_TWO, Arrays.asList(FACILITY_TWO_ASSET, FACILITY_TWO_ROOM));
        ASSET_TS_COL_LIST.add(TEST_SET1);
        ASSET_TS_COL_LIST.add(TEST_SET2);
        ASSET_TS_COL_LIST.add(TEST_SET3);
        SAMPLE_KEY_VALUE_PAIRS.put(PAIR_ONE_KEY, PAIR_ONE_VALUE);
        SAMPLE_KEY_VALUE_PAIRS.put(PAIR_TWO_KEY, PAIR_TWO_VALUE);
    }

    @Test
    void testConstruct_GenericFilter() {
        // Construct the object through the alternate constructor
        PostgresVariable variable = new PostgresVariable(EXPECTED_VARIABLE, DATABASE_ID, SAMPLE_KEY_VALUE_PAIRS);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedPostgresVarSyntaxForGenericFilter(EXPECTED_VARIABLE, DATABASE_ID, SAMPLE_KEY_VALUE_PAIRS), result);
    }

    @Test
    void testConstruct_ItemFilterForAsset() {
        // Construct the object through the alternate constructor
        PostgresVariable variable = new PostgresVariable(EXPECTED_ASSET_TYPE, FACILITY_ITEM_MAPPING, DATABASE_ID);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedPostgresVarSyntaxForItemFilter(EXPECTED_ASSET_TYPE, DATABASE_ID, FACILITY_ITEM_MAPPING), result);
    }

    @Test
    void testConstruct_ItemFilterForRoom() {
        // Construct the object through the alternate constructor
        PostgresVariable variable = new PostgresVariable(StringHelper.ROOM_KEY, FACILITY_ITEM_MAPPING, DATABASE_ID);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedPostgresVarSyntaxForItemFilter(StringHelper.ROOM_KEY, DATABASE_ID, FACILITY_ITEM_MAPPING), result);
    }

    @Test
    void testConstruct_ItemFilterForSystem() {
        // Construct the object through the alternate constructor
        PostgresVariable variable = new PostgresVariable(StringHelper.SYSTEM_KEY, FACILITY_ITEM_MAPPING, DATABASE_ID);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedPostgresVarSyntaxForItemFilter(StringHelper.SYSTEM_KEY, DATABASE_ID, FACILITY_ITEM_MAPPING), result);
    }

    @Test
    void testConstruct_MeasureFilter() {
        // Construct the object through the alternate constructor
        PostgresVariable variable = new PostgresVariable(EXPECTED_MEASURE, EXPECTED_ASSET_TYPE, DATABASE_ID, ASSET_TS_COL_LIST);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedPostgresVarSyntaxForMeasureFilter(EXPECTED_MEASURE, EXPECTED_ASSET_TYPE, DATABASE_ID, ASSET_TS_COL_LIST), result);
    }

    public static String genExpectedPostgresVarSyntaxForGenericFilter(String varName, String databaseID, Map<String, String> keyValuePairs) {
        String formattedName = varName.toLowerCase().replaceAll("\\s", "");
        String description = "A hidden filter that displays the " + StringHelper.addSpaceBetweenCapitalWords(varName).toLowerCase()
                + " as requested by the user";
        StringBuilder temp = new StringBuilder();
        for (Map.Entry<String, String> entry : keyValuePairs.entrySet()) {
            // Only append a comma at the start if it is not the first value
            if (temp.length() != 0) temp.append(", ");
            // Append the name and the corresponding column name
            temp.append("('").append(entry.getKey()).append("', '")
                    .append(entry.getValue()).append("')");
        }
        String query = "SELECT k AS \\\"__text\\\", v AS \\\"__value\\\" FROM (values " +
                temp + ") AS v(k,v);";
        return genExpectedPostgresVarSyntax(formattedName, varName, description, databaseID, query, 2, false, false);
    }

    public static String genExpectedPostgresVarSyntaxForItemFilter(String itemType, String databaseID, Map<String, List<String>> facilityItemMapping) {
        String formattedItemType = itemType.toLowerCase().replaceAll("\\s", "");
        String label = itemType.equals(StringHelper.ROOM_KEY) ? "Rooms" :
                itemType.equals(StringHelper.SYSTEM_KEY) ? "Smart Meter" : StringHelper.addSpaceBetweenCapitalWords(itemType);
        String description = "A filter for the items of " + label.toLowerCase() + " type.";
        List<String[]> parsedMappings = new ArrayList<>();
        // Parse the input map to give a list containing arrays in the form of [Facility, Item]
        for (String facility : facilityItemMapping.keySet()) {
            List<String> containingItems = facilityItemMapping.get(facility);
            containingItems.forEach((item) -> {
                String[] mapping = new String[2];
                mapping[0] = facility;
                mapping[1] = item;
                parsedMappings.add(mapping);
            });
        }
        String query = "SELECT v AS \\\"__value\\\" FROM (values " +
                genValueQueryForListOfArrays(parsedMappings) + ") AS v(k,v)  WHERE k IN (${" + StringHelper.FACILITY_KEY + "});";
        return genExpectedPostgresVarSyntax(formattedItemType, label, description, databaseID, query, 0, true, true);
    }

    public static String genExpectedPostgresVarSyntaxForMeasureFilter(String measure, String assetType, String databaseID, List<String[]> assetMeasureMap) {
        String formattedMeasure = measure.toLowerCase().replaceAll("\\s", "");
        String formattedAssetType = assetType.toLowerCase().replaceAll("\\s", "");
        String description = "A hidden filter that displays the corresponding time series of " + StringHelper.addSpaceBetweenCapitalWords(measure).toLowerCase()
                + " for " + StringHelper.addSpaceBetweenCapitalWords(assetType).toLowerCase();
        String query = "SELECT k AS \\\"__text\\\", v AS \\\"__value\\\" FROM (values " +
                genValueQueryForListOfArrays(assetMeasureMap) + ") AS v(k,v)  WHERE k IN (${" + formattedAssetType + "});";
        return genExpectedPostgresVarSyntax(formattedMeasure + formattedAssetType, "", description, databaseID, query, 2, true, true);
    }

    private static String genExpectedPostgresVarSyntax(String title, String label, String description, String databaseID, String query, int displayOption, boolean multiVal, boolean includeAll) {
        return TemplateVariableTest.genExpectedCommonJsonBase(title, "", displayOption, multiVal, includeAll) +
                "\"label\": \"" + label + "\"," +
                "\"datasource\": {\"type\": \"postgres\", \"uid\": \"" + databaseID + "\"}," +
                "\"description\": \"" + description + "\"," +
                "\"definition\": \"" + query + "\"," +
                "\"query\": \"" + query + "\"" +
                ",\"regex\": \"\",\"sort\" : 0,\"type\": \"query\"}";
    }

    private static StringBuilder genValueQueryForListOfArrays(List<String[]> assetMeasureMap) {
        StringBuilder results = new StringBuilder();
        for (String[] asset : assetMeasureMap) {
            // Append comma before if it is not the only value
            if (results.length() != 0) results.append(", ");
            results.append("('").append(asset[0]).append("', '")
                    .append(asset[1]).append("')");
        }
        return results;
    }
}