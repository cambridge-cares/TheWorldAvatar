package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

class PostgresVariableTest {
    private static ArrayDeque<String[]> FACILITY_ITEM_MAPPING;
    private static final String EXPECTED_ASSET_TYPE = "Fridge";
    private static final String EXPECTED_MEASURE = "Energy Consumption";
    private static final String EXPECTED_VARIABLE = "Reference Month";
    private static final String DATABASE_ID = "nhsaf781rh";
    private static final String FACILITY_ONE = "Home";
    private static final String FACILITY_ONE_ASSET = "Bed";
    private static final String FACILITY_TWO = "Bakery";
    private static final String FACILITY_TWO_ASSET = "Oven";
    private static final String FACILITY_TWO_ROOM = "Kitchen";
    private static final ArrayDeque<String[]> ASSET_TS_COL_LIST = new ArrayDeque<>();
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
        ASSET_TS_COL_LIST.offer(TEST_SET1);
        ASSET_TS_COL_LIST.offer(TEST_SET2);
        ASSET_TS_COL_LIST.offer(TEST_SET3);
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
        FACILITY_ITEM_MAPPING = new ArrayDeque<>();
        FACILITY_ITEM_MAPPING.offer(new String[]{FACILITY_ONE, FACILITY_ONE_ASSET});
        FACILITY_ITEM_MAPPING.offer(new String[]{FACILITY_TWO, FACILITY_TWO_ASSET});
        // Construct the object through the alternate constructor
        PostgresVariable variable = new PostgresVariable(EXPECTED_ASSET_TYPE, FACILITY_ITEM_MAPPING.clone(), DATABASE_ID);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedPostgresVarSyntaxForItemFilter(EXPECTED_ASSET_TYPE, DATABASE_ID, FACILITY_ITEM_MAPPING), result);
    }

    @Test
    void testConstruct_ItemFilterForRoom() {
        FACILITY_ITEM_MAPPING = new ArrayDeque<>();
        FACILITY_ITEM_MAPPING.offer(new String[]{FACILITY_TWO, FACILITY_TWO_ROOM});
        // Construct the object through the alternate constructor
        PostgresVariable variable = new PostgresVariable(StringHelper.ROOM_KEY, FACILITY_ITEM_MAPPING.clone(), DATABASE_ID);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedPostgresVarSyntaxForItemFilter(StringHelper.ROOM_KEY, DATABASE_ID, FACILITY_ITEM_MAPPING), result);
    }

    @Test
    void testConstruct_ItemFilterForSystem() {
        FACILITY_ITEM_MAPPING = new ArrayDeque<>();
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
        PostgresVariable variable = new PostgresVariable(EXPECTED_MEASURE, EXPECTED_ASSET_TYPE, ASSET_TS_COL_LIST.clone(), DATABASE_ID);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedPostgresVarSyntaxForMeasureFilter(EXPECTED_MEASURE, EXPECTED_ASSET_TYPE, ASSET_TS_COL_LIST.clone(), DATABASE_ID), result);
    }

    public static String genExpectedPostgresVarSyntaxForGenericFilter(String varName, String databaseID, Map<String, String> keyValuePairs) {
        String formattedName = varName.toLowerCase().replaceAll("\\s", "");
        String description = "A hidden filter that displays the " + StringHelper.addSpaceBetweenCapitalWords(varName).toLowerCase()
                + " as requested by the user";
        StringBuilder temp = new StringBuilder();
        List<Map.Entry<String, String>> sortedEntries = keyValuePairs.entrySet()
                .stream()
                .sorted(Comparator.comparingInt(entry -> Integer.parseInt(entry.getValue())))
                .collect(Collectors.toList());
        for (Map.Entry<String, String> entry : sortedEntries) {
            // Only append a comma at the start if it is not the first value
            if (temp.length() != 0) temp.append(", ");
            // Append the name and the corresponding column name
            temp.append("('").append(entry.getKey()).append("', '")
                    .append(entry.getValue()).append("')");
        }
        String queryTemplate = "SELECT k AS \\\"__text\\\", v AS \\\"__value\\\" FROM (values %s) AS v(k,v);";
        String query = String.format(queryTemplate, temp);
        return genExpectedPostgresVarSyntax(formattedName, varName, description, databaseID, query, 2, false, false);
    }

    public static String genExpectedPostgresVarSyntaxForItemFilter(String itemType, String databaseID, Queue<String[]> facilityItemMapping) {
        String formattedItemType = itemType.toLowerCase().replaceAll("\\s", "");
        String label = itemType.equals(StringHelper.ROOM_KEY) ? "Rooms" :
                itemType.equals(StringHelper.SYSTEM_KEY) ? "Smart Meter" : StringHelper.addSpaceBetweenCapitalWords(itemType);
        String description = "A filter for the items of " + label.toLowerCase() + " type.";
        List<String[]> parsedMappings = new ArrayList<>();
        // Flip the value positions as the genValueQueryForListOfArrays method requires these positions instead
        while (!facilityItemMapping.isEmpty()) {
            String[] facilityItem = facilityItemMapping.poll();
            parsedMappings.add(new String[]{facilityItem[1], facilityItem[0]});
        }
        String queryTemplate = "SELECT v AS \\\"__value\\\" FROM (values %s) AS v(k,v) WHERE k IN (${%s});";
        String query = String.format(queryTemplate, genValueQueryForListOfArrays(parsedMappings), StringHelper.FACILITY_KEY);
        return genExpectedPostgresVarSyntax(formattedItemType, label, description, databaseID, query, 0, true, true);
    }

    public static String genExpectedPostgresVarSyntaxForMeasureFilter(String measure, String assetType, Queue<String[]> assetMeasureMap, String databaseID) {
        String formattedMeasure = measure.toLowerCase().replaceAll("\\s", "");
        String formattedAssetType = assetType.toLowerCase().replaceAll("\\s", "");
        String description = "A hidden filter that displays the corresponding time series of " + StringHelper.addSpaceBetweenCapitalWords(measure).toLowerCase()
                + " for " + StringHelper.addSpaceBetweenCapitalWords(assetType).toLowerCase();
        String queryTemplate = "SELECT v AS \\\"__value\\\" FROM (values %s) AS v(k,v) WHERE k IN (${%s});";
        String query = String.format(queryTemplate, genValueQueryForListOfArrays(new ArrayList<>(assetMeasureMap)), formattedAssetType);
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

    private static StringBuilder genValueQueryForListOfArrays(List<String[]> keyValueMap) {
        StringBuilder results = new StringBuilder();
        for (String[] keyValue : keyValueMap) {
            // Append comma before if it is not the only value
            if (results.length() != 0) results.append(", ");
            results.append("('").append(keyValue[0]).append("', '")
                    .append(keyValue[1]).append("')");
        }
        return results;
    }
}