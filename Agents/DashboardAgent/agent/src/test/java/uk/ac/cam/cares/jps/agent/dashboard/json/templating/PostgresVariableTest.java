package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class PostgresVariableTest {
    private static final String EXPECTED_ASSET_TYPE = "Fridge";
    private static final String EXPECTED_MEASURE = "Energy Consumption";
    private static final String DATABASE_ID = "nhsaf781rh";
    private static final List<String[]> ASSET_TS_COL_LIST = new ArrayList<>();
    private static final String[] TEST_SET1 = new String[]{"F1", "column1"};
    private static final String[] TEST_SET2 = new String[]{"F2", "column2"};
    private static final String[] TEST_SET3 = new String[]{"F3", "column3"};

    @BeforeAll
    static void genTestAssetMeasureList() {
        ASSET_TS_COL_LIST.add(TEST_SET1);
        ASSET_TS_COL_LIST.add(TEST_SET2);
        ASSET_TS_COL_LIST.add(TEST_SET3);
    }

    @Test
    void testConstruct() {
        // Construct the object through the alternate constructor
        PostgresVariable variable = new PostgresVariable(EXPECTED_MEASURE, EXPECTED_ASSET_TYPE, DATABASE_ID, ASSET_TS_COL_LIST);
        // Execute the method
        String result = variable.construct();
        // Test outputs
        assertEquals(genExpectedPostgresVarSyntax(EXPECTED_MEASURE, EXPECTED_ASSET_TYPE, DATABASE_ID, ASSET_TS_COL_LIST), result);
    }

    public static String genExpectedPostgresVarSyntax(String measure, String assetType, String databaseID, List<String[]> assetMeasureMap) {
        String formattedMeasure = measure.toLowerCase().replaceAll("\\s", "");
        String formattedAssetType = assetType.toLowerCase().replaceAll("\\s", "");
        StringBuilder intermediateStep = genValueQueryForListOfArrays(assetMeasureMap);
        StringBuilder results = new StringBuilder();
        results.append(TemplateVariableTest.genExpectedCommonJsonBase(formattedMeasure + formattedAssetType, 2))
                .append("\"datasource\": {\"type\": \"postgres\", \"uid\": \"").append(databaseID).append("\"},")
                .append("\"description\": \"A hidden template variable that displays the corresponding time series of ")
                .append(measure.toLowerCase()).append(" for ").append(assetType.toLowerCase()).append("\",")
                .append("\"definition\": \"SELECT k AS \\\"__text\\\", v AS \\\"__value\\\" FROM (values ").append(intermediateStep)
                .append(") AS v(k,v)  WHERE k IN (${").append(formattedAssetType).append("});\",")
                .append("\"query\": \"SELECT k AS \\\"__text\\\", v AS \\\"__value\\\" FROM (values ").append(intermediateStep)
                .append(") AS v(k,v)  WHERE k IN (${").append(formattedAssetType).append("});\",\"regex\": \"\",\"sort\" : 0,\"type\": \"query\"}");
        return results.toString();
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