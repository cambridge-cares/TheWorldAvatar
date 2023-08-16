package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class TemplatingModelTest {
    private static Map<String, String> SAMPLE_DB_CONNECTION_ID_MAP;
    private static Map<String, Map<String, List<String[]>>> SAMPLE_ASSETS;

    @BeforeAll
    static void genSampleData() {
        SAMPLE_ASSETS = TestUtils.genSampleAssetMeasureMap();
        SAMPLE_DB_CONNECTION_ID_MAP = TestUtils.genSampleDatabaseConnectionMap();
    }

    @Test
    void testConstruct() {
        // Construct and execute the method
        String result = new TemplatingModel(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ASSETS).construct();
        // Test outputs
        assertEquals(genExpectedJsonSyntax(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ASSETS), result);
    }

    private static String genExpectedJsonSyntax(Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> assets) {
        StringBuilder builder = new StringBuilder();
        builder.append("{\"enable\": true,\"list\": [")
                .append(CustomVariableTest.genExpectedCustomVariableSyntax(TestUtils.ASSET_TYPE_TWO, new String[]{TestUtils.ASSET_OVEN}, 0))
                .append(",")
                .append(PostgresVariableTest.genExpectedPostgresVarSyntax(TestUtils.MEASURE_OVEN, TestUtils.ASSET_TYPE_TWO,
                        databaseConnectionMap.get(TestUtils.DATABASE_HEAT), assets.get(TestUtils.ASSET_TYPE_TWO).get(TestUtils.MEASURE_OVEN)))
                .append(",")
                .append(PostgresVariableTest.genExpectedPostgresVarSyntax(TestUtils.MEASURE_COMMON, TestUtils.ASSET_TYPE_TWO,
                        databaseConnectionMap.get(TestUtils.DATABASE_ELEC), assets.get(TestUtils.ASSET_TYPE_TWO).get(TestUtils.MEASURE_COMMON)))
                .append(",")
                .append(CustomVariableTest.genExpectedCustomVariableSyntax(TestUtils.ASSET_TYPE_ONE, new String[]{TestUtils.ASSET_LAMP_ONE, TestUtils.ASSET_LAMP_TWO}, 0))
                .append(",")
                .append(PostgresVariableTest.genExpectedPostgresVarSyntax(TestUtils.MEASURE_COMMON, TestUtils.ASSET_TYPE_ONE,
                        databaseConnectionMap.get(TestUtils.DATABASE_ELEC), assets.get(TestUtils.ASSET_TYPE_ONE).get(TestUtils.MEASURE_COMMON)))
                .append("]}");
        return builder.toString();
    }
}