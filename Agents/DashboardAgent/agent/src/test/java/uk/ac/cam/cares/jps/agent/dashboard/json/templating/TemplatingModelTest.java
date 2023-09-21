package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class TemplatingModelTest {
    private static Map<String, String> SAMPLE_DB_CONNECTION_ID_MAP;
    private static Map<String, Map<String, List<String[]>>> SAMPLE_ASSETS;
    private static Map<String, Map<String, List<String[]>>> SAMPLE_ROOMS;


    @BeforeAll
    static void genSampleData() {
        SAMPLE_ASSETS = TestUtils.genSampleAssetMeasureMap();
        SAMPLE_DB_CONNECTION_ID_MAP = TestUtils.genSampleDatabaseConnectionMap();
        SAMPLE_ROOMS = TestUtils.genSampleRoomMeasureMap(true);
    }

    @Test
    void testConstruct_AssetsOnly() {
        // Construct and execute the method
        String result = new TemplatingModel(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ASSETS).construct();
        // Test outputs
        assertEquals(genExpectedJsonSyntax(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ASSETS), result);
    }

    @Test
    void testConstruct_RoomsOnly() {
        // Construct and execute the method
        String result = new TemplatingModel(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ROOMS).construct();
        // Test outputs
        assertEquals(genExpectedJsonSyntax(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleRoomMeasureMap(true)), result);
    }

    private static String genExpectedJsonSyntax(Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> items) {
        StringBuilder builder = new StringBuilder();
        StringBuilder tempBuilder = new StringBuilder();
        builder.append("{\"enable\": true,\"list\": [");
        for (String itemType : items.keySet()) {
            Map<String, List<String[]>> itemMeasures = items.get(itemType);
            if (tempBuilder.length() != 0) tempBuilder.append(",");
            String nestedKey = itemType.equals(StringHelper.ROOM_KEY) ? StringHelper.ROOM_KEY : StringHelper.ASSET_KEY;
            // Retrieve the list of assets in the asset key and flatten the list of arrays into one array
            tempBuilder.append(CustomVariableTest.genExpectedCustomVariableSyntax(itemType, itemMeasures.get(nestedKey).stream().flatMap(Stream::of).distinct().toArray(String[]::new), 0));
            for (String measure : itemMeasures.keySet()) {
                if (!measure.equals(StringHelper.ASSET_KEY) && !measure.equals(StringHelper.ROOM_KEY) && !measure.equals(StringHelper.THRESHOLD_KEY)) {
                    List<String[]> itemMeasureList = itemMeasures.get(measure);
                    String dbName = itemMeasureList.get(0)[3];
                    tempBuilder.append(",")
                            .append(PostgresVariableTest.genExpectedPostgresVarSyntax(measure, itemType, databaseConnectionMap.get(dbName), itemMeasureList));
                }
            }
        }
        builder.append(tempBuilder)
                .append("]}");
        return builder.toString();
    }
}