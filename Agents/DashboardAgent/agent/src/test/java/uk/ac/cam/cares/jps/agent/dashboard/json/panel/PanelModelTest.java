package uk.ac.cam.cares.jps.agent.dashboard.json.panel;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.LayoutTemplateTest;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.types.GaugeTest;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.types.TimeSeriesChartTest;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class PanelModelTest {
    private static Map<String, String> SAMPLE_DB_CONNECTION_ID_MAP;
    private static Map<String, Map<String, List<String[]>>> SAMPLE_ASSETS;
    private static Map<String, Map<String, List<String[]>>> SAMPLE_ROOMS;
    // A global counter will ensure value can be increased even in nested methods
    private static int ROW_NUMBER;

    @BeforeAll
    static void genSampleData() {
        SAMPLE_ASSETS = TestUtils.genSampleAssetMeasureMap();
        SAMPLE_DB_CONNECTION_ID_MAP = TestUtils.genSampleDatabaseConnectionMap();
    }

    @BeforeEach
    void genModifiableSampleData() {
        // Sample rooms will be removed during the syntax construction
        SAMPLE_ROOMS = TestUtils.genSampleRoomMeasureMap(false);
    }

    @Test
    void testConstruct_AssetsOnly() {
        // Construct and execute the method
        String result = new PanelModel(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ASSETS).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ASSETS), result);
    }

    @Test
    void testConstruct_RoomsOnlyNoThresholds() {
        // Construct and execute the method
        String result = new PanelModel(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ROOMS).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleRoomMeasureMap(false)), result);
    }

    @Test
    void testConstruct_RoomsOnlyThresholds() {
        // This test only include thresholds for one of the room measures as thresholds are not mandatory
        // Construct and execute the method
        String result = new PanelModel(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleRoomMeasureMap(true)).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleRoomMeasureMap(true)), result);
    }

    @Test
    void testConstruct_SystemsOnly() {
        // This test only include thresholds for one of the room measures as thresholds are not mandatory
        // Construct and execute the method
        String result = new PanelModel(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleSystemMeasureMap()).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleSystemMeasureMap()), result);
    }

    @Test
    void testConstruct_AssetsAndRoomsNoThresholds() {
        // Construct and execute the method
        String result = new PanelModel(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(false)).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(false)), result);
    }

    @Test
    void testConstruct_AssetsAndRoomsWithThreshold() {
        // This test only include thresholds for one of the room measures as thresholds are not mandatory
        // Construct and execute the method
        String result = new PanelModel(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(true)).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(true)), result);
    }


    public static String genExpectedRowSyntax(Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> items) {
        if (items.isEmpty()) return "";
        // Initialise the settings
        StringBuilder builder = new StringBuilder();
        ROW_NUMBER = 0;
        // For the rooms
        if (items.containsKey(StringHelper.ROOM_KEY)) {
            Map<String, List<String[]>> roomMetadata = items.get(StringHelper.ROOM_KEY);
            // Initialise a new threshold list to be placed later
            List<String[]> thresholdList = new ArrayList<>();
            // If there is a threshold key, retrieve the list and remove it
            if (roomMetadata.containsKey(StringHelper.THRESHOLD_KEY)) {
                // Retrieve the thresholds and process it into a measure key with threshold values
                thresholdList = roomMetadata.get(StringHelper.THRESHOLD_KEY);
                roomMetadata.remove(StringHelper.THRESHOLD_KEY);
            }
            // Each row should correspond to one measure
            for (String roomMeasure : roomMetadata.keySet()) {
                // Ensure that the rooms key are not processed
                if (!roomMeasure.equals(StringHelper.ROOM_KEY)) {
                    // Create a new map which should only include the current measure and thresholds to generate the expected syntax
                    Map<String, List<String[]>> expectedIndividualMap = new HashMap<>();
                    expectedIndividualMap.put(roomMeasure, roomMetadata.get(roomMeasure));
                    // If the threshold list is not empty, add it in
                    if (!thresholdList.isEmpty()) expectedIndividualMap.put(StringHelper.THRESHOLD_KEY, thresholdList);
                    // Create a title depending on if a unit exists
                    String title = StringHelper.addSpaceBetweenCapitalWords(roomMeasure);
                    title = roomMetadata.get(roomMeasure).get(0)[4].equals("null") ? title : title + "[" + roomMetadata.get(roomMeasure).get(0)[4] + "]";
                    // Append a comma if this is not the first row
                    if (builder.length() != 0) builder.append(",");
                    // General row syntax for rooms
                    builder.append("{\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                            .append("\"title\": \"").append(title).append("\",")
                            .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(TestUtils.CHART_WIDTH * 2)
                            // The row header should start at 0, and then increment the number after
                            .append(",\"x\": 0,\"y\": ").append(ROW_NUMBER++).append("},")
                            // Row contents should start at +1 from the header
                            .append("\"panels\": [").append(LayoutTemplateTest.genExpectedRoomLayoutJson(ROW_NUMBER, expectedIndividualMap, databaseConnectionMap))
                            .append("]}");
                }
            }
            items.remove(StringHelper.ROOM_KEY);
        }
        // Generate one row for the system
        if (items.containsKey(StringHelper.SYSTEM_KEY)) {
            if (builder.length() != 0) builder.append(",");
            Map<String, List<String[]>> systemMetadata = items.get(StringHelper.SYSTEM_KEY);
            String title = "Smart Meter";
            // General row syntax for system
            builder.append("{\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                    .append("\"title\": \"").append(title).append("\",")
                    .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(TestUtils.CHART_WIDTH * 2)
                    // The row header should start on a separate position (-1) before the row contents
                    .append(",\"x\": 0,\"y\": ").append(ROW_NUMBER).append("},")
                    .append("\"panels\": [").append(LayoutTemplateTest.genExpectedSystemLayoutJson(++ROW_NUMBER, systemMetadata, databaseConnectionMap))
                    .append("]}");
            items.remove(StringHelper.SYSTEM_KEY);
        }
        // A new row for each asset should be generated
        for (String assetType : items.keySet()) {
            if (builder.length() != 0) builder.append(",");
            Map<String, List<String[]>> assetMetadata = items.get(assetType);
            String title = StringHelper.addSpaceBetweenCapitalWords(assetType);
            // General row syntax for assets
            builder.append("{\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                    .append("\"title\": \"").append(title).append("\",")
                    .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(TestUtils.CHART_WIDTH * 2)
                    // The row header should start on a separate position (-1) before the row contents
                    .append(",\"x\": 0,\"y\": ").append(ROW_NUMBER).append("},")
                    .append("\"panels\": [").append(LayoutTemplateTest.genExpectedAssetLayoutJson(++ROW_NUMBER, assetType, assetMetadata, databaseConnectionMap))
                    .append("]}");
        }
        return builder.toString();
    }

    private static String genExpectedPanelsSyntax(int rowNumber, String itemType, Map<String, String> databaseConnectionMap, Map<String, List<String[]>> itemMetadata, String[] thresholds) {
        StringBuilder builder = new StringBuilder();
        for (String measure : itemMetadata.keySet()) {
            if (!measure.equals(StringHelper.ASSET_KEY) && !measure.equals(StringHelper.ROOM_KEY) && !measure.equals(StringHelper.SYSTEM_KEY)) {
                if (builder.length() != 0) builder.append(",");
                String[] metadata = itemMetadata.get(measure).get(0);
                String[] expectedConfigItems = new String[]{measure, itemType, metadata[2], databaseConnectionMap.get(metadata[3]), metadata[4]};
                int[] expectedGeometryPosition = new int[]{TestUtils.CHART_HEIGHT, TestUtils.CHART_WIDTH, 0, rowNumber + 1};
                if (itemType.equals(StringHelper.ROOM_KEY) || itemType.equals(StringHelper.SYSTEM_KEY)) {
                    // For the overall average Gauge chart
                    expectedGeometryPosition[1] = 4; // New width
                    String query = itemType.equals(StringHelper.ROOM_KEY) ? GaugeTest.genAggregateQuery(itemMetadata.get(measure), true) : GaugeTest.genAggregateQuery(itemMetadata.get(measure), false);
                    builder.append(GaugeTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, itemMetadata.get(measure),
                                    thresholds, query))
                            .append(",");
                    // For the generic Gauge chart
                    expectedGeometryPosition[1] = 8;  // New width
                    expectedGeometryPosition[2] = 4;  // New x position
                    builder.append(GaugeTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, itemMetadata.get(measure), thresholds))
                            .append(",");
                    // For the time series chart
                    expectedGeometryPosition[1] = TestUtils.CHART_WIDTH; // Original Width
                    expectedGeometryPosition[2] = TestUtils.CHART_WIDTH; // New x position
                    builder.append(TimeSeriesChartTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, itemMetadata.get(measure), thresholds));
                } else {
                    // For the generic Gauge chart
                    builder.append(GaugeTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, itemMetadata.get(measure)))
                            .append(",");
                    // For the time series chart, only the x position will change
                    expectedGeometryPosition[2] = TestUtils.CHART_WIDTH;
                    builder.append(TimeSeriesChartTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, itemMetadata.get(measure)));
                }
            }
        }
        return builder.toString();
    }
}