package uk.ac.cam.cares.jps.agent.dashboard.json.panel;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class PanelModelTest {
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
    void testConstructForOnlyAssets() {
        // Construct and execute the method
        String result = new PanelModel(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ASSETS).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ASSETS), result);
    }

    @Test
    void testConstructForOnlyRoomsNoThresholds() {
        // Construct and execute the method
        String result = new PanelModel(SAMPLE_DB_CONNECTION_ID_MAP, SAMPLE_ROOMS).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleRoomMeasureMap(false)), result);
    }

    @Test
    void testConstructForOnlyRoomsThresholds() {
        // This test only include thresholds for one of the room measures as thresholds are not mandatory
        // Construct and execute the method
        String result = new PanelModel(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleRoomMeasureMap(true)).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleRoomMeasureMap(true)), result);
    }

    @Test
    void testConstructForBothAssetsAndRoomsNoThresholds() {
        // Construct and execute the method
        String result = new PanelModel(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(false)).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(false)), result);
    }

    @Test
    void testConstructForBothAssetsAndRoomsWithThreshold() {
        // This test only include thresholds for one of the room measures as thresholds are not mandatory
        // Construct and execute the method
        String result = new PanelModel(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(true)).construct();
        // Test outputs
        assertEquals(genExpectedRowSyntax(SAMPLE_DB_CONNECTION_ID_MAP, TestUtils.genSampleComplexMeasureMap(true)), result);
    }


    private static String genExpectedRowSyntax(Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> items) {
        // Initialise the settings
        StringBuilder builder = new StringBuilder();
        ROW_NUMBER = 0;
        // For the rooms
        if (items.containsKey(StringHelper.ROOM_KEY)) {
            Map<String, List<String[]>> roomMetadata = items.get(StringHelper.ROOM_KEY);
            // For non-rooms, thresholds should be empty
            Map<String, String[]> thresholdMap = new HashMap<>();
            // If there are thresholds available, process them
            if (roomMetadata.containsKey(StringHelper.THRESHOLD_KEY)) {
                // Retrieve the thresholds and process it into a measure key with threshold values
                List<String[]> thresholdList = roomMetadata.get(StringHelper.THRESHOLD_KEY);
                for (String[] threshold : thresholdList) {
                    thresholdMap.put(threshold[0], new String[]{threshold[1], threshold[2]});
                }
                roomMetadata.remove(StringHelper.THRESHOLD_KEY); // remove the threshold after processing
            }
            // Each row should correspond to one measure
            for (String roomMeasure : roomMetadata.keySet()) {
                // Ensure that the rooms key are not processed
                if (!roomMeasure.equals(StringHelper.ROOM_KEY)) {
                    // Create a new map which should only include the current measure to generate the expected syntax
                    Map<String, List<String[]>> expectedIndividualMap = new HashMap<>();
                    String[] thresholds = thresholdMap.containsKey(roomMeasure) ? thresholdMap.get(roomMeasure) : new String[]{};
                    // Sort list based on the names
                    List<String[]> sortedList = roomMetadata.get(roomMeasure);
                    Collections.sort(sortedList, Comparator.comparing(metadata -> metadata[0]));
                    expectedIndividualMap.put(roomMeasure, sortedList);
                    // Create a title depending on if a unit exists
                    String title = StringHelper.addSpaceBetweenCapitalWords(roomMeasure);
                    title = roomMetadata.get(roomMeasure).get(0)[4].equals("null") ? title : title + "[" + roomMetadata.get(roomMeasure).get(0)[4] + "]";
                    // Append a comma if this is not the first row
                    if (builder.length() != 0) builder.append(",");
                    // General row syntax for assets
                    builder.append("{\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                            .append("\"title\": \"").append(title).append("\",")
                            .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(TestUtils.CHART_WIDTH * 2)
                            .append(",\"x\": 0,\"y\": ").append(ROW_NUMBER).append("},")
                            // To generate the panels for assets
                            .append("\"panels\": [").append(genExpectedPanelsSyntax(ROW_NUMBER, StringHelper.ROOM_KEY, databaseConnectionMap, expectedIndividualMap, thresholds))
                            .append("]}");
                    ROW_NUMBER++;
                }
            }
            items.remove(StringHelper.ROOM_KEY);
        }
        // A new row for each asset item should be generated
        for (String assetType : items.keySet()) {
            if (builder.length() != 0) builder.append(",");
            Map<String, List<String[]>> assetMetadata = items.get(assetType);
            // General row syntax for assets
            builder.append("{\"id\":null, \"type\":\"row\", \"collapsed\":true,")
                    .append("\"title\": \"").append(StringHelper.addSpaceBetweenCapitalWords(assetType)).append("\",")
                    .append(" \"gridPos\": {\"h\": 1,\"w\": ").append(TestUtils.CHART_WIDTH * 2)
                    .append(",\"x\": 0,\"y\": ").append(ROW_NUMBER).append("},")
                    // To generate the panels for assets
                    .append("\"panels\": [").append(genExpectedPanelsSyntax(ROW_NUMBER, assetType, databaseConnectionMap, assetMetadata, new String[]{}))
                    .append("]}");
            ROW_NUMBER++;
        }
        return builder.toString();
    }

    private static String genExpectedPanelsSyntax(int rowNumber, String itemType, Map<String, String> databaseConnectionMap, Map<String, List<String[]>> itemMetadata, String[] thresholds) {
        StringBuilder builder = new StringBuilder();
        for (String measure : itemMetadata.keySet()) {
            if (!measure.equals(StringHelper.ASSET_KEY) && !measure.equals(StringHelper.ROOM_KEY)) {
                if (builder.length() != 0) builder.append(",");
                String[] metadata = itemMetadata.get(measure).get(0);
                String[] expectedConfigItems = new String[]{measure, itemType, metadata[2], databaseConnectionMap.get(metadata[3]), metadata[4]};
                int[] expectedGeometryPosition = new int[]{TestUtils.CHART_HEIGHT, TestUtils.CHART_WIDTH, 0, rowNumber + 1};
                if (itemType.equals(StringHelper.ROOM_KEY)) {
                    // For the overall average Gauge chart
                    expectedGeometryPosition[1] = 4; // New width
                    builder.append(GaugeTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, itemMetadata.get(measure),
                                    thresholds, GaugeTest.genAverageQuery(itemMetadata.get(measure))))
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