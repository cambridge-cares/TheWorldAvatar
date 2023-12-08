package uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.types.*;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class LayoutTemplateTest {
    private static Map<String, String> sampleDbConnectionIdMap;

    @BeforeAll
    static void genSampleData() {
        sampleDbConnectionIdMap = TestUtils.genSampleDatabaseConnectionMap();
    }

    @Test
    void testGenAssetLayoutTemplate() {
        // Prepare test setup
        Map<String, List<String[]>> assetMeasures = TestUtils.genSampleAssetMeasureMap().get(TestUtils.ASSET_TYPE_ONE);
        String expectedInput = genExpectedAssetLayoutJson(0, TestUtils.ASSET_TYPE_ONE, assetMeasures, sampleDbConnectionIdMap);
        // Execute method
        Queue<TemplatePanel[]> results = LayoutTemplate.genAssetLayoutTemplate(TestUtils.ASSET_TYPE_ONE, assetMeasures, sampleDbConnectionIdMap);
        // Verify number of results
        assertEquals(1, results.size());
        // Process results for testing
        StringBuilder jsonResult = new StringBuilder();
        int rowNumber = 0;
        while (!results.isEmpty()) {
            if (jsonResult.length() != 0) jsonResult.append(",");
            TemplatePanel[] panels = results.poll();
            String gaugePanelJson = panels[0].construct(TestUtils.CHART_HEIGHT, TestUtils.CHART_WIDTH, 0, rowNumber);
            String timeSeriesPanelJson = panels[1].construct(TestUtils.CHART_HEIGHT, TestUtils.CHART_WIDTH, TestUtils.CHART_WIDTH, rowNumber);
            jsonResult.append(gaugePanelJson).append(",").append(timeSeriesPanelJson);
            rowNumber++;
        }
        // Verify results
        assertEquals(expectedInput, jsonResult.toString());
    }

    @Test
    void testGenRoomLayoutTemplate_IncludeThresholds() {
        // Prepare test setup
        Map<String, List<String[]>> roomMeasures = TestUtils.genSampleRoomMeasureMap(true).get(StringHelper.ROOM_KEY);
        String expectedOutput = genExpectedRoomLayoutJson(0, TestUtils.genSampleRoomMeasureMap(true).get(StringHelper.ROOM_KEY), sampleDbConnectionIdMap);
        // Execute method
        Queue<TemplatePanel[]> results = LayoutTemplate.genRoomLayoutTemplate(roomMeasures, sampleDbConnectionIdMap);
        // Verify number of results
        assertEquals(2, results.size()); // Two room measures are available
        // Process results for testing
        StringBuilder jsonResult = new StringBuilder();
        int rowNumber = 0;
        while (!results.isEmpty()) {
            if (jsonResult.length() != 0) jsonResult.append(",");
            TemplatePanel[] panels = results.poll();
            String averageGaugePanelJson = panels[0].construct(TestUtils.CHART_HEIGHT, 4, 0, rowNumber);
            String gaugePanelJson = panels[1].construct(TestUtils.CHART_HEIGHT, 8, 4, rowNumber);
            String timeSeriesPanelJson = panels[2].construct(TestUtils.CHART_HEIGHT, TestUtils.CHART_WIDTH, TestUtils.CHART_WIDTH, rowNumber);
            jsonResult.append(averageGaugePanelJson).append(",").append(gaugePanelJson).append(",").append(timeSeriesPanelJson);
            rowNumber++;
        }
        // Verify results
        assertEquals(expectedOutput, jsonResult.toString());
    }

    @Test
    void testGenRoomLayoutTemplate_NoThresholds() {
        // Prepare test setup
        Map<String, List<String[]>> roomMeasures = TestUtils.genSampleRoomMeasureMap(false).get(StringHelper.ROOM_KEY);
        String expectedOutput = genExpectedRoomLayoutJson(0, TestUtils.genSampleRoomMeasureMap(false).get(StringHelper.ROOM_KEY), sampleDbConnectionIdMap);
        // Execute method
        Queue<TemplatePanel[]> results = LayoutTemplate.genRoomLayoutTemplate(roomMeasures, sampleDbConnectionIdMap);
        // Verify number of results
        assertEquals(2, results.size()); // Two room measures are available
        // Process results for testing
        StringBuilder jsonResult = new StringBuilder();
        int rowNumber = 0;
        while (!results.isEmpty()) {
            if (jsonResult.length() != 0) jsonResult.append(",");
            TemplatePanel[] panels = results.poll();
            String averageGaugePanelJson = panels[0].construct(TestUtils.CHART_HEIGHT, 4, 0, rowNumber);
            String gaugePanelJson = panels[1].construct(TestUtils.CHART_HEIGHT, 8, 4, rowNumber);
            String timeSeriesPanelJson = panels[2].construct(TestUtils.CHART_HEIGHT, TestUtils.CHART_WIDTH, TestUtils.CHART_WIDTH, rowNumber);
            jsonResult.append(averageGaugePanelJson).append(",").append(gaugePanelJson).append(",").append(timeSeriesPanelJson);
            rowNumber++;
        }
        // Verify results
        assertEquals(expectedOutput, jsonResult.toString());
    }

    @Test
    void testGenSystemsLayoutTemplate() {
        // Prepare test setup
        Map<String, List<String[]>> systemMeasures = TestUtils.genSampleSystemMeasureMap().get(StringHelper.SYSTEM_KEY);
        String expectedOutput = genExpectedSystemLayoutJson(0, TestUtils.genSampleSystemMeasureMap().get(StringHelper.SYSTEM_KEY), sampleDbConnectionIdMap);
        // Execute method
        Queue<TemplatePanel[]> results = LayoutTemplate.genSystemsLayoutTemplate(systemMeasures, sampleDbConnectionIdMap);
        // Verify number of results
        assertEquals(2, results.size()); // Two system measures are available
        // Process results for testing
        StringBuilder jsonResult = new StringBuilder();
        int rowNumber = 0;
        while (!results.isEmpty()) {
            if (jsonResult.length() != 0) jsonResult.append(",");
            TemplatePanel[] panels = results.poll();
            String averageGaugePanelJson = panels[0].construct(TestUtils.CHART_HEIGHT, 4, 0, rowNumber);
            String pieChartJson = panels[1].construct(TestUtils.CHART_HEIGHT, 8, 4, rowNumber);
            String barChartJson = panels[2].construct(TestUtils.CHART_HEIGHT, TestUtils.CHART_WIDTH, TestUtils.CHART_WIDTH, rowNumber);
            jsonResult.append(averageGaugePanelJson).append(",").append(pieChartJson).append(",").append(barChartJson);
            rowNumber++;
        }
        // Verify results
        assertEquals(expectedOutput, jsonResult.toString());
    }

    public static String genExpectedAssetLayoutJson(int rowNumber, String assetType, Map<String, List<String[]>> assetMeasures, Map<String, String> databaseConnectionMap) {
        StringBuilder builder = new StringBuilder();
        for (String measure : assetMeasures.keySet()) {
            if (!measure.equals(StringHelper.ASSET_KEY)) {
                // Add a comma if there are previous items
                if (builder.length() != 0) builder.append(",");
                String[] metadata = assetMeasures.get(measure).get(0);
                String[] expectedConfigItems = new String[]{measure, assetType, metadata[2], databaseConnectionMap.get(metadata[3]), metadata[4]};
                int[] expectedGeometryPosition = new int[]{TestUtils.CHART_HEIGHT, TestUtils.CHART_WIDTH, 0, rowNumber};
                // For the generic Gauge chart
                builder.append(GaugeTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, assetMeasures.get(measure)))
                        .append(",");
                // For the time series chart, only the x position will change
                expectedGeometryPosition[2] = TestUtils.CHART_WIDTH;
                builder.append(TimeSeriesChartTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, assetMeasures.get(measure)));
                rowNumber++;
            }
        }
        return builder.toString();
    }

    public static String genExpectedRoomLayoutJson(int rowNumber, Map<String, List<String[]>> roomMeasures, Map<String, String> databaseConnectionMap) {
        StringBuilder builder = new StringBuilder();
        // Initialise a threshold map if there are thresholds
        Map<String, String[]> thresholdMap = new HashMap<>();
        // If this item group is rooms and have thresholds available, process them
        if (roomMeasures.containsKey(StringHelper.THRESHOLD_KEY)) {
            // Retrieve the thresholds and process it for generating the charts
            List<String[]> thresholdList = roomMeasures.get(StringHelper.THRESHOLD_KEY);
            // Process them into a measure key with threshold values
            for (String[] threshold : thresholdList) {
                thresholdMap.put(threshold[0], new String[]{threshold[1], threshold[2]});
            }
        }
        for (String measure : roomMeasures.keySet()) {
            if (!measure.equals(StringHelper.ROOM_KEY) && !measure.equals(StringHelper.THRESHOLD_KEY)) {
                // Sort the metadata beforehand to ensure column values are in order, as they are presorted in the agent code
                List<String[]> sortedList = roomMeasures.get(measure);
                Collections.sort(sortedList, Comparator.comparing(metadata -> metadata[0]));
                roomMeasures.put(measure, sortedList);
                // Add a comma if there are previous items
                if (builder.length() != 0) builder.append(",");
                String[] metadata = roomMeasures.get(measure).get(0);
                String[] thresholds = thresholdMap.containsKey(measure) ? thresholdMap.get(measure) : new String[]{};
                String[] expectedConfigItems = new String[]{measure, StringHelper.ROOM_KEY, metadata[2], databaseConnectionMap.get(metadata[3]), metadata[4]};
                int[] expectedGeometryPosition = new int[]{TestUtils.CHART_HEIGHT, 4, 0, rowNumber};
                // For the overall average Gauge chart
                String query = GaugeTest.genAggregateQuery(roomMeasures.get(measure), true);
                builder.append(GaugeTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, roomMeasures.get(measure), thresholds, query))
                        .append(",");
                // For the generic Gauge chart
                expectedGeometryPosition[1] = 8;  // New width
                expectedGeometryPosition[2] = 4;  // New x position
                builder.append(GaugeTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, roomMeasures.get(measure), thresholds))
                        .append(",");
                // For the time series chart
                expectedGeometryPosition[1] = TestUtils.CHART_WIDTH; // Original Width
                expectedGeometryPosition[2] = TestUtils.CHART_WIDTH; // New x position
                builder.append(TimeSeriesChartTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, roomMeasures.get(measure), thresholds));
                rowNumber++;
            }
        }
        return builder.toString();
    }

    public static String genExpectedSystemLayoutJson(int rowNumber, Map<String, List<String[]>> systemMeasures, Map<String, String> databaseConnectionMap) {
        StringBuilder builder = new StringBuilder();
        for (String measure : systemMeasures.keySet()) {
            if (!measure.equals(StringHelper.SYSTEM_KEY)) {
                // Add a comma if there are previous items
                if (builder.length() != 0) builder.append(",");
                String[] metadata = systemMeasures.get(measure).get(0);
                String[] expectedConfigItems = new String[]{measure, StringHelper.SYSTEM_KEY, metadata[2], databaseConnectionMap.get(metadata[3]), metadata[4]};
                int[] expectedGeometryPosition = new int[]{TestUtils.CHART_HEIGHT, TestUtils.CHART_WIDTH, 0, rowNumber};
                List<String[]> systemTimeSeries = systemMeasures.get(measure);
                Collections.sort(systemTimeSeries, Comparator.comparing(data -> data[1]));
                // For the overall average Gauge chart
                expectedGeometryPosition[1] = 4; // New width
                String query = GaugeTest.genAggregateQuery(systemMeasures.get(measure), false);
                builder.append(GaugeTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, systemTimeSeries,
                                new String[]{}, query))
                        .append(",");
                // For the pie chart
                expectedGeometryPosition[1] = 8;  // New width
                expectedGeometryPosition[2] = 4;  // New x position
                builder.append(PieChartTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, systemTimeSeries))
                        .append(",");
                // For the bar chart
                expectedGeometryPosition[1] = TestUtils.CHART_WIDTH; // Original Width
                expectedGeometryPosition[2] = TestUtils.CHART_WIDTH; // New x position
                builder.append(BarChartTest.genExpectedResults(expectedConfigItems, expectedGeometryPosition, systemTimeSeries));
                rowNumber++;
            }
        }
        return builder.toString();
    }
}