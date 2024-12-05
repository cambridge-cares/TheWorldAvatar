package uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Measure;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Organisation;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Threshold;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.types.*;
import uk.ac.cam.cares.jps.agent.dashboard.stack.SparqlClientTest;
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
        Organisation organisation = TestUtils.genSampleAssetMeasures(null);
        Queue<String> expectedOutputs = genExpectedAssetLayoutJson(0, SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_TYPE, sampleDbConnectionIdMap, organisation);
        StringBuilder expectedJson = new StringBuilder();
        while (!expectedOutputs.isEmpty()) {
            if (expectedJson.length() != 0) expectedJson.append(",");
            expectedJson.append((expectedOutputs.poll()));
        }
        // Execute method
        Queue<TemplatePanel[]> results = LayoutTemplate.genAssetLayoutTemplate(SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_TYPE, organisation, sampleDbConnectionIdMap);
        // Verify number of results
        assertEquals(1, results.size());
        // Process results for testing
        StringBuilder jsonResult = new StringBuilder();
        int rowNumber = 0;
        while (!results.isEmpty()) {
            if (jsonResult.length() != 0) jsonResult.append(",");
            TemplatePanel[] panels = results.poll();
            String gaugePanelJson = panels[0].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, 0, rowNumber);
            String timeSeriesPanelJson = panels[1].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, TestUtils.ROW_WITH_TWO_CHART_WIDTH, rowNumber);
            jsonResult.append(gaugePanelJson).append(",").append(timeSeriesPanelJson);
            rowNumber++;
        }
        // Verify results
        assertEquals(expectedJson.toString(), jsonResult.toString());
    }

    @Test
    void testGenRoomLayoutTemplate_IncludeThresholds() {
        // Prepare test setup
        Organisation organisation = TestUtils.genSampleRoomMeasures(null, true);
        Queue<String> expectedOutputs = genExpectedRoomLayoutJson(0, sampleDbConnectionIdMap, organisation);
        // Execute method
        Queue<TemplatePanel[]> results = LayoutTemplate.genRoomLayoutTemplate(organisation, sampleDbConnectionIdMap);
        // Verify number of results
        assertEquals(expectedOutputs.size(), results.size()); // Two room measures are available
        // Process results for testing
        StringBuilder jsonResult = new StringBuilder();
        StringBuilder expectedJson = new StringBuilder();
        int rowNumber = 0;
        while (!results.isEmpty()) {
            if (jsonResult.length() != 0) jsonResult.append(",");
            TemplatePanel[] panels = results.poll();
            String averageGaugePanelJson = panels[0].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH, 0, rowNumber);
            String gaugePanelJson = panels[1].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH, TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH, rowNumber);
            String timeSeriesPanelJson = panels[2].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH, TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH + TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH, rowNumber);
            jsonResult.append(averageGaugePanelJson).append(",").append(gaugePanelJson).append(",").append(timeSeriesPanelJson);
            rowNumber++;
            if (expectedJson.length() != 0) expectedJson.append(",");
            expectedJson.append(expectedOutputs.poll());
        }
        // Verify results
        assertEquals(expectedJson.toString(), jsonResult.toString());
    }

    @Test
    void testGenRoomLayoutTemplate_NoThresholds() {
        // Prepare test setup
        Organisation organisation = TestUtils.genSampleRoomMeasures(null, false);
        Queue<String> expectedOutputs = genExpectedRoomLayoutJson(0, sampleDbConnectionIdMap, organisation);
        // Execute method
        Queue<TemplatePanel[]> results = LayoutTemplate.genRoomLayoutTemplate(organisation, sampleDbConnectionIdMap);
        // Verify number of results
        assertEquals(expectedOutputs.size(), results.size()); // Two room measures are available
        // Process results for testing
        StringBuilder jsonResult = new StringBuilder();
        StringBuilder expectedJson = new StringBuilder();
        int rowNumber = 0;
        while (!results.isEmpty()) {
            if (jsonResult.length() != 0) jsonResult.append(",");
            TemplatePanel[] panels = results.poll();
            String averageGaugePanelJson = panels[0].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH, 0, rowNumber);
            String gaugePanelJson = panels[1].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH, TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH, rowNumber);
            String timeSeriesPanelJson = panels[2].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH, TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH + TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH, rowNumber);
            jsonResult.append(averageGaugePanelJson).append(",").append(gaugePanelJson).append(",").append(timeSeriesPanelJson);
            rowNumber++;
            if (expectedJson.length() != 0) expectedJson.append(",");
            expectedJson.append(expectedOutputs.poll());
        }
        // Verify results
        assertEquals(expectedJson.toString(), jsonResult.toString());
    }

    @Test
    void testGenSystemsLayoutTemplate() {
        // Prepare test setup
        Organisation organisation = TestUtils.genSampleSystemMeasures(null);
        Queue<String> expectedOutputs = genExpectedSystemLayoutJson(0, sampleDbConnectionIdMap, organisation);
        StringBuilder expectedJson = new StringBuilder();
        while (!expectedOutputs.isEmpty()) {
            if (expectedJson.length() != 0) expectedJson.append(",");
            expectedJson.append((expectedOutputs.poll()));
        }
        // Execute method
        Queue<TemplatePanel[]> results = LayoutTemplate.genSystemsLayoutTemplate(organisation, sampleDbConnectionIdMap);
        // Verify number of results
        assertEquals(3, results.size()); // Two system measures are available with three sets of array each
        // Process results for testing
        StringBuilder jsonResult = new StringBuilder();
        int rowNumber = 0;
        while (!results.isEmpty()) {
            if (jsonResult.length() != 0) jsonResult.append(",");
            TemplatePanel[] panels = results.poll();
            if (panels.length % 2 == 0) {
                String pieChartJson = panels[0].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, 0, rowNumber);
                String currentMonthChartJson = panels[1].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, TestUtils.ROW_WITH_TWO_CHART_WIDTH, rowNumber);
                jsonResult.append(pieChartJson).append(",").append(currentMonthChartJson);
            } else {
                String VariablePanelJson = panels[0].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH, 0, rowNumber);
                String lastPeriodChartJson = panels[1].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH, TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH, rowNumber);
                String currentPeriodChartJson = panels[2].construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH, TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH + TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH, rowNumber);
                jsonResult.append(VariablePanelJson).append(",").append(lastPeriodChartJson).append(",").append(currentPeriodChartJson);
            }
            rowNumber++;
        }
        // Verify results
        assertEquals(expectedJson.toString(), jsonResult.toString());
    }

    public static Queue<String> genExpectedAssetLayoutJson(int rowNumber, String assetType, Map<String, String> databaseConnectionMap, Organisation organisation) {
        Queue<String> assetPanels = new ArrayDeque<>();
        List<String> measures = new ArrayList<>(organisation.getAllMeasureNames(assetType));
        for (String measure : measures) {
            StringBuilder builder = new StringBuilder();
            Measure currentMeasure = organisation.getMeasure(assetType, measure);
            String databaseID = databaseConnectionMap.get(currentMeasure.getTimeSeriesDatabase());
            int[] expectedGeometryPosition = new int[]{TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, 0, rowNumber};
            // For the generic Gauge chart
            builder.append(GaugeTest.genExpectedResults(currentMeasure, assetType, databaseID, expectedGeometryPosition, new String[]{}, ""))
                    .append(",");
            // For the time series chart, only the x position will change
            expectedGeometryPosition[2] = TestUtils.ROW_WITH_TWO_CHART_WIDTH;
            builder.append(TimeSeriesChartTest.genExpectedResults(currentMeasure, assetType, databaseID, expectedGeometryPosition, new String[]{}));
            rowNumber++;
            assetPanels.offer(builder.toString());
        }
        return assetPanels;
    }

    public static Queue<String> genExpectedRoomLayoutJson(int rowNumber, Map<String, String> databaseConnectionMap, Organisation organisation) {
        Queue<String> roomPanels = new ArrayDeque<>();
        List<String> measures = new ArrayList<>(organisation.getAllMeasureNames(StringHelper.ROOM_KEY));
        Threshold thresholdModel = organisation.getThresholds(StringHelper.ROOM_KEY).poll();
        for (String measure : measures) {
            StringBuilder builder = new StringBuilder();
            Measure currentMeasure = organisation.getMeasure(StringHelper.ROOM_KEY, measure);
            String databaseID = databaseConnectionMap.get(currentMeasure.getTimeSeriesDatabase());
            int[] expectedGeometryPosition = new int[]{TestUtils.CHART_HEIGHT, TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH, 0, rowNumber};
            String[] thresholds = thresholdModel == null ? new String[]{} : thresholdModel.getThreshold(measure);
            // For the overall average Gauge chart
            String query = GaugeTest.genAggregateQuery(currentMeasure, true);
            builder.append(GaugeTest.genExpectedResults(currentMeasure, StringHelper.ROOM_KEY, databaseID, expectedGeometryPosition, thresholds, query))
                    .append(",");
            // For the generic Gauge chart
            expectedGeometryPosition[1] = TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH;  // new chart width
            expectedGeometryPosition[2] = TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH; // new x position
            builder.append(GaugeTest.genExpectedResults(currentMeasure, StringHelper.ROOM_KEY, databaseID, expectedGeometryPosition, thresholds, ""))
                    .append(",");
            // For the time series chart, new x position
            expectedGeometryPosition[2] = TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH + TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH;
            builder.append(TimeSeriesChartTest.genExpectedResults(currentMeasure, StringHelper.ROOM_KEY, databaseID, expectedGeometryPosition, thresholds));
            rowNumber++;
            roomPanels.offer(builder.toString());
        }
        return roomPanels;
    }

    public static Queue<String> genExpectedSystemLayoutJson(int rowNumber, Map<String, String> databaseConnectionMap, Organisation organisation) {
        Queue<String> systemPanels = new ArrayDeque<>();
        List<String> measures = new ArrayList<>(organisation.getAllMeasureNames(StringHelper.SYSTEM_KEY));
        for (String measure : measures) {
            StringBuilder builder = new StringBuilder();
            Measure currentMeasure = organisation.getMeasure(StringHelper.SYSTEM_KEY, measure);
            String databaseID = databaseConnectionMap.get(currentMeasure.getTimeSeriesDatabase());
            int[] expectedGeometryPosition = new int[]{TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, 0, rowNumber};
            // Charts will be positioned as follows: Row1: Pie Chart, Current Month Bar; Row 2: Time Interval Variable Panel, Last Period Bar Chart, Current Period Bar Chart
            builder.append(PieChartTest.genExpectedResults(currentMeasure, StringHelper.SYSTEM_KEY, databaseID, expectedGeometryPosition))
                    .append(",");
            // Current month bar will be found at the chart_width x position
            expectedGeometryPosition[2] = TestUtils.ROW_WITH_TWO_CHART_WIDTH;
            builder.append(BarChartTest.genExpectedResults(currentMeasure, StringHelper.SYSTEM_KEY, databaseID, expectedGeometryPosition, 1))
                    .append(",");
            // Increment the row number as the system should generate 2 rows in total
            expectedGeometryPosition[3] = rowNumber + 1;
            // Reassign x position and chart width for variable panel
            expectedGeometryPosition[1] = TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH;
            expectedGeometryPosition[2] = 0;
            builder.append(VariablePanelTest.genExpectedResults(StringHelper.INTERVAL_VARIABLE_NAME, VariablePanelTest.INTERVAL_DESCRIPTION, expectedGeometryPosition))
                    .append(",");
            // Reassign the x position and chart width for the last period bar chart
            expectedGeometryPosition[1] = TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH;
            expectedGeometryPosition[2] = TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH;
            builder.append(BarChartTest.genExpectedResults(currentMeasure, StringHelper.SYSTEM_KEY, databaseID, expectedGeometryPosition, 2))
                    .append(",");
            // Reassign the x position for the current period bar chart
            expectedGeometryPosition[2] = TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH + TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH;
            builder.append(BarChartTest.genExpectedResults(currentMeasure, StringHelper.SYSTEM_KEY, databaseID, expectedGeometryPosition, 3))
                    .append(",");
            // Increment the row number as the system should generate 3 rows in total
            expectedGeometryPosition[3] = rowNumber + 2;
            // Reassign x position and chart width for variable panel
            expectedGeometryPosition[1] = TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH;
            expectedGeometryPosition[2] = 0;
            builder.append(VariablePanelTest.genExpectedResults(StringHelper.REF_MONTH_VARIABLE_NAME, VariablePanelTest.REF_MONTH_DESCRIPTION, expectedGeometryPosition))
                    .append(",");
            // Reassign the x position and chart width for the daily comparison bar chart
            expectedGeometryPosition[1] = TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH;
            expectedGeometryPosition[2] = TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH;
            builder.append(BarChartTest.genExpectedResults(currentMeasure, StringHelper.SYSTEM_KEY, databaseID, expectedGeometryPosition, 4))
                    .append(",");
            // Reassign the x position for the weekly comparison bar chart
            expectedGeometryPosition[2] = TestUtils.ROW_OF_THREE_FIRST_CHART_WIDTH + TestUtils.ROW_OF_THREE_DUAL_CHART_WIDTH;
            builder.append(BarChartTest.genExpectedResults(currentMeasure, StringHelper.SYSTEM_KEY, databaseID, expectedGeometryPosition, 5));
            rowNumber +=3;
            systemPanels.offer(builder.toString());
        }
        return systemPanels;
    }
}