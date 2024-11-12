package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Measure;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.UnitMapper;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.TransformationOptionsTest;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class PieChartTest {
    private static final String SAMPLE_DATABASE_ID = "3831j";
    private static final int SAMPLE_PANEL_X_POSITION = 1;
    private static final int SAMPLE_PANEL_Y_POSITION = 0;
    private static final int[] EXPECTED_GEOMETRY_POSITIONS = new int[]{TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};


    @Test
    void testConstructor() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(false);
        // Execute the method
        PieChart chart = new PieChart(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID);
        // Verify results
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS), result);
    }

    @Test
    void testConstruct_NoUnit() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(false);
        // Construct the object
        PieChart chart = new PieChart(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID);
        // Execute the method
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS), result);
        assertEquals(sample.getUnit(), chart.getUnit());
    }

    @Test
    void testConstruct_WithUnit() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(true);
        // Construct the object
        PieChart chart = new PieChart(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID);
        // Execute the method
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS), result);
        assertEquals(sample.getUnit(), chart.getUnit());
    }

    @Test
    void testGetMeasure() {
        Measure sample = TestUtils.genSampleMeasure(true);
        // Construct the object
        PieChart chart = new PieChart(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID);
        // Execute the method and verify result
        assertEquals(sample.getName(), chart.getMeasure());
    }

    public static String genExpectedResults(Measure measure, String group, String databaseId, int[] geometryPositions) {
        String measureName = measure.getName();
        String unit = measure.getUnit();
        String titleContent = "Latest " + StringHelper.addSpaceBetweenCapitalWords(measureName) + " distribution";
        titleContent = unit == null ? titleContent : titleContent + " [" + unit + "]";
        // Similar to above
        String description = "A pie chart displaying the latest distribution for " + measureName.toLowerCase() + " of " + group.toLowerCase();
        String expectedTransformations = "[" + TransformationOptionsTest.genExpectedOrganizeTransformation(measure.getTimeSeriesData(), "") + "]";
        // Construct the expected syntax
        return "{" + TestUtils.genExpectedCommonDefaultGrafanaPanelJson(titleContent, description, "piechart", expectedTransformations,
                databaseId, group, measure, geometryPositions, "") +
                "\"pluginVersion\":\"10.0.3\"," +
                "\"fieldConfig\": {" +
                "\"defaults\": {\"color\": {\"mode\": \"palette-classic\"}," +
                "\"custom\":{" + "\"hideFrom\":{\"legend\":false,\"tooltip\":false,\"viz\":false}" + "}," +
                "\"mappings\": []," +
                "\"unit\":\"" + UnitMapper.getUnitSyntax(unit) + "\"" +
                "}," +
                "\"overrides\": []" +
                "}," +
                // Options
                "\"options\":{" +
                "\"legend\":{\"displayMode\":\"list\",\"placement\":\"right\",\"showLegend\":true}," +
                "\"tooltip\":{\"mode\":\"single\",\"sort\":\"none\"}," +
                "\"displayLabels\":[\"percent\"]," +
                "\"pieType\":\"donut\"," +
                "\"reduceOptions\": {\"calcs\":[\"lastNotNull\"],\"fields\":\"\",\"values\":false}" +
                "}" + // end of options
                "}";
    }
}