package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Measure;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.UnitMapper;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.TransformationOptionsTest;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import static org.junit.jupiter.api.Assertions.*;

public class TimeSeriesChartTest {
    private static final String[] SAMPLE_THRESHOLDS = new String[2];
    private static final String SAMPLE_DATABASE_ID = "eaus17";
    private static final String THRESHOLD_MIN = "3";
    private static final String THRESHOLD_MAX = "8";
    private static final int SAMPLE_PANEL_X_POSITION = 1;
    private static final int SAMPLE_PANEL_Y_POSITION = 0;
    private static final int[] EXPECTED_GEOMETRY_POSITIONS = new int[]{TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};

    @BeforeAll
    static void setup() {
        SAMPLE_THRESHOLDS[0] = THRESHOLD_MIN;
        SAMPLE_THRESHOLDS[1] = THRESHOLD_MAX;
    }

    @Test
    void testConstruct_WithThresholds() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(false);
        // Construct the object
        TimeSeriesChart chart = new TimeSeriesChart(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, SAMPLE_THRESHOLDS);
        // Execute the method
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS, SAMPLE_THRESHOLDS), result);
    }

    @Test
    void testConstruct_NoThresholds() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(false);
        // Construct the object
        TimeSeriesChart chart = new TimeSeriesChart(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, new String[]{});
        // Execute the method
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS, new String[]{}), result);
    }

    @Test
    void testConstructNoUnit() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(false);
        // Construct the object
        TimeSeriesChart chart = new TimeSeriesChart(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, new String[]{});
        // Execute the method
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS, new String[]{}), result);
        assertEquals(sample.getUnit(), chart.getUnit());
    }

    @Test
    void testConstructWithUnit() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(true);
        // Construct the object
        TimeSeriesChart chart = new TimeSeriesChart(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, new String[]{});
        // Execute the method
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS, new String[]{}), result);
        assertEquals(sample.getUnit(), chart.getUnit());
    }

    @Test
    void testGetMeasure() {
        Measure sample = TestUtils.genSampleMeasure(false);
        // Construct the object
        TimeSeriesChart chart = new TimeSeriesChart(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, new String[]{});
        // Execute the method and verify result
        assertEquals(sample.getName(), chart.getMeasure());
    }

    public static String genExpectedResults(Measure measure, String group, String databaseId, int[] geometryPositions, String[] thresholds) {
        String measureName = measure.getName();
        String unit = measure.getUnit();
        String titleContent = StringHelper.addSpaceBetweenCapitalWords(measureName) + " of " + StringHelper.addSpaceBetweenCapitalWords(group);
        titleContent = unit == null ? titleContent : titleContent + " [" + unit + "]";
        String description = "A chart displaying the time series of " + measureName.toLowerCase() + " for " + group.toLowerCase();
        String expectedTransformations = "[" + TransformationOptionsTest.genExpectedOrganizeTransformation(measure.getTimeSeriesData(), "") + "]";
        String thresholdStyle = thresholds.length == 0 ? "off" : "area";
        String thresholdSteps = thresholds.length == 0 ? "" : "\"thresholds\":{\"mode\": \"absolute\", \"steps\": [" +
                "{\"color\":\"red\",\"value\":null},{\"color\":\"green\",\"value\":" + thresholds[0] + "}," +
                "{\"color\":\"red\",\"value\":" + thresholds[1] + "}]},";
        return "{" + TestUtils.genExpectedCommonDefaultGrafanaPanelJson(titleContent, description, "timeseries", expectedTransformations,
                databaseId, group, measure, geometryPositions, "") +
                "\"fieldConfig\": { " +
                "\"defaults\": {\"color\": {\"mode\": \"palette-classic\"}," +
                "\"custom\":{" + "\"axisCenteredZero\":false,\"axisColorMode\":\"text\"," +
                "\"axisLabel\":\"\",\"axisPlacement\":\"auto\", \"barAlignment\":0, \"drawStyle\":\"line\"," +
                "\"fillOpacity\":0,\"gradientMode\":\"none\"," +
                "\"hideFrom\":{\"legend\":false, \"tooltip\":false, \"viz\":false}," +
                "\"lineInterpolation\":\"linear\", \"lineWidth\":1, \"pointSize\":5," +
                "\"scaleDistribution\":{\"type\":\"linear\"}, \"showPoints\":\"auto\", \"spanNulls\":false," +
                "\"stacking\":{\"group\":\"A\", \"mode\":\"none\"}, \"thresholdsStyle\":{\"mode\":\"" + thresholdStyle + "\"}" +
                "}," +
                thresholdSteps +
                "\"mappings\": []," +
                "\"unit\":\"" + UnitMapper.getUnitSyntax(unit) + "\"" +
                "}," +
                "\"overrides\": []" +
                "}," +
                "\"options\":{" +
                "\"legend\":{\"calcs\": [], \"displayMode\":\"list\",\"placement\":\"bottom\",\"showLegend\":true}," +
                "\"tooltip\":{\"mode\":\"single\",\"sort\":\"none\"}" +
                "}" +
                "}";
    }
}