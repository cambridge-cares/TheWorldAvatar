package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Measure;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.UnitMapper;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.TransformationOptionsTest;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.Queue;

import static org.junit.jupiter.api.Assertions.*;

public class GaugeTest {
    private static final String[] SAMPLE_THRESHOLDS = new String[2];
    private static final String SAMPLE_DATABASE_ID = "eaus17";
    private static final String THRESHOLD_MIN = "1";
    private static final String THRESHOLD_MAX = "5";
    private static final int SAMPLE_PANEL_X_POSITION = 1;
    private static final int SAMPLE_PANEL_Y_POSITION = 0;
    private static final int[] EXPECTED_GEOMETRY_POSITIONS = new int[]{TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};

    @BeforeAll
    static void setup() {
        SAMPLE_THRESHOLDS[0] = THRESHOLD_MIN;
        SAMPLE_THRESHOLDS[1] = THRESHOLD_MAX;
    }

    @Test
    void testConstructor() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(true);
        // Execute the method
        Gauge chart = new Gauge(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, SAMPLE_THRESHOLDS, true);
        // Verify results
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS, SAMPLE_THRESHOLDS, genAggregateQuery(sample, true)),
                result);
    }

    @Test
    void testConstructor_NoThresholds() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(true);
        // Execute the method
        Gauge chart = new Gauge(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, new String[]{}, true);
        // Verify results
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS, new String[]{}, genAggregateQuery(sample, true)),
                result);
    }

    @Test
    void testConstructor_MultipleThresholds() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(true);
        String[] thresholds =  new String[]{"0", "3", "6", "8"};
        // Execute the method
        Gauge chart = new Gauge(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, thresholds, true);
        // Verify results
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS, thresholds, genAggregateQuery(sample, true)),
                result);
    }

    @Test
    void testConstructor_NoOptionalValues() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(true);
        // Execute the method
        Gauge chart = new Gauge(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, new String[]{}, false);
        // Verify results
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS, new String[]{}, ""),
                result);
    }

    @Test
    void testConstruct_NoUnit() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(false);
        // Execute the method
        Gauge chart = new Gauge(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, new String[]{}, true);
        // Verify results
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS, new String[]{}, genAggregateQuery(sample, true)),
                result);
        assertEquals(sample.getUnit(), chart.getUnit());
    }

    @Test
    void testConstruct_WithUnit() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(true);
        // Execute the method
        Gauge chart = new Gauge(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, new String[]{}, true);
        // Verify results
        String result = chart.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS, new String[]{}, genAggregateQuery(sample, true)),
                result);
        assertEquals(sample.getUnit(), chart.getUnit());
    }

    @Test
    void testGetMeasure() {
        // Generate expected inputs
        Measure sample = TestUtils.genSampleMeasure(true);
        // Construct the object
        Gauge chart = new Gauge(sample, TestUtils.ASSET_TYPE_ONE, SAMPLE_DATABASE_ID, new String[]{}, true);
        // Execute the method and verify result
        assertEquals(sample.getName(), chart.getMeasure());
    }

    public static String genExpectedResults(Measure measure, String group, String databaseId, int[] geometryPositions, String[] thresholds, String query) {
        String measureName = measure.getName();
        String unit = measure.getUnit();
        String titleContent = "Latest " + StringHelper.addSpaceBetweenCapitalWords(measureName);
        titleContent = query.isEmpty() ?
                unit == null ? titleContent : titleContent + " [" + unit + "]"  // If query is empty, this is for generic gauges with no aggregate computation
                : query.contains(")/") ? "Latest Average" : "Latest Cumulative Total"; // If there is signs of an average computation, employ average title instead
        // Similar to above
        String description = query.isEmpty() ? "A gauge chart displaying the latest value of all individuals' "
                : query.contains(")/") ? "A gauge chart displaying the latest average value of " : "A gauge chart displaying the latest cumulative total value of ";
        description += query.isEmpty() ? measureName.toLowerCase() + " for " + group.toLowerCase() :
                measureName.toLowerCase() + " for all " + group.toLowerCase() + "; Do note that this value is constant at the dashboard level and unaffected by any filters for individual elements";
        String expectedTransformations = "[" + TransformationOptionsTest.genExpectedOrganizeTransformation(measure.getTimeSeriesData(), "") + "]";
        boolean showThresholdMarkers = false;
        String colorMode = "palette-classic";
        String colorSteps = "{\"color\":\"red\",\"value\":80}";
        String minMax = "";
       if (thresholds.length != 0) {
            showThresholdMarkers = true;
            colorMode = "thresholds";
            colorSteps = thresholds.length == 4 ? "{\"color\":\"green\",\"value\":null},{\"color\":\"yellow\",\"value\":" + thresholds[0] +
                    "},{\"color\":\"orange\",\"value\":" + thresholds[1] + "}" +
                    "},{\"color\":\"red\",\"value\":" + thresholds[2] + "}" +
                    "},{\"color\":\"dark-red\",\"value\":" + thresholds[3] + "}" :
                    // Else, color steps should be from min (green) to max (red) threshold
                    "{\"color\":\"green\",\"value\":" + thresholds[0] + "},{\"color\":\"red\",\"value\":" + thresholds[1] + "}";
            // Ensure that the gauge can see past the max threshold to allow user to the limits
            double minValue = Float.parseFloat(thresholds[0]) - 1.0;
            double maxValue = Float.parseFloat(thresholds[thresholds.length - 1]) + 1.0; // Always retrieve the last object
            minMax = "\"min\":" + minValue + ",\"max\":" + maxValue + ",";
        }
        return "{" + TestUtils.genExpectedCommonDefaultGrafanaPanelJson(titleContent, description, "gauge", expectedTransformations,
                databaseId, group, measure, geometryPositions, query) +
                "\"fieldConfig\":{" +
                "\"defaults\":{\"color\":{\"mode\": \"" + colorMode + "\"}," +
                "\"thresholds\":{\"mode\": \"absolute\"," +
                "\"steps\": [{\"color\":\"red\",\"value\":null}," + colorSteps + "]}," +
                minMax +
                "\"mappings\": []," +
                "\"unit\":\"" + UnitMapper.getUnitSyntax(unit) + "\"" +
                "}," +
                "\"overrides\": []" +
                "}," +
                "\"options\":{" +
                "\"reduceOptions\": {\"values\": false,\"calcs\": [\"lastNotNull\"],\"fields\": \"\"}," +
                "\"orientation\": \"auto\",\"showThresholdLabels\":false,\"showThresholdMarkers\":" + showThresholdMarkers + ",\"text\": {}" +
                "}" +
                "}";
    }

    public static String genAggregateQuery(Measure measure, boolean isAverage) {
        int totalItems = 0;
        StringBuilder averageQuery = new StringBuilder();
        StringBuilder summation = new StringBuilder();
        Queue<String[]> timeSeriesCols = measure.getTimeSeriesData();
        while (!timeSeriesCols.isEmpty()) {
            String[] metadata = timeSeriesCols.poll();
            if (summation.length() != 0) summation.append("+");
            summation.append("\\\"").append(metadata[1]).append("\\\"");
            totalItems++;
        }
        String aggregateVar = isAverage ? "(" + summation + ")/" + totalItems : summation.toString();
        averageQuery.append("SELECT time AS \\\"time\\\",").append(aggregateVar).append(" FROM \\\"")
                .append(measure.getTimeSeriesTable())
                .append("\\\" WHERE $__timeFilter(time)");
        return averageQuery.toString();
    }
}