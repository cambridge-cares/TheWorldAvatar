package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.UnitMapper;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class TimeSeriesChartTest {
    private static final List<String[]> SAMPLE_METADATA = new ArrayList<>();
    private static final String[] SAMPLE_THRESHOLDS = new String[2];
    private static final String SAMPLE_MEASURE = "ElectricalConsumption";
    private static final String SAMPLE_UNIT = "kwh";
    private static final String SAMPLE_ITEM_GROUP = "Fridge";
    private static final String SAMPLE_DATABASE_ID = "eaus17";
    private static final String SAMPLE_FIRST_ASSET_NAME = "asset one";
    private static final String SAMPLE_FIRST_ASSET_COL_NAME = "column7";
    private static final String SAMPLE_ASSET_TABLE_NAME = "table1";
    private static final String SAMPLE_SEC_ASSET_NAME = "asset two";
    private static final String SAMPLE_SEC_ASSET_COL_NAME = "column16";
    private static final String THRESHOLD_MIN = "3";
    private static final String THRESHOLD_MAX = "8";
    private static final int SAMPLE_PANEL_HEIGHT = 8;
    private static final int SAMPLE_PANEL_WIDTH = 12;
    private static final int SAMPLE_PANEL_X_POSITION = 1;
    private static final int SAMPLE_PANEL_Y_POSITION = 0;


    @BeforeAll
    static void setup() {
        SAMPLE_METADATA.add(new String[]{SAMPLE_FIRST_ASSET_NAME, SAMPLE_FIRST_ASSET_COL_NAME, SAMPLE_ASSET_TABLE_NAME});
        SAMPLE_METADATA.add(new String[]{SAMPLE_SEC_ASSET_NAME, SAMPLE_SEC_ASSET_COL_NAME, SAMPLE_ASSET_TABLE_NAME});
        SAMPLE_THRESHOLDS[0] = THRESHOLD_MIN;
        SAMPLE_THRESHOLDS[1] = THRESHOLD_MAX;
    }

    @Test
    void testConstructWithThresholds() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        TimeSeriesChart chart = new TimeSeriesChart(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, SAMPLE_THRESHOLDS);
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA, SAMPLE_THRESHOLDS), result);
    }

    @Test
    void testConstructNoThresholds() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        TimeSeriesChart chart = new TimeSeriesChart(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, new String[]{});
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
    }

    @Test
    void testConstructNoUnit() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        TimeSeriesChart chart = new TimeSeriesChart(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, new String[]{});
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
        assertEquals("null", chart.getUnit());
    }

    @Test
    void testConstructWithUnit() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID, SAMPLE_UNIT, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        TimeSeriesChart chart = new TimeSeriesChart(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_UNIT, SAMPLE_DATABASE_ID, SAMPLE_METADATA, new String[]{});
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
        assertEquals(SAMPLE_UNIT, chart.getUnit());
    }

    @Test
    void testGetMeasure() {
        // Construct the object
        TimeSeriesChart chart = new TimeSeriesChart(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_UNIT, SAMPLE_DATABASE_ID, SAMPLE_METADATA, new String[]{});
        // Execute the method and verify result
        assertEquals(SAMPLE_MEASURE, chart.getMeasure());
    }

    public static String genExpectedResults(String[] metadata, int[] geometryPositions, List<String[]> itemDetails) {
        return genExpectedResults(metadata, geometryPositions, itemDetails, new String[]{});
    }

    public static String genExpectedResults(String[] metadata, int[] geometryPositions, List<String[]> itemDetails, String[] thresholds) {
        String titleContent = StringHelper.addSpaceBetweenCapitalWords(metadata[0]) + " of " + StringHelper.addSpaceBetweenCapitalWords(metadata[1]);
        titleContent = metadata[4].equals("null") ? titleContent : titleContent + " [" + metadata[4] + "]";
        String description = "A chart displaying the time series of " + metadata[0].toLowerCase() + " for " + metadata[1].toLowerCase();
        String expectedTransformations = "[" + TransformationOptionsTest.genExpectedOrganizeTransformation(itemDetails, "") + "]";
        String thresholdStyle = thresholds.length == 0 ? "off" : "area";
        String thresholdSteps = thresholds.length == 0 ? "" : "\"thresholds\":{\"mode\": \"absolute\", \"steps\": [" +
                "{\"color\":\"red\",\"value\":null},{\"color\":\"green\",\"value\":" + thresholds[0] + "}," +
                "{\"color\":\"red\",\"value\":" + thresholds[1] + "}]},";
        return "{" + TestUtils.genExpectedCommonTemplatePanelJson(titleContent, description, expectedTransformations, metadata, geometryPositions, itemDetails) +
                ",\"type\": \"timeseries\"," +
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
                "\"unit\":\"" + UnitMapper.getUnitSyntax(metadata[4]) + "\"" +
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