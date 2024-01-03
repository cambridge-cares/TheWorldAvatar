package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.UnitMapper;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class GaugeTest {
    private static final List<String[]> SAMPLE_METADATA = new ArrayList<>();
    private static final String[] SAMPLE_THRESHOLDS = new String[2];
    private static final String SAMPLE_MEASURE = "ElectricalConsumption";
    private static final String SAMPLE_UNIT = "kWh";
    private static final String SAMPLE_ITEM_GROUP = "Fridge";
    private static final String SAMPLE_DATABASE_ID = "eaus17";
    private static final String SAMPLE_FIRST_ASSET_NAME = "asset one";
    private static final String SAMPLE_FIRST_ASSET_COL_NAME = "column7";
    private static final String SAMPLE_ASSET_TABLE_NAME = "table1";
    private static final String SAMPLE_SEC_ASSET_NAME = "asset two";
    private static final String SAMPLE_SEC_ASSET_COL_NAME = "column16";
    private static final String THRESHOLD_MIN = "1";
    private static final String THRESHOLD_MAX = "5";
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
    void testConstructor() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Execute the method
        Gauge chart = new Gauge(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, SAMPLE_THRESHOLDS, true);
        // Verify results
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA, SAMPLE_THRESHOLDS, genAggregateQuery(SAMPLE_METADATA, true)), result);
    }

    @Test
    void testConstructorNoThresholds() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Execute the method
        Gauge chart = new Gauge(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, new String[]{}, true);
        // Verify results
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA, genAggregateQuery(SAMPLE_METADATA, true)), result);
    }

    @Test
    void testConstructorNoOptionalValues() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Execute the method
        Gauge chart = new Gauge(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, new String[]{});
        // Verify results
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
    }

    @Test
    void testConstructNoUnit() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        Gauge chart = new Gauge(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, new String[]{});
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
        assertEquals("null", chart.getUnit());
    }

    @Test
    void testConstructWithUnit() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID, SAMPLE_UNIT};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        Gauge chart = new Gauge(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_UNIT, SAMPLE_DATABASE_ID, SAMPLE_METADATA, new String[]{});
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
        assertEquals(SAMPLE_UNIT, chart.getUnit());
    }

    @Test
    void testGetMeasure() {
        // Construct the object
        Gauge chart = new Gauge(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_UNIT, SAMPLE_DATABASE_ID, SAMPLE_METADATA, new String[]{});
        // Execute the method and verify result
        assertEquals(SAMPLE_MEASURE, chart.getMeasure());
    }

    public static String genExpectedResults(String[] metadata, int[] geometryPositions, List<String[]> itemDetails) {
        return genExpectedResults(metadata, geometryPositions, itemDetails, new String[]{}, "");
    }

    public static String genExpectedResults(String[] metadata, int[] geometryPositions, List<String[]> itemDetails, String[] thresholds) {
        return genExpectedResults(metadata, geometryPositions, itemDetails, thresholds, "");
    }

    public static String genExpectedResults(String[] metadata, int[] geometryPositions, List<String[]> itemDetails, String query) {
        return genExpectedResults(metadata, geometryPositions, itemDetails, new String[]{}, query);
    }

    public static String genExpectedResults(String[] metadata, int[] geometryPositions, List<String[]> itemDetails, String[] thresholds, String query) {
        String titleContent = "Latest " + StringHelper.addSpaceBetweenCapitalWords(metadata[0]);
        titleContent = query.isEmpty() ?
                metadata[4].equals("null") ? titleContent : titleContent + " [" + metadata[4] + "]"  // If query is empty, this is for generic gauges with no aggregate computation
                : query.contains(")/") ? "Latest Average" : "Latest Cumulative Total"; // If there is signs of an average computation, employ average title instead
        // Similar to above
        String description = query.isEmpty() ? "A gauge chart displaying the latest value of all individuals' "
                : query.contains(")/") ? "A gauge chart displaying the latest average value of " : "A gauge chart displaying the latest cumulative total value of ";
        description += query.isEmpty() ? metadata[0].toLowerCase() + " for " + metadata[1].toLowerCase() :
                metadata[0].toLowerCase() + " for all " + metadata[1].toLowerCase() + "; Do note that this value is constant at the dashboard level and unaffected by any filters for individual elements";
        String expectedTransformations = "[" + TransformationOptionsTest.genExpectedOrganizeTransformation(itemDetails, "") + "]";
        boolean showThresholdMarkers = false;
        String colorMode = "palette-classic";
        String colorSteps = "{\"color\":\"red\",\"value\":80}";
        String minMax = "";
        if (thresholds.length != 0) {
            showThresholdMarkers = true;
            colorMode = "thresholds";
            colorSteps = "{\"color\":\"green\",\"value\":" + thresholds[0] + "},{\"color\":\"red\",\"value\":" + thresholds[1] + "}";
            // Ensure that the gauge can see past the max threshold to allow user to the limits
            double minValue = Float.parseFloat(thresholds[0]) - 1.0;
            double maxValue = Float.parseFloat(thresholds[1]) + 1.0;
            minMax = "\"min\":" + minValue + ",\"max\":" + maxValue + ",";
        }
        return "{" + TestUtils.genExpectedCommonTemplatePanelJson(titleContent, description, expectedTransformations, metadata, geometryPositions, itemDetails, query) +
                ",\"type\": \"gauge\"," +
                "\"fieldConfig\":{" +
                "\"defaults\":{\"color\":{\"mode\": \"" + colorMode + "\"}," +
                "\"thresholds\":{\"mode\": \"absolute\"," +
                "\"steps\": [{\"color\":\"red\",\"value\":null}," + colorSteps + "]}," +
                minMax +
                "\"mappings\": []," +
                "\"unit\":\"" + UnitMapper.getUnitSyntax(metadata[4]) + "\"" +
                "}," +
                "\"overrides\": []" +
                "}," +
                "\"options\":{" +
                "\"reduceOptions\": {\"values\": false,\"calcs\": [\"lastNotNull\"],\"fields\": \"\"}," +
                "\"orientation\": \"auto\",\"showThresholdLabels\":false,\"showThresholdMarkers\":" + showThresholdMarkers + ",\"text\": {}" +
                "}" +
                "}";
    }

    public static String genAggregateQuery(List<String[]> itemsMetadata, boolean isAverage) {
        int totalItems = 0;
        StringBuilder averageQuery = new StringBuilder();
        StringBuilder summation = new StringBuilder();
        for (String[] metadata : itemsMetadata) {
            if (summation.length() != 0) summation.append("+");
            summation.append("\\\"").append(metadata[1]).append("\\\"");
            totalItems++;
        }
        String aggregateVar = isAverage ? "(" + summation + ")/" + totalItems : summation.toString();
        averageQuery.append("SELECT time AS \\\"time\\\",").append(aggregateVar).append(" FROM \\\"")
                .append(itemsMetadata.get(0)[2])
                .append("\\\" WHERE $__timeFilter(time)");
        return averageQuery.toString();
    }
}