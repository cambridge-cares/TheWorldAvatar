package uk.ac.cam.cares.jps.agent.dashboard.json.panel;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class GaugeTest {
    private static final List<String[]> SAMPLE_METADATA = new ArrayList<>();
    private static final String SAMPLE_MEASURE = "ElectricalConsumption";
    private static final String SAMPLE_UNIT = "kwh";
    private static final String SAMPLE_ITEM_GROUP = "Fridge";
    private static final String SAMPLE_DATABASE_ID = "eaus17";
    private static final String SAMPLE_FIRST_ASSET_NAME = "asset one";
    private static final String SAMPLE_FIRST_ASSET_COL_NAME = "column7";
    private static final String SAMPLE_ASSET_TABLE_NAME = "table1";
    private static final String SAMPLE_SEC_ASSET_NAME = "asset two";
    private static final String SAMPLE_SEC_ASSET_COL_NAME = "column16";
    private static final int SAMPLE_PANEL_HEIGHT = 8;
    private static final int SAMPLE_PANEL_WIDTH = 12;
    private static final int SAMPLE_PANEL_X_POSITION = 1;
    private static final int SAMPLE_PANEL_Y_POSITION = 0;


    @BeforeAll
    static void setup() {
        SAMPLE_METADATA.add(new String[]{SAMPLE_FIRST_ASSET_NAME, SAMPLE_FIRST_ASSET_COL_NAME, SAMPLE_ASSET_TABLE_NAME});
        SAMPLE_METADATA.add(new String[]{SAMPLE_SEC_ASSET_NAME, SAMPLE_SEC_ASSET_COL_NAME, SAMPLE_ASSET_TABLE_NAME});
    }

    @Test
    void testConstructor() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Execute the method
        Gauge chart = new Gauge(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, true);
        // Verify results
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA, genAverageQuery(SAMPLE_METADATA)), result);
    }

    @Test
    void testConstructorNoOptionalValues() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Execute the method
        Gauge chart = new Gauge(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        // Verify results
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
    }

    @Test
    void testConstructNoUnit() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        Gauge chart = new Gauge(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
    }

    @Test
    void testConstructWithUnit() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID, SAMPLE_UNIT};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        Gauge chart = new Gauge(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_UNIT, SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
    }

    private static String genExpectedResults(String[] metadata, int[] geometryPositions, List<String[]> itemDetails) {
        return genExpectedResults(metadata, geometryPositions, itemDetails, "");
    }

    private static String genExpectedResults(String[] metadata, int[] geometryPositions, List<String[]> itemDetails, String query) {
        String titleContent = "Latest " + StringHelper.addSpaceBetweenCapitalWords(metadata[0]);
        titleContent = query.isEmpty() ? metadata.length > 4 ? titleContent + " [" + metadata[4] + "]" : titleContent : "Latest Average";
        String description = query.isEmpty() ? "A gauge chart displaying the latest value of all individuals' " + metadata[0].toLowerCase() + " for " + metadata[1].toLowerCase()
                : "A gauge chart displaying the latest average value of " + metadata[0].toLowerCase() + " for " + metadata[1].toLowerCase();;
        StringBuilder sb = new StringBuilder();
        sb.append("{").append(TemplatePanelTest.genExpectedCommonJsonBase(titleContent, description, metadata, geometryPositions, itemDetails, query))
                .append(",\"type\": \"gauge\",")
                .append("\"fieldConfig\":{")
                .append("\"defaults\":{\"color\":{\"mode\": \"palette-classic\"},")
                .append("\"thresholds\":{\"mode\": \"absolute\",")
                .append("\"steps\": [{\"color\":\"green\",\"value\":null},{\"color\":\"red\",\"value\":80}]},")
                .append("\"mappings\": []")
                .append("},")
                .append("\"overrides\": []")
                .append("},")
                .append("\"options\":{")
                .append("\"reduceOptions\": {\"values\": false,\"calcs\": [\"lastNotNull\"],\"fields\": \"\"},")
                .append("\"orientation\": \"auto\",\"showThresholdLabels\":false,\"showThresholdMarkers\":false,\"text\": {}")
                .append("}")
                .append("}");
        return sb.toString();
    }

    private static String genAverageQuery(List<String[]> itemsMetadata) {
        int totalItems = 0;
        StringBuilder averageQuery = new StringBuilder();
        StringBuilder summation = new StringBuilder();
        for (String[] metadata : itemsMetadata) {
            if (summation.length() != 0) summation.append("+");
            summation.append("\\\"").append(metadata[1]).append("\\\"");
            totalItems++;
        }
        averageQuery.append("SELECT time AS \\\"time\\\",(").append(summation).append(")/").append(totalItems).append(" FROM \\\"")
                .append(itemsMetadata.get(0)[2])
                .append("\\\" WHERE $__timeFilter(time)");
        return averageQuery.toString();
    }
}