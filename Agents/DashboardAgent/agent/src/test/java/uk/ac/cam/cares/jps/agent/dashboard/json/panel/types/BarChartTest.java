package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class BarChartTest {
    private static final List<String[]> SAMPLE_METADATA = new ArrayList<>();
    private static final String SAMPLE_MEASURE = "ElectricalConsumption";
    private static final String SAMPLE_UNIT = "kwh";
    private static final String SAMPLE_DATABASE_ID = "fsv8f87";
    private static final String SAMPLE_FIRST_SYSTEM_NAME = "Emergency systems";
    private static final String SAMPLE_FIRST_SYSTEM_COL_NAME = "column8";
    private static final String SAMPLE_SEC_SYSTEM_NAME = "Kitchen system";
    private static final String SAMPLE_SEC_SYSTEM_COL_NAME = "column22";
    private static final String SAMPLE_SYSTEM_TABLE_NAME = "table5";
    private static final int SAMPLE_PANEL_HEIGHT = 8;
    private static final int SAMPLE_PANEL_WIDTH = 12;
    private static final int SAMPLE_PANEL_X_POSITION = 1;
    private static final int SAMPLE_PANEL_Y_POSITION = 0;


    @BeforeAll
    static void setup() {
        SAMPLE_METADATA.add(new String[]{SAMPLE_FIRST_SYSTEM_NAME, SAMPLE_FIRST_SYSTEM_COL_NAME, SAMPLE_SYSTEM_TABLE_NAME});
        SAMPLE_METADATA.add(new String[]{SAMPLE_SEC_SYSTEM_NAME, SAMPLE_SEC_SYSTEM_COL_NAME, SAMPLE_SYSTEM_TABLE_NAME});
    }

    @Test
    void testConstructor() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_SYSTEM_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Execute the method
        BarChart chart = new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        // Verify results
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
    }


    @Test
    void testConstruct_NoUnit() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_SYSTEM_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        BarChart chart = new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
        assertEquals("null", chart.getUnit());
    }

    @Test
    void testConstruct_WithUnit() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_SYSTEM_TABLE_NAME, SAMPLE_DATABASE_ID, SAMPLE_UNIT};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        BarChart chart = new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_UNIT, SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
        assertEquals(SAMPLE_UNIT, chart.getUnit());
    }

    @Test
    void testGetMeasure() {
        // Construct the object
        BarChart chart = new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_UNIT, SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        // Execute the method and verify result
        assertEquals(SAMPLE_MEASURE, chart.getMeasure());
    }


    public static String genExpectedResults(String[] metadata, int[] geometryPositions, List<String[]> itemDetails) {
        String titleContent = StringHelper.addSpaceBetweenCapitalWords(metadata[0]) + " trends";
        titleContent = metadata[4].equals("null") ? titleContent : titleContent + " [" + metadata[4] + "]"; // Unit is optional
        String description = "A bar chart displaying the trends for " + metadata[0].toLowerCase() + " over a specific period";
        String expectedTransformations = "[" +
                TransformationOptionsTest.genExpectedGroupByTransformation("range", itemDetails) + "," +
                TransformationOptionsTest.genExpectedOrganizeTransformation(itemDetails, " (range)") + "]";
        StringBuilder query = new StringBuilder().append("SELECT to_char(time, 'DD-Mon-YY') as \\\"Day\\\", ${")
                .append(StringHelper.formatVariableName(metadata[0])).append(StringHelper.formatVariableName(metadata[1])).append(":csv} ")
                .append("FROM \\\"").append(metadata[2]).append("\\\" ")
                .append("WHERE time BETWEEN TO_TIMESTAMP(${__to}/1000) - interval '6 day' AND TO_TIMESTAMP(${__to}/1000) ")
                .append("ORDER BY (EXTRACT(DOW FROM time)- EXTRACT(DOW FROM TO_TIMESTAMP(${__to}/1000)) + 6) % 7;");
        StringBuilder sb = new StringBuilder();
        sb.append("{").append(TestUtils.genExpectedCommonTemplatePanelJson(titleContent, description, expectedTransformations, metadata, geometryPositions, itemDetails, query.toString()))
                .append(",\"type\": \"barchart\",")
                .append("\"pluginVersion\": \"10.0.3\",")
                // Field Configuration
                .append("\"fieldConfig\": { ")
                .append("\"defaults\": {\"color\": {\"mode\": \"palette-classic\"},")
                // Custom parts of field configurations
                .append("\"custom\":{").append("\"axisCenteredZero\":false,\"axisColorMode\":\"text\",")
                .append("\"axisLabel\":\"\",\"axisPlacement\":\"auto\", \"barAlignment\":0, \"drawStyle\":\"line\",")
                .append("\"fillOpacity\":80,\"gradientMode\":\"none\",\"lineWidth\":1,")
                .append("\"hideFrom\":{\"legend\":false, \"tooltip\":false, \"viz\":false},")
                .append("\"scaleDistribution\":{\"type\":\"linear\"}, \"showPoints\":\"auto\", \"spanNulls\":false,")
                .append("\"stacking\":{\"group\":\"A\", \"mode\":\"none\"}, \"thresholdsStyle\":{\"mode\":\"off\"}")
                .append("},") // End of custom parts
                .append("\"thresholds\":{\"mode\": \"absolute\", \"steps\": [" +
                        "{\"color\":\"green\",\"value\":null},{\"color\":\"red\",\"value\":80},}]},)")
                .append("\"mappings\": []")
                .append("},")
                .append("\"overrides\": []")
                .append("},") // End of field configuration
                // Options
                .append("\"options\":{")
                .append("\"legend\":{\"calcs\": [], \"displayMode\":\"list\",\"placement\":\"bottom\",\"showLegend\":true},")
                .append("\"tooltip\":{\"mode\":\"single\",\"sort\":\"none\"},")
                .append("\"barRadius\":0,\"barWidth\":0.97,\"fullHighlight\":false,\"groupWidth\":0.7,")
                .append("\"orientation\":\"auto\",\"showValue\":\"never\",\"stacking\":\"normal\",")
                .append("\"xTickLabelRotation\":0,\"xTickLabelSpacing\":0")
                .append("}") // end of options
                .append("}");
        return sb.toString();
    }
}