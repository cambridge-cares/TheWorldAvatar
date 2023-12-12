package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.TemporalInterval;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.UnitMapper;
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
        String timeIntervalVariableName = StringHelper.formatVariableName(StringHelper.INTERVAL_VARIABLE_NAME);
        String query = "SELECT CASE" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.DAILY_OVER_WEEK + "' THEN to_char(time,'DD-Mon-YY')" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.DAILY_OVER_MONTH + "' THEN to_char(time,'DD-Mon')" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.WEEKLY_OVER_MONTH + "' THEN 'Week '|| to_char(time,'W Mon-YY')" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.MONTHLY + "' THEN to_char(time,'Mon-YY')" +
                " END AS \\\"interval\\\",${" +
                StringHelper.formatVariableName(metadata[0]) + StringHelper.formatVariableName(metadata[1]) + ":csv} " +
                "FROM \\\"" + metadata[2] + "\\\" " +
                "WHERE CASE" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.DAILY_OVER_WEEK + "' THEN time BETWEEN TO_TIMESTAMP(${__to}/1000)-interval'6 day' AND TO_TIMESTAMP(${__to}/1000)" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.DAILY_OVER_MONTH + "' THEN time BETWEEN TO_TIMESTAMP(${__to}/1000)-interval'1 month'+interval'1 day' AND TO_TIMESTAMP(${__to}/1000)" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.WEEKLY_OVER_MONTH + "' THEN time BETWEEN TO_TIMESTAMP(${__to}/1000)-interval'1 month'+interval'1 day' AND TO_TIMESTAMP(${__to}/1000)" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.MONTHLY + "' THEN time BETWEEN TO_TIMESTAMP(${__to}/1000)-interval'1 year'+interval'1 day' AND TO_TIMESTAMP(${__to}/1000)" +
                " END " +
                // Arrange results starting from the latest interval and go backwards
                "ORDER BY CASE" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.DAILY_OVER_WEEK + "' THEN (EXTRACT(DOW FROM time)-EXTRACT(DOW FROM TO_TIMESTAMP(${__to}/1000))+6)%7" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.DAILY_OVER_MONTH + "' THEN (EXTRACT(DOY FROM time)-EXTRACT(DOY FROM TO_TIMESTAMP(${__to}/1000))+365)%366" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.WEEKLY_OVER_MONTH + "' THEN (EXTRACT(WEEK FROM time)-EXTRACT(WEEK FROM TO_TIMESTAMP(${__to}/1000))+51)%52" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.MONTHLY + "' THEN (EXTRACT(MONTH FROM time)-EXTRACT(MONTH FROM TO_TIMESTAMP(${__to}/1000))+11)%12" +
                " END;";
        return "{" + TestUtils.genExpectedCommonTemplatePanelJson(titleContent, description, expectedTransformations, metadata, geometryPositions, itemDetails, query) +
                ",\"type\": \"barchart\"," +
                "\"pluginVersion\": \"10.0.3\"," +
                // Field Configuration
                "\"fieldConfig\": { " +
                "\"defaults\": {\"color\": {\"mode\": \"palette-classic\"}," +
                // Custom parts of field configurations
                "\"custom\":{" + "\"axisCenteredZero\":false,\"axisColorMode\":\"text\"," +
                "\"axisLabel\":\"\",\"axisPlacement\":\"auto\", \"barAlignment\":0, \"drawStyle\":\"line\"," +
                "\"fillOpacity\":80,\"gradientMode\":\"none\",\"lineWidth\":1," +
                "\"hideFrom\":{\"legend\":false, \"tooltip\":false, \"viz\":false}," +
                "\"scaleDistribution\":{\"type\":\"linear\"}, \"showPoints\":\"auto\", \"spanNulls\":false," +
                "\"stacking\":{\"group\":\"A\", \"mode\":\"none\"}, \"thresholdsStyle\":{\"mode\":\"off\"}" +
                "}," + // End of custom parts
                "\"thresholds\":{\"mode\": \"absolute\", \"steps\": [" +
                "{\"color\":\"green\",\"value\":null},{\"color\":\"red\",\"value\":80}" +
                "]}," +
                "\"mappings\": []," +
                "\"unit\":\"" + UnitMapper.getUnitSyntax(metadata[4]) + "\"" +
                "}," +
                "\"overrides\": []" +
                "}," + // End of field configuration
                // Options
                "\"options\":{" +
                "\"legend\":{\"calcs\": [], \"displayMode\":\"list\",\"placement\":\"bottom\",\"showLegend\":true}," +
                "\"tooltip\":{\"mode\":\"single\",\"sort\":\"none\"}," +
                "\"barRadius\":0,\"barWidth\":0.8,\"fullHighlight\":false,\"groupWidth\":0.7," +
                "\"orientation\":\"auto\",\"showValue\":\"never\",\"stacking\":\"normal\"," +
                "\"xTickLabelRotation\":0,\"xTickLabelSpacing\":100" +
                "}" + // end of options
                "}";
    }
}