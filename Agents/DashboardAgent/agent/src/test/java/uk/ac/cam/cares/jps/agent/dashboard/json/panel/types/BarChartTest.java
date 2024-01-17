package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.TemporalInterval;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.UnitMapper;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.TransformationOptionsTest;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

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
    private static final String INTERVAL_VAR_NAME = "interval";
    private static final String CURR_MONTH_SQL_VARIABLE = "currmonth";
    private static final String REF_MONTH_SQL_VARIABLE = "refmonth";
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
    void testConstructor_ChartTypeOne() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_SYSTEM_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Execute the method
        BarChart chart = new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, 1);
        // Verify results
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA, 1), result);
    }

    @Test
    void testConstructor_ChartTypeTwo() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_SYSTEM_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Execute the method
        BarChart chart = new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, 2);
        // Verify results
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA, 2), result);
    }

    @Test
    void testConstructor_ChartTypeThree() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_SYSTEM_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Execute the method
        BarChart chart = new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, 3);
        // Verify results
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA, 3), result);
    }

    @Test
    void testConstructor_ChartTypeFour() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_SYSTEM_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Execute the method
        BarChart chart = new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, 4);
        // Verify results
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA, 4), result);
    }

    @Test
    void testConstructor_ChartTypeFive() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_SYSTEM_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Execute the method
        BarChart chart = new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, 5);
        // Verify results
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA, 5), result);
    }

    @Test
    void testConstruct_NoUnit() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_SYSTEM_TABLE_NAME, SAMPLE_DATABASE_ID, "null"};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        BarChart chart = new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA, 3);
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA, 3), result);
        assertEquals("null", chart.getUnit());
    }

    @Test
    void testConstruct_WithUnit() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_SYSTEM_TABLE_NAME, SAMPLE_DATABASE_ID, SAMPLE_UNIT};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        BarChart chart = new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_UNIT, SAMPLE_DATABASE_ID, SAMPLE_METADATA, 3);
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA, 3), result);
        assertEquals(SAMPLE_UNIT, chart.getUnit());
    }

    @Test
    void testGetMeasure() {
        // Construct the object
        BarChart chart = new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_UNIT, SAMPLE_DATABASE_ID, SAMPLE_METADATA, 3);
        // Execute the method and verify result
        assertEquals(SAMPLE_MEASURE, chart.getMeasure());
    }

    @Test
    void testConstructor_InvalidChartType() {
        // Verify that illegal argument exception is thrown with wrong chart type
        IllegalArgumentException error = assertThrows(IllegalArgumentException.class, () ->
                new BarChart(SAMPLE_MEASURE, StringHelper.SYSTEM_KEY, SAMPLE_UNIT, SAMPLE_DATABASE_ID, SAMPLE_METADATA, 100)
        );
        assertEquals("Invalid input for bar chart type. Only 1,2, 3, 4, or 5 is accepted as valid argument.", error.getMessage());
    }


    public static String genExpectedResults(String[] metadata, int[] geometryPositions, List<String[]> itemDetails, int chartType) {
        String titleContent = StringHelper.addSpaceBetweenCapitalWords(metadata[0]);
        String description = "";
        String query = "";
        String orientationOption = "auto";
        String stackingOption = "normal";
        String showValueOption = "never";
        String groupWidthOption = "0.7";
        List<String[]> transformationInputs = itemDetails;
        // Process the inputs based on the chart type required
        switch (chartType) {
            case 1:
                titleContent += " for current month";
                description = "A bar chart displaying the " + metadata[0].toLowerCase() + " for the current month";
                query = "SELECT to_char(time,'Mon-YY') AS \\\"interval\\\"," +
                        "${" + StringHelper.formatVariableName(metadata[0]) + StringHelper.formatVariableName(metadata[1]) + ":csv} " +
                        "FROM \\\"" + metadata[2] + "\\\" " +
                        "WHERE time BETWEEN DATE_TRUNC('MONTH', TO_TIMESTAMP(${__to}/1000)) AND TO_TIMESTAMP(${__to}/1000)";
                // Modify the following options
                orientationOption = "horizontal";
                stackingOption = "none";
                showValueOption = "always";
                groupWidthOption = "1";
                break;
            case 2:
                titleContent += " trends for last period";
                description = "A bar chart displaying the trends for " + metadata[0].toLowerCase() + " over the last period of the specified interval";
                query = genTrendsQueryConfiguration(metadata[0], metadata[1], metadata[2], false);
                break;
            case 3:
                titleContent += " trends for current period";
                description = "A bar chart displaying the trends for " + metadata[0].toLowerCase() + " over the current period of the specified interval";
                query = genTrendsQueryConfiguration(metadata[0], metadata[1], metadata[2], true);
                break;
            case 4:
                titleContent = "Daily comparisons for " + titleContent;
                description = "A bar chart displaying the daily comparisons for " + metadata[0].toLowerCase() + " between the current and reference month";
                query = genComparisonQueryConfiguration(metadata[2], true, itemDetails);
                // Modify the following options
                stackingOption = "none";
                showValueOption = "always";
                transformationInputs = List.of(
                        new String[]{"Current Month", CURR_MONTH_SQL_VARIABLE},
                        new String[]{StringHelper.REF_MONTH_VARIABLE_NAME, REF_MONTH_SQL_VARIABLE}
                );
                break;
            case 5:
                titleContent = "Weekly comparisons for " + titleContent;
                description = "A bar chart displaying the weekly comparisons for " + metadata[0].toLowerCase() + " between the current and reference month";
                query = genComparisonQueryConfiguration(metadata[2], false, itemDetails);
                // Modify the following options
                stackingOption = "none";
                showValueOption = "always";
                transformationInputs = List.of(
                        new String[]{"Current Month", CURR_MONTH_SQL_VARIABLE},
                        new String[]{StringHelper.REF_MONTH_VARIABLE_NAME, REF_MONTH_SQL_VARIABLE}
                );
                break;
            default:
        }
        titleContent = metadata[4].equals("null") ? titleContent : titleContent + " [" + metadata[4] + "]"; // Unit is optional
        String expectedTransformations = "[" +
                TransformationOptionsTest.genExpectedGroupByTransformation("range", transformationInputs) + "," +
                TransformationOptionsTest.genExpectedOrganizeTransformation(transformationInputs, " (range)") + "]";
        return "{" + TestUtils.genExpectedCommonDefaultGrafanaPanelJson(titleContent, description, "barchart", expectedTransformations, metadata, geometryPositions, query) +
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
                "\"barRadius\":0,\"barWidth\":0.8,\"fullHighlight\":false,\"groupWidth\":" + groupWidthOption + "," +
                "\"orientation\":\"" + orientationOption + "\",\"showValue\":\"" + showValueOption + "\",\"stacking\":\"" + stackingOption + "\"," +
                "\"xTickLabelRotation\":0,\"xTickLabelSpacing\":100" +
                "}" + // end of options
                "}";
    }

    /**
     * Generate the query configuration for trends.
     *
     * @param measure        The measure name for this variable.
     * @param itemGroup      The item group for this measure - asset type, rooms, or smart meters.
     * @param tableName      The name of the table containing the data.
     * @param isCurrentTrend A boolean indicating if we should generate the syntax for current trend or not.
     */
    private static String genTrendsQueryConfiguration(String measure, String itemGroup, String tableName, boolean isCurrentTrend) {
        String timeIntervalVariableName = StringHelper.formatVariableName(StringHelper.INTERVAL_VARIABLE_NAME);
        String dailyOverWeekTimeFrameClause = isCurrentTrend ? "THEN time BETWEEN TO_TIMESTAMP(${__to}/1000)-INTERVAL'7 day' AND TO_TIMESTAMP(${__to}/1000)" : "THEN time BETWEEN TO_TIMESTAMP(${__to}/1000)-INTERVAL'14 day' AND DATE_TRUNC('DAY', TO_TIMESTAMP(${__to}/1000))-INTERVAL'7 day'";
        String dailyOverMonthTimeFrameClause = isCurrentTrend ? "THEN time BETWEEN TO_TIMESTAMP(${__to}/1000)-INTERVAL'1 month' AND TO_TIMESTAMP(${__to}/1000)" : "THEN time BETWEEN TO_TIMESTAMP(${__to}/1000)-INTERVAL'2 month' AND DATE_TRUNC('DAY', TO_TIMESTAMP(${__to}/1000))-INTERVAL'1 month'";
        String weeklyOverMonthTimeFrameClause = isCurrentTrend ? "THEN time BETWEEN TO_TIMESTAMP(${__to}/1000)-INTERVAL'1 month' AND TO_TIMESTAMP(${__to}/1000)" : "THEN time BETWEEN TO_TIMESTAMP(${__to}/1000)-INTERVAL'2 month' AND DATE_TRUNC('WEEK', TO_TIMESTAMP(${__to}/1000))-INTERVAL'1 month'";
        String monthlyTimeFrameClause = isCurrentTrend ? "THEN time BETWEEN TO_TIMESTAMP(${__to}/1000)-INTERVAL'1 year' AND TO_TIMESTAMP(${__to}/1000)" : "THEN time BETWEEN TO_TIMESTAMP(${__to}/1000)-INTERVAL'2 year' AND DATE_TRUNC('MONTH', TO_TIMESTAMP(${__to}/1000))-INTERVAL'1 year'";
        return "SELECT CASE" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.DAILY_OVER_WEEK + "' THEN to_char(time,'DD-Mon-YY')" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.DAILY_OVER_MONTH + "' THEN to_char(time,'DD-Mon')" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.WEEKLY_OVER_MONTH + "' THEN 'Week '|| to_char(time,'W Mon-YY')" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.MONTHLY + "' THEN to_char(time,'Mon-YY')" +
                " END AS " + StringHelper.formatEscapeQuoteSQL(INTERVAL_VAR_NAME) + ",${" +
                // Custom csv parameter must be lower case with no spacing ie: measurenameitemgroup
                StringHelper.formatVariableName(measure) + StringHelper.formatVariableName(itemGroup) + ":csv} " +
                "FROM " + StringHelper.formatEscapeQuoteSQL(tableName) +
                // Time interval according to template variable
                " WHERE CASE" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.DAILY_OVER_WEEK + "' " + dailyOverWeekTimeFrameClause +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.DAILY_OVER_MONTH + "' " + dailyOverMonthTimeFrameClause +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.WEEKLY_OVER_MONTH + "' " + weeklyOverMonthTimeFrameClause +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.MONTHLY + "' " + monthlyTimeFrameClause +
                " END " +
                // Arrange results starting from the latest interval and go backwards
                "ORDER BY CASE" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.DAILY_OVER_WEEK + "' OR '${" +
                timeIntervalVariableName + ":csv}'='" + TemporalInterval.DAILY_OVER_MONTH + "' THEN (EXTRACT(DOY FROM time)-EXTRACT(DOY FROM TO_TIMESTAMP(${__to}/1000))+365)%366" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.WEEKLY_OVER_MONTH + "' THEN (EXTRACT(WEEK FROM time)-EXTRACT(WEEK FROM TO_TIMESTAMP(${__to}/1000))+51)%52" +
                " WHEN '${" + timeIntervalVariableName + ":csv}'='" + TemporalInterval.MONTHLY + "' THEN (EXTRACT(MONTH FROM time)-EXTRACT(MONTH FROM TO_TIMESTAMP(${__to}/1000))+11)%12" +
                " END;";
    }

    /**
     * Generate the query configuration for comparison.
     *
     * @param tableName          The name of the table containing the data.
     * @param isDaily            A boolean indicating if we should generate the syntax for daily or weekly intervals.
     * @param timeSeriesMetadata A list of items and their metadata for the specified measure.
     */
    private static String genComparisonQueryConfiguration(String tableName, boolean isDaily, List<String[]> timeSeriesMetadata) {
        String refMonthVar = StringHelper.formatVariableName(StringHelper.REF_MONTH_VARIABLE_NAME);
        String measureSummationSyntax = timeSeriesMetadata.stream()
                .map(meterMetadata -> meterMetadata[1])
                .collect(Collectors.joining("+"));
        String intervalSyntax = isDaily ? "'Day ' || TO_CHAR(time,'DD')" : "'Week ' || TO_CHAR(time,'W')";
        return "SELECT " + intervalSyntax + " AS " + StringHelper.formatEscapeQuoteSQL(INTERVAL_VAR_NAME) + "," +
                // For reference month, we must query and ensure data is from the start of the month to the end relative from the current time
                "CASE WHEN time BETWEEN " +
                // To get start of reference month, get current month from specified timestamp and deduct the required number of months as interval
                "DATE_TRUNC('MONTH',TO_TIMESTAMP(${__to}/1000))-" +
                // Interval to be subtracted from current month to get reference month
                "CAST(CASE WHEN EXTRACT(MONTH FROM TO_TIMESTAMP(${__to}/1000))-${" + refMonthVar + "}<=0 THEN EXTRACT(MONTH FROM TO_TIMESTAMP(${__to}/1000))-${" + refMonthVar + "}+12 " +
                "ELSE EXTRACT(MONTH FROM TO_TIMESTAMP(${__to}/1000))-${" + refMonthVar + "} END||' month' AS Interval)" +
                " AND " +
                // Similar query to above but this gets the end of the month by adding INTERVAL '1 month'
                "DATE_TRUNC('MONTH',TO_TIMESTAMP(${__to}/1000))+INTERVAL '1 month'-" +
                // Interval to be subtracted from current month to get reference month
                "CAST(CASE WHEN EXTRACT(MONTH FROM TO_TIMESTAMP(${__to}/1000))-${" + refMonthVar + "}<=0 THEN EXTRACT(MONTH FROM TO_TIMESTAMP(${__to}/1000))-${" + refMonthVar + "}+12 " +
                "ELSE EXTRACT(MONTH FROM TO_TIMESTAMP(${__to}/1000))-${" + refMonthVar + "} END||' month' AS Interval)" +
                // END of the first case when
                " THEN " + measureSummationSyntax +
                " END AS " + StringHelper.formatEscapeQuoteSQL(REF_MONTH_SQL_VARIABLE) + "," +
                // For current month, get from start of month to current time
                " CASE WHEN time BETWEEN DATE_TRUNC('MONTH',TO_TIMESTAMP(${__to}/1000)) AND TO_TIMESTAMP(${__to}/1000) THEN " + measureSummationSyntax +
                " END AS " + StringHelper.formatEscapeQuoteSQL(CURR_MONTH_SQL_VARIABLE) +
                " FROM " + StringHelper.formatEscapeQuoteSQL(tableName) +
                // These comparisons will only be available up to a year ago
                " WHERE time BETWEEN TO_TIMESTAMP(${__to}/1000)-INTERVAL '1 year' AND TO_TIMESTAMP(${__to}/1000)" +
                " ORDER BY " + INTERVAL_VAR_NAME + ";";
    }
}