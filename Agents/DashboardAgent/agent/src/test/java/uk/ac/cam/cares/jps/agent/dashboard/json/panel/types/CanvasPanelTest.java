package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Measure;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout.UnitMapper;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.canvas.CanvasFrameElementTest;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.canvas.ColorOption;
import uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.canvas.TextOption;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.Queue;

import static org.junit.jupiter.api.Assertions.*;

class CanvasPanelTest {
    private static final String SAMPLE_DATABASE_ID = "m17fh913";
    private static final String SAMPLE_WEATHER_TABLE = "wk37ey19r7f";
    private static final String SAMPLE_WEATHER_DATABASE = "weather";
    private static final String HUMIDITY_COLUMN = "column2";
    private static final String HUMIDITY_UNIT = "%";
    private static final String PRECIPITATION_COLUMN = "column1";
    private static final String PRECIPITATION_UNIT = "mm";
    private static final String TEMPERATURE_COLUMN = "column3";
    private static final String TEMPERATURE_UNIT = "deg";
    private static final String WIND_DIRECTION_COLUMN = "column4";
    private static final String FEELS_LIKE_COLUMN = "column5";
    private static final String FEELS_LIKE_UNIT = "deg";
    private static final Measure HUMIDITY = new Measure(StringHelper.WEATHER_STATION_HUMIDITY_FIELD, HUMIDITY_UNIT);
    private static final Measure PRECIPITATION = new Measure(StringHelper.WEATHER_STATION_PRECIPITATION_FIELD, PRECIPITATION_UNIT);
    private static final Measure TEMPERATURE = new Measure(StringHelper.WEATHER_STATION_TEMPERATURE_FIELD, TEMPERATURE_UNIT);
    private static final Measure WIND_DIRECTION = new Measure(StringHelper.WEATHER_STATION_WIND_DIRECTION_FIELD, null);
    private static final Measure FEELS_LIKE = new Measure(StringHelper.WEATHER_STATION_FEELS_LIKE_TEMPERATURE_FIELD, FEELS_LIKE_UNIT);
    private static final int SAMPLE_PANEL_X_POSITION = 5;
    private static final int SAMPLE_PANEL_Y_POSITION = 6;
    private static final int[] EXPECTED_GEOMETRY_POSITIONS = new int[]{TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};

    @BeforeAll
    static void setup() {
        HUMIDITY.addTimeSeriesIris(StringHelper.WEATHER_STATION_KEY, TestUtils.SAMPLE_DATA_IRI, TestUtils.SAMPLE_TS_IRI);
        HUMIDITY.addTimeSeriesMetadata(StringHelper.WEATHER_STATION_KEY, HUMIDITY_COLUMN, SAMPLE_WEATHER_TABLE, SAMPLE_WEATHER_DATABASE);
        PRECIPITATION.addTimeSeriesIris(StringHelper.WEATHER_STATION_KEY, TestUtils.SAMPLE_DATA_IRI, TestUtils.SAMPLE_TS_IRI);
        PRECIPITATION.addTimeSeriesMetadata(StringHelper.WEATHER_STATION_KEY, PRECIPITATION_COLUMN, SAMPLE_WEATHER_TABLE, SAMPLE_WEATHER_DATABASE);
        TEMPERATURE.addTimeSeriesIris(StringHelper.WEATHER_STATION_KEY, TestUtils.SAMPLE_DATA_IRI, TestUtils.SAMPLE_TS_IRI);
        TEMPERATURE.addTimeSeriesMetadata(StringHelper.WEATHER_STATION_KEY, TEMPERATURE_COLUMN, SAMPLE_WEATHER_TABLE, SAMPLE_WEATHER_DATABASE);
        WIND_DIRECTION.addTimeSeriesIris(StringHelper.WEATHER_STATION_KEY, TestUtils.SAMPLE_DATA_IRI, TestUtils.SAMPLE_TS_IRI);
        WIND_DIRECTION.addTimeSeriesMetadata(StringHelper.WEATHER_STATION_KEY, WIND_DIRECTION_COLUMN, SAMPLE_WEATHER_TABLE, SAMPLE_WEATHER_DATABASE);
        FEELS_LIKE.addTimeSeriesIris(StringHelper.WEATHER_STATION_KEY, TestUtils.SAMPLE_DATA_IRI, TestUtils.SAMPLE_TS_IRI);
        FEELS_LIKE.addTimeSeriesMetadata(StringHelper.WEATHER_STATION_KEY, FEELS_LIKE_COLUMN, SAMPLE_WEATHER_TABLE, SAMPLE_WEATHER_DATABASE);
    }

    @Test
    void testConstruct_CurrentTimeCanvas() {
        // Set up
        CanvasPanel samplePanel = new CanvasPanel(HUMIDITY, PRECIPITATION, SAMPLE_DATABASE_ID, 0);
        // Execute the method and verify result
        assertEquals(genExpectedResults(HUMIDITY, PRECIPITATION, SAMPLE_DATABASE_ID, 0, EXPECTED_GEOMETRY_POSITIONS),
                samplePanel.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION));
    }

    @Test
    void testConstruct_1HourAgoTimeCanvas() {
        // Set up
        CanvasPanel samplePanel = new CanvasPanel(HUMIDITY, PRECIPITATION, SAMPLE_DATABASE_ID, 1);
        // Execute the method and verify result
        assertEquals(genExpectedResults(HUMIDITY, PRECIPITATION, SAMPLE_DATABASE_ID, 1, EXPECTED_GEOMETRY_POSITIONS),
                samplePanel.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION));
    }

    @Test
    void testConstruct_2HoursAgoTimeCanvas() {
        // Set up
        CanvasPanel samplePanel = new CanvasPanel(HUMIDITY, PRECIPITATION, SAMPLE_DATABASE_ID, 2);
        // Execute the method and verify result
        assertEquals(genExpectedResults(HUMIDITY, PRECIPITATION, SAMPLE_DATABASE_ID, 2, EXPECTED_GEOMETRY_POSITIONS),
                samplePanel.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION));
    }

    @Test
    void testConstruct_LatestTemperature() {
        // Set up
        CanvasPanel samplePanel = new CanvasPanel(1, TEMPERATURE, FEELS_LIKE, SAMPLE_DATABASE_ID);
        // Execute the method and verify result
        assertEquals(genExpectedResults(1, TEMPERATURE, FEELS_LIKE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS),
                samplePanel.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION));
    }

    @Test
    void testConstruct_LatestWindConditions() {
        // Set up
        CanvasPanel samplePanel = new CanvasPanel(2, WIND_DIRECTION, FEELS_LIKE, SAMPLE_DATABASE_ID);
        // Execute the method and verify result
        assertEquals(genExpectedResults(2, WIND_DIRECTION, FEELS_LIKE, SAMPLE_DATABASE_ID, EXPECTED_GEOMETRY_POSITIONS),
                samplePanel.construct(TestUtils.CHART_HEIGHT, TestUtils.ROW_WITH_TWO_CHART_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION));
    }

    public static String genExpectedResults(int panelType, Measure measureOfInterest, Measure feelsLikeMeasure, String databaseId, int[] geometryPositions) {
        String title = "";
        String frameElementSyntax = "";
        String description = "A panel displaying the latest weather conditions for " + measureOfInterest.getName() + ".";
        // Construct the query by retrieving the corresponding columns
        // Retrieve the measure's column only if it exists or else, error should be thrown
        Queue<String[]> measureMetadata = measureOfInterest.getTimeSeriesData();
        String measureOfInterestColumn = "";
        if (!measureMetadata.isEmpty()) {
            measureOfInterestColumn = measureMetadata.poll()[1];
        } else {
            throw new IllegalArgumentException("Missing measure of interest column! Please ensure the data is valid.");
        }
        // Retrieve the feels like column only if it exists or else, error should be thrown
        Queue<String[]> feelsLikeMetadata = feelsLikeMeasure.getTimeSeriesData();
        String feelsLikeColumn = "";
        if (!feelsLikeMetadata.isEmpty()) {
            feelsLikeColumn = feelsLikeMetadata.poll()[1];
        } else {
            throw new IllegalArgumentException("Missing the feels like column! Please ensure the data is valid.");
        }
        String query = "";
        switch (panelType) {
            case 1:
                title = "Latest Temperature";
                query = "SELECT " + measureOfInterestColumn + " AS " + StringHelper.formatEscapeQuoteSQL(StringHelper.WEATHER_STATION_TEMPERATURE_FIELD) + "," +
                        feelsLikeColumn + " AS " + StringHelper.formatEscapeQuoteSQL(StringHelper.WEATHER_STATION_FEELS_LIKE_TEMPERATURE_FIELD) +
                        " FROM \\\"" + feelsLikeMeasure.getTimeSeriesTable() + "\\\" " +
                        "WHERE time BETWEEN TO_TIMESTAMP(${__to}/1000) - INTERVAL '5 hour' AND TO_TIMESTAMP(${__to}/1000) " +
                        "ORDER BY time DESC LIMIT 1";
                frameElementSyntax = CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Temperature Measure", "40", new String[]{"50", "150", "40", "0"},
                        new TextOption(StringHelper.WEATHER_STATION_TEMPERATURE_FIELD, false),
                        new ColorOption("text", "")) + "," +
                        CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Emoji", "50", new String[]{"70", "100", "20", "185"},
                                new TextOption("\uD83C\uDF21", true),
                                new ColorOption("text", "")) + "," +
                        CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Feels Like Text", "25", new String[]{"50", "150", "100", "0"},
                                new TextOption("Feels like:", true),
                                new ColorOption("text", "")) + "," +
                        CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Feels Like Measure", "30", new String[]{"75", "150", "90", "150"},
                                new TextOption(StringHelper.WEATHER_STATION_FEELS_LIKE_TEMPERATURE_FIELD, false),
                                new ColorOption("text", ""));
                break;
            case 2:
                title = "Latest Wind Conditions";
                query = "SELECT CASE WHEN " + measureOfInterestColumn + ">=337.5 OR " + measureOfInterestColumn + "<22.5 THEN '⬆' " +
                        "WHEN " + measureOfInterestColumn + ">=22.5 AND " + measureOfInterestColumn + "<67.5 THEN '↗' " +
                        "WHEN " + measureOfInterestColumn + ">=67.5 AND " + measureOfInterestColumn + "<112.5 THEN '➡' " +
                        "WHEN " + measureOfInterestColumn + ">=112.5 AND " + measureOfInterestColumn + "<157.5 THEN '↘' " +
                        "WHEN " + measureOfInterestColumn + ">=157.5 AND " + measureOfInterestColumn + "<202.5 THEN '⬇' " +
                        "WHEN " + measureOfInterestColumn + ">=202.5 AND " + measureOfInterestColumn + "<247.5 THEN '↙' " +
                        "WHEN " + measureOfInterestColumn + ">=247.5 AND " + measureOfInterestColumn + "<292.5 THEN '⬅' " +
                        "WHEN " + measureOfInterestColumn + ">=292.5 AND " + measureOfInterestColumn + "<337.5 THEN '↖ " +
                        "END AS " + StringHelper.formatEscapeQuoteSQL(StringHelper.WEATHER_STATION_WIND_DIRECTION_FIELD) + "," +
                        feelsLikeColumn + " AS " + StringHelper.formatEscapeQuoteSQL(StringHelper.WEATHER_STATION_WIND_CHILL_FIELD) +
                        " FROM \\\"" + feelsLikeMeasure.getTimeSeriesTable() + "\\\" " +
                        "WHERE time BETWEEN TO_TIMESTAMP(${__to}/1000) - INTERVAL '5 hour' AND TO_TIMESTAMP(${__to}/1000) " +
                        "ORDER BY time DESC LIMIT 1";
                frameElementSyntax = CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Direction", "40", new String[]{"60", "60", "25", "55"},
                        new TextOption(StringHelper.WEATHER_STATION_WIND_DIRECTION_FIELD, false),
                        new ColorOption("text", "")) + "," +
                        CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Direction Text", "16", new String[]{"50", "100", "75", "40"},
                                new TextOption("Direction", true),
                                new ColorOption("text", "")) + "," +
                        CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Emoji", "70", new String[]{"100", "100", "160", "0"},
                                new TextOption("\uD83C\uDF90", true),
                                new ColorOption("text", "")) + "," +
                        CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Feels Like Text", "25", new String[]{"50", "150", "100", "0"},
                                new TextOption("Feels like:", true),
                                new ColorOption("text", "")) + "," +
                        CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Feels Like Measure", "40", new String[]{"50", "170", "110", "130"},
                                new TextOption(StringHelper.WEATHER_STATION_WIND_CHILL_FIELD, false),
                                new ColorOption("text", ""));
                break;
            default:
                throw new IllegalArgumentException("Invalid option: Only 1 or 2 is allowed!");
        }

        return "{" + TestUtils.genExpectedCommonDefaultGrafanaPanelJson(title, description, "canvas", "[]",
                databaseId, StringHelper.WEATHER_STATION_KEY, measureOfInterest, geometryPositions, query) +
                // Field Configuration
                "\"fieldConfig\":{" +
                // Default field configuration
                "\"defaults\":{\"color\":{\"mode\": \"continuous-blues\"}," +
                "\"thresholds\":{\"mode\": \"absolute\"," +
                "\"steps\": [{\"color\":\"green\",\"value\":null}]," +
                "\"mappings\": []," +
                "\"unit\":\"" + UnitMapper.getUnitSyntax(feelsLikeMeasure.getUnit()) + "\"" +
                "}," +// End of defaults
                "\"overrides\": []" +
                "}," + // End of field configuration
                // Options
                "\"options\":{" +
                "\"inlineEditing\":false," +
                "\"showAdvancedTypes\":false," +
                // Root options
                "\"root\":{" +
                "\"background\":{\"color\": {\"fixed\":\"transparent\"}}," +
                "\"border\":{\"color\": {\"fixed\":\"dark-green\"}}," +
                "\"constraint\":{\"horizontal\":\"left\",\"vertical\":\"top\"}," +
                "\"elements\":[" + frameElementSyntax + "]," +
                "\"name\":\"Element\"," +
                "\"placement\":{\"height\":100,\"left\":0,\"top\":0,\"width\":100}," +
                "\"type\":\"frame\"" +
                "}" + // end of root options
                "}" + // end of options
                "}" + // end of field configuration
                "}";
    }

    public static String genExpectedResults(Measure humidityMeasure, Measure precipitationMeasure, String databaseId, int requiredTime, int[] geometryPositions) {
        String title = "";
        String description = "A panel displaying the precipitation and humidity ";
        // Construct the query by retrieving the corresponding columns
        // Retrieve the humidity column only if it exists or else, error should be thrown
        Queue<String[]> humidityMetadata = humidityMeasure.getTimeSeriesData();
        String humidityColumn = "";
        if (!humidityMetadata.isEmpty()) {
            humidityColumn = humidityMetadata.poll()[1];
        } else {
            throw new IllegalArgumentException("Missing humidity column! Please ensure the data is valid.");
        }
        // Retrieve the precipitation column only if it exists or else, error should be thrown
        Queue<String[]> precipitationMetadata = precipitationMeasure.getTimeSeriesData();
        String precipitationColumn = "";
        if (!precipitationMetadata.isEmpty()) {
            precipitationColumn = precipitationMetadata.poll()[1];
        } else {
            throw new IllegalArgumentException("Missing precipitation column! Please ensure the data is valid.");
        }
        String query = "SELECT CONCAT(${__to:date:HH},':', ${__to:date:mm}) AS " + StringHelper.formatEscapeQuoteSQL(StringHelper.WEATHER_STATION_TIMING_FIELD) + "," +
                "CASE WHEN " + precipitationColumn + "<0.2 THEN '☀' " +
                "WHEN " + precipitationColumn + ">=0.2 AND " + precipitationColumn + "<4 THEN '\uD83C\uDF26' " +
                "WHEN " + precipitationColumn + ">=4 AND " + precipitationColumn + "<30 THEN '\uD83C\uDF27' " +
                "WHEN " + precipitationColumn + ">=30 AND " + precipitationColumn + "<100 THEN '\uD83C\uDF27\uD83C\uDF27' END AS " +
                StringHelper.formatEscapeQuoteSQL(StringHelper.WEATHER_STATION_PRECIPITATION_FIELD) + "," +
                humidityColumn + " AS " + StringHelper.formatEscapeQuoteSQL(StringHelper.WEATHER_STATION_HUMIDITY_FIELD) +
                " FROM \\\"" + humidityMeasure.getTimeSeriesTable() + "\\\" " +
                "WHERE time BETWEEN TO_TIMESTAMP(${__to}/1000) - INTERVAL '5 hour' AND TO_TIMESTAMP(${__to}/1000)%s " +
                "ORDER BY time DESC LIMIT 1";
        switch (requiredTime) {
            case 0:
                // When required time is current time
                title = "T-00 (Now)";
                description = description + "at current time.";
                query = String.format(query, ""); // query should not change
                break;
            case 1:
                title = "T-01";
                description = description + "from 1 hour ago.";
                query = String.format(query, "- INTERVAL '" + requiredTime + " hour ' "); // query deduct the corresponding number of hours
                break;
            case 2:
            case 3:
            case 4:
                title = "T-0" + requiredTime;
                description = description + "from " + requiredTime + " hours ago.";
                query = String.format(query, "- INTERVAL '" + requiredTime + " hour ' "); // query deduct the corresponding number of hours
                break;
            default:
                throw new IllegalArgumentException("Invalid option: Only up to four hours ago is allowed!");
        }
        String frameElementSyntax = CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Timing", "50", new String[]{"75", "150", "5", "5"},
                new TextOption(StringHelper.WEATHER_STATION_TIMING_FIELD, false),
                new ColorOption("#ccccdb26", "")) + "," +
                CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Rainfall", "50", new String[]{"75", "150", "70", "0"},
                        new TextOption(StringHelper.WEATHER_STATION_PRECIPITATION_FIELD, false),
                        new ColorOption("text", "")) + "," +
                CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Humidity", "30", new String[]{"75", "100", "140", "15"},
                        new TextOption(StringHelper.WEATHER_STATION_HUMIDITY_FIELD, false),
                        new ColorOption("text", StringHelper.WEATHER_STATION_HUMIDITY_FIELD)) + "," +
                CanvasFrameElementTest.genExpectedCanvasFrameElementSyntax("Droplet", "50", new String[]{"75", "80", "140", "95"},
                        new TextOption("\uD83D\uDCA7", true),
                        new ColorOption("text", ""));
        return "{" + TestUtils.genExpectedCommonDefaultGrafanaPanelJson(title, description, "canvas", "[]",
                databaseId, StringHelper.WEATHER_STATION_KEY, humidityMeasure, geometryPositions, query) +
                // Field Configuration
                "\"fieldConfig\":{" +
                // Default field configuration
                "\"defaults\":{\"color\":{\"mode\": \"continuous-blues\"}," +
                "\"thresholds\":{\"mode\": \"absolute\"," +
                "\"steps\": [{\"color\":\"green\",\"value\":null}]," +
                "\"max\":100,\"min\":0," +
                "\"mappings\": []," +
                "\"unit\":\"" + UnitMapper.getUnitSyntax(humidityMeasure.getUnit()) + "\"" +
                "}," +// End of defaults
                "\"overrides\": []" +
                "}," + // End of field configuration
                // Options
                "\"options\":{" +
                "\"inlineEditing\":false," +
                "\"showAdvancedTypes\":false," +
                // Root options
                "\"root\":{" +
                "\"background\":{\"color\": {\"fixed\":\"transparent\"}}," +
                "\"border\":{\"color\": {\"fixed\":\"dark-green\"}}," +
                "\"constraint\":{\"horizontal\":\"left\",\"vertical\":\"top\"}," +
                "\"elements\":[" + frameElementSyntax + "]," +
                "\"name\":\"Element\"," +
                "\"placement\":{\"height\":100,\"left\":0,\"top\":0,\"width\":100}," +
                "\"type\":\"frame\"" +
                "}" + // end of root options
                "}" + // end of options
                "}" + // end of field configuration
                "}";
    }
}