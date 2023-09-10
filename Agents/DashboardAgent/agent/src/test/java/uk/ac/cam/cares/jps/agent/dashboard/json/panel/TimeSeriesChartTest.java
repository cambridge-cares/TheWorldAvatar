package uk.ac.cam.cares.jps.agent.dashboard.json.panel;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class TimeSeriesChartTest {
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
    void testConstructNoUnit() {
        // Generate expected inputs
        String[] expectedConfigItems = new String[]{SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_ASSET_TABLE_NAME, SAMPLE_DATABASE_ID};
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        TimeSeriesChart chart = new TimeSeriesChart(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, "null", SAMPLE_DATABASE_ID, SAMPLE_METADATA);
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
        TimeSeriesChart chart = new TimeSeriesChart(SAMPLE_MEASURE, SAMPLE_ITEM_GROUP, SAMPLE_UNIT, SAMPLE_DATABASE_ID, SAMPLE_METADATA);
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify results
        assertEquals(genExpectedResults(expectedConfigItems, expectedGeometryPosition, SAMPLE_METADATA), result);
    }

    private static String genExpectedResults(String[] metadata, int[] geometryPositions, List<String[]> itemDetails) {
        String titleContent = StringHelper.addSpaceBetweenCapitalWords(metadata[0]) + " of " + StringHelper.addSpaceBetweenCapitalWords(metadata[1]);
        titleContent = metadata.length > 4 ? titleContent + " [" + metadata[4] + "]" : titleContent;
        String description = "A chart displaying the time series of " + metadata[0].toLowerCase() + " for " + metadata[1].toLowerCase();
        StringBuilder sb = new StringBuilder();
        sb.append("{").append(TemplatePanelTest.genExpectedCommonJsonBase(titleContent, description, metadata, geometryPositions, itemDetails))
                .append(",\"type\": \"timeseries\",")
                .append("\"fieldConfig\": { ")
                .append("\"defaults\": {\"color\": {\"mode\": \"palette-classic\"},")
                .append("\"custom\":{").append("\"axisCenteredZero\":false,\"axisColorMode\":\"text\",")
                .append("\"axisLabel\":\"\",\"axisPlacement\":\"auto\", \"barAlignment\":0, \"drawStyle\":\"line\",")
                .append("\"fillOpacity\":0,\"gradientMode\":\"none\",")
                .append("\"hideFrom\":{\"legend\":false, \"tooltip\":false, \"viz\":false},")
                .append("\"lineInterpolation\":\"linear\", \"lineWidth\":1, \"pointSize\":5,")
                .append("\"scaleDistribution\":{\"type\":\"linear\"}, \"showPoints\":\"auto\", \"spanNulls\":false,")
                .append("\"stacking\":{\"group\":\"A\", \"mode\":\"none\"}, \"thresholdsStyle\":{\"mode\":\"off\"}")
                .append("},")
                .append("\"mappings\": []")
                .append("},")
                .append("\"overrides\": []")
                .append("},")
                .append("\"options\":{")
                .append("\"legend\":{\"calcs\": [], \"displayMode\":\"list\",\"placement\":\"bottom\",\"showLegend\":true},")
                .append("\"tooltip\":{\"mode\":\"single\",\"sort\":\"none\"}")
                .append("}")
                .append("}");
        return sb.toString();
    }
}