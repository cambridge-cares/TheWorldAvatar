package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.dashboard.TestUtils;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import static org.junit.jupiter.api.Assertions.*;

public class VariablePanelTest {
    public static final String INTERVAL_DESCRIPTION = "Select the required time interval for the current and last period trends on the right.";
    public static final String REF_MONTH_DESCRIPTION = "Select the reference month for comparison with the current month through the charts on the right.";
    private static final int SAMPLE_PANEL_HEIGHT = 8;
    private static final int SAMPLE_PANEL_WIDTH = 12;
    private static final int SAMPLE_PANEL_X_POSITION = 1;
    private static final int SAMPLE_PANEL_Y_POSITION = 0;

    @Test
    void testConstruct() {
        // Generate expected inputs
        int[] expectedGeometryPosition = new int[]{SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION};
        // Construct the object
        VariablePanel chart = new VariablePanel(StringHelper.INTERVAL_VARIABLE_NAME, INTERVAL_DESCRIPTION);
        // Execute the method
        String result = chart.construct(SAMPLE_PANEL_HEIGHT, SAMPLE_PANEL_WIDTH, SAMPLE_PANEL_X_POSITION, SAMPLE_PANEL_Y_POSITION);
        // Verify result
        assertEquals(genExpectedResults(StringHelper.INTERVAL_VARIABLE_NAME, INTERVAL_DESCRIPTION, expectedGeometryPosition), result);
    }

    public static String genExpectedResults(String variable, String description, int[] geometryPositions) {
        // Construct the expected syntax
        return "{" + TestUtils.genExpectedCommonTemplatePanelJson(variable, description, "volkovlabs-variable-panel", geometryPositions) +
                // Field Configuration
                "\"fieldConfig\": { " +
                // Default field configuration
                "\"defaults\": {\"thresholds\":{\"mode\": \"absolute\", \"steps\": [{\"color\":\"green\",\"value\":null},{\"color\":\"red\",\"value\":80}]}" +
                "}," + // End of defaults
                "\"overrides\": []" +
                "}," + // End of field configuration
                // Options
                "\"options\":{" +
                "\"displayMode\":\"table\"," +
                "\"padding\":10," +
                "\"emptyValue\":false," +
                "\"persistent\":false," +
                "\"sticky\":false," +
                "\"autoScroll\":false," +
                "\"header\":false," +
                "\"filter\":false," +
                "\"alwaysVisibleFilter\":false," +
                "\"favorites\":false," +
                "\"statusSort\":false," +
                "\"showName\":false," +
                "\"groupSelection\":false," +
                "\"variable\": \"" + StringHelper.formatVariableName(variable) + "\"" +
                "}" + // end of options
                "}";
    }
}