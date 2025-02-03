package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information
 * about the variable panel syntax designed by Volkov Labs for Grafana.
 *
 * @author qhouyee
 */
public class VariablePanel extends TemplatePanel {
    private final String variable;

    /**
     * Standard Constructor.
     *
     * @param variableName The variable name that will be displayed as the title and for referencing the template variable.
     * @param description  Description of the panel for users to read.
     */
    public VariablePanel(String variableName, String description) {
        super("volkovlabs-variable-panel");
        this.variable = StringHelper.formatVariableName(variableName);
        super.setTitle(variableName);
        super.setDescription(description);
    }

    /**
     * Construct the Variable Panel syntax as a String.
     *
     * @param height    Height of the panel.
     * @param width     Width of the panel.
     * @param xPosition X position within the dashboard.
     * @param yPosition Y position within the dashboard.
     * @return The Variable Panel syntax as a String.
     */
    @Override
    public String construct(int height, int width, int xPosition, int yPosition) {
        return "{" + super.genCommonJson(height, width, xPosition, yPosition) +
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
                "\"variable\": \"" + this.variable + "\"" +
                "}" + // end of options
                "}";
    }
}
