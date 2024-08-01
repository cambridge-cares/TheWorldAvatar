package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information
 * about the text-value option in the variable syntax specific to Grafana dashboard.
 *
 * @author qhouyee
 */
class TextValueOption {
    private final String isSelected;
    private final String label;
    private final String value;

    /**
     * Constructor to ingest required data.
     *
     * @param isSelected   A boolean indicating if the current value is selected.
     * @param displayLabel The display label.
     * @param value        The value that Grafana requires to enable proper variable linkage.
     */
    protected TextValueOption(boolean isSelected, String displayLabel, String value) {
        this.isSelected = String.valueOf(isSelected);
        this.label = displayLabel;
        this.value = value;
    }

    /**
     * Construct the custom variable as a String.
     *
     * @return The variable option syntax as a String.
     */
    protected String construct() {
        return "{" +
                "\"selected\": " + this.isSelected + "," +
                // Display text
                "\"text\": \"" + this.label + "\"," +
                // Value for processing in Grafana
                "\"value\": \"" + this.value + "\"" +
                "}";
    }
}
