package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information
 * about the text-value option in the variable syntax specific to Grafana dashboard.
 *
 * @author qhouyee
 */
class TextValueOption {
    private final String IS_SELECTED;
    private final String LABEL;
    private final String VALUE;

    /**
     * Constructor to ingest required data.
     *
     * @param isSelected   A boolean indicating if the current value is selected.
     * @param displayLabel The display label.
     * @param value        The value that Grafana requires to enable proper variable linkage.
     */
    protected TextValueOption(boolean isSelected, String displayLabel, String value) {
        this.IS_SELECTED = String.valueOf(isSelected);
        this.LABEL = displayLabel;
        this.VALUE = value;
    }

    /**
     * Construct the custom variable as a String.
     *
     * @return The variable option syntax as a String.
     */
    protected String construct() {
        StringBuilder builder = new StringBuilder();
        builder.append("{")
                .append("\"selected\": ").append(this.IS_SELECTED).append(",")
                // Display text
                .append("\"text\": \"").append(this.LABEL).append("\",")
                // Value for processing in Grafana
                .append("\"value\": \"").append(this.VALUE).append("\"")
                .append("}");
        return builder.toString();
    }
}
