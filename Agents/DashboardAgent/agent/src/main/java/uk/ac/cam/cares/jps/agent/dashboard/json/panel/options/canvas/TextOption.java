package uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.canvas;

/**
 * A helper to create the json syntax of each element's text options on the canvas frame.
 *
 * @author qhouyee
 */
public class TextOption {
    private final String valueOrField;
    private final boolean isFixed;

    /**
     * Constructor to set either a fixed text input or generated from a dynamic field.
     *
     * @param valueOrField The fixed text input or dynamic field.
     * @param isFixed      Indicates if this should be the fixed text input or not.
     */
    public TextOption(String valueOrField, boolean isFixed) {
        this.valueOrField = valueOrField;
        this.isFixed = isFixed;
    }

    /**
     * Construct the data source into the Grafana compliant JSON syntax.
     *
     * @return The JSON Data source syntax as a String.
     */
    public String construct() {
        if (this.isFixed) {
            return "{\"fixed\":\"" + this.valueOrField + "\"}";
        } else {
            return "{\"field\":\"" + this.valueOrField + "\",\"fixed\":\"\",\"mode\":\"field\"}";
        }
    }
}
