package uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.canvas;

/**
 * A helper to create the json syntax of each element's color options on the canvas frame.
 *
 * @author qhouyee
 */
public class ColorOption {
    private final String fixedField;
    private final String colorField;

    /**
     * Constructor to set either a static or dynamic display color following the field value.
     * For static consistent colors, only pass in a color for the fixed value with an empty string for field.
     * For dynamic colors, pass in text for the fixed value and the field name for field.
     *
     * @param fixedValue A fixed value that can either be a static color or `text` for dynamic colors.
     * @param field      The displayed color will follow the field value on the gradient color palette selected. Keep this as an empty String if you require a set color.
     */
    public ColorOption(String fixedValue, String field) {
        this.fixedField = fixedValue;
        this.colorField = field;
    }

    /**
     * Construct the data source into the Grafana compliant JSON syntax.
     *
     * @return The JSON Data source syntax as a String.
     */
    public String construct() {
        if (this.fixedField.equals("text") && !this.colorField.isEmpty()) {
            return "{\"field\":\"" + this.colorField + "\",\"fixed\":\"text\"}";
        } else {
            return "{\"fixed\":\"" + this.fixedField + "\"}";
        }
    }
}
