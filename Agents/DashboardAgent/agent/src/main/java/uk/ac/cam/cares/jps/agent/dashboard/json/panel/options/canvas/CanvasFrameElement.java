package uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.canvas;

/**
 * A helper to create the json syntax of an element for the canvas frame.
 *
 * @author qhouyee
 */
public class CanvasFrameElement {
    private final String elementName;
    private final String fontSize;
    private final ColorOption fontColorOption;
    private final TextOption textOption;

    /**
     * Constructor.
     *
     * @param elementName     The name for this element.
     * @param textOption      The text option for this element.
     * @param fontColorOption The font color option for this element.
     * @param fontSize        The size of the font for this element.
     */
    public CanvasFrameElement(String elementName, TextOption textOption, ColorOption fontColorOption, String fontSize) {
        this.textOption = textOption;
        this.fontColorOption = fontColorOption;
        this.fontSize = fontSize;
        this.elementName = elementName;
    }

    /**
     * Construct the data source into the Grafana compliant JSON syntax.
     *
     * @return The JSON Data source syntax as a String.
     */
    public String construct(String height, String width, String topPlacement, String leftPlacement) {
        return "{" +
                "\"background\":{\"color\": {\"fixed\":\"transparent\"}}," +
                "\"border\":{\"color\": {\"fixed\":\"dark-green\"}}," +
                "\"config\":{" +
                "\"align\":\"center\"," +
                "\"color\":" + this.fontColorOption.construct() + "," +
                "\"size\":" + this.fontSize + "," +
                "\"text\":" + this.textOption.construct() + "," +
                "\"valign\": \"middle\"" +
                "}," +
                "\"constraint\":{\"horizontal\":\"left\",\"vertical\":\"top\"}," +
                "\"name\":\"" + this.elementName + "\"," +
                "\"placement\":{\"height\":" + height + ",\"width\":" + width + ",\"top\":" + topPlacement + ",\"left\":" + leftPlacement + "}," +
                "\"type\":\"metric-value\"" +
                "}";
    }
}
