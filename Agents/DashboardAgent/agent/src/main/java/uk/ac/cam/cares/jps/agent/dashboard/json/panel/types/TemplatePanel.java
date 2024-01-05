package uk.ac.cam.cares.jps.agent.dashboard.json.panel.types;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information about template panel syntax
 * specific to Grafana dashboard. This is a super class that is intended to be implemented by the subclass, and only provide common syntax.
 *
 * @author qhouyee
 */
public abstract class TemplatePanel {
    private String title;
    private String description;
    private final String panelType;

    /**
     * A constructor that sets up common aspects of the panels.
     *
     * @param panelType The panel type that has to be included for the dashboard.
     */
    protected TemplatePanel(String panelType) {
        this.panelType = panelType;
    }

    /**
     * Sets the title.
     */
    protected void setTitle(String title) {
        this.title = title;
    }

    /**
     * Sets the description.
     */
    protected void setDescription(String description) {
        this.description = description;
    }

    /**
     * Construct the common JSON parts for panel as a StringBuilder which will continue to append specific syntax for different query types.
     *
     * @return The variable syntax as a StringBuilder.
     */
    protected String genCommonJson(int height, int width, int xPosition, int yPosition) {
        verifyVariable(this.title, "Title");
        verifyVariable(this.description, "Description");
        return "\"id\": null," +  // Generate a new ID for this panel
                // Title of this panel
                "\"title\": \"" + this.title + "\"," +
                // Description for this panel
                "\"description\": \"" + this.description + "\"," +
                "\"type\": \"" + this.panelType + "\"," +
                // Grid position: Two panels of height:4 and width:6 will be found at (0,0), (6,4) coordinates
                // Height and width are dimensions of the panel
                "\"gridPos\":{\"h\":" + height + ",\"w\":" + width +
                // x and y are the placement position within the dashboard/grid
                ",\"x\":" + xPosition + ",\"y\":" + yPosition + "},";
    }

    /**
     * An abstract method to construct the variable syntax required. This method must be overridden to be executed in the implemented classes.
     */
    public abstract String construct(int height, int width, int xPosition, int yPosition);

    /**
     * Verifies if the mandatory variable exists.
     *
     * @throws NullPointerException when variable is empty.
     */
    private void verifyVariable(String variable, String varType) {
        if (variable == null) {
            throw new NullPointerException(varType + " has not yet been set or is empty!");
        }
    }
}
