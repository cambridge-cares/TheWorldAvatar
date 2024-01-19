package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information about template variable syntax
 * specific to Grafana dashboard. This is a super class that is intended to be implemented by the subclass, and only provide common syntax.
 *
 * @author qhouyee
 */
class TemplateVariable {
    private String defaultSelectedText;
    private String defaultSelectedValue;
    private final String name;
    private final String dashboardDisplayOption;
    private final boolean isMultiOption;
    private final boolean includeAllOption;

    /**
     * Standard Constructor.
     *
     * @param name                   The name of the assets to create for this variable.
     * @param dashboardDisplayOption The display options for the variable on the dashboard by Grafana. 0 - Display both label and values; 1 - Display only value; 2 - Display nothing.
     * @param isMultiValue           A boolean to indicate if multiple values are allowed for the variable in Grafana.
     * @param includeAllOption       A boolean to indicate if the "All" option should be enabled in Grafana.
     */
    protected TemplateVariable(String name, Integer dashboardDisplayOption, boolean isMultiValue, boolean includeAllOption) {
        // Transform name into lower cases and remove all white spaces
        this.name = StringHelper.formatVariableName(name);
        this.dashboardDisplayOption = dashboardDisplayOption.toString();
        this.isMultiOption = isMultiValue;
        this.includeAllOption = includeAllOption;
        this.defaultSelectedText = "All";
        this.defaultSelectedValue = "$__all";
    }


    /**
     * Construct the common JSON parts for variable as a StringBuilder which will continue to append specific syntax for different query types.
     *
     * @return The variable syntax as a string.
     */
    protected String genCommonJson() {
        return "{" +
                // Default selection should be all
                "\"current\": {" +
                "\"selected\": false," +
                "\"text\": [\"" + this.defaultSelectedText + "\"]," +
                "\"value\": [\"" + this.defaultSelectedValue + "\"]" +
                "}," +
                // Variable name
                "\"name\": \"" + this.name + "\"," +
                // Include option to select all values
                "\"includeAll\": " + includeAllOption + "," +
                // Allow multiple value selection if true eg Value 1 and 2 can be selected but not Value 3
                "\"multi\":" + this.isMultiOption + "," +
                // The display option for this variable
                "\"hide\": " + this.dashboardDisplayOption + "," +
                "\"skipUrlSync\": false,";
    }

    /**
     * Sets the default selected text and value.
     */
    protected void setDefaultSelectedTextValue(String defaultValue) {
        this.defaultSelectedText = defaultValue;
        this.defaultSelectedValue = defaultValue;
    }

    /**
     * A placeholder method to construct the variable syntax required. This method must be overridden to be executed in the implemented classes.
     */
    protected String construct() {
        throw new UnsupportedOperationException("Construct() method is not supported for TemplateVariable. Please use their implementation classes instead!");
    }
}
