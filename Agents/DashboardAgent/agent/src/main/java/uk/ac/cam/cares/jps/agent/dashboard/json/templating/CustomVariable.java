package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information
 * about custom template variable syntax specific to Grafana dashboard. At the moment,
 * the custom variables are used to filter between various asset types
 * and between individual assets from specific asset type.
 *
 * @author qhouyee
 */
class CustomVariable extends TemplateVariable {
    private final String LABEL;
    private final String DESCRIPTION;
    private final StringBuilder VARIABLE_SELECTION_OPTIONS = new StringBuilder();
    private final StringBuilder QUERY_SYNTAX = new StringBuilder();

    /**
     * A Constructor that provides customised settings and defaults to true for the selectAllOption and multivalue slections.
     *
     * @param name                   The name of the assets or rooms to create for this variable.
     * @param description            The description of this variable
     * @param values                 An array of values to be included into the query component. Tentatively, this is a value of all available asset types, rooms or individual assets within one type.
     * @param dashboardDisplayOption The display options for the variable on the dashboard by Grafana. 0 - Display both label and values; 1 - Display only value; 2 - Display nothing.
     */
    protected CustomVariable(String name, String description, String[] values, Integer dashboardDisplayOption) {
        this(name, description, values, dashboardDisplayOption, true, true);
    }

    /**
     * Basic Constructor that provides customised settings.
     *
     * @param name                   The name of the assets or rooms to create for this variable.
     * @param description            The description of this variable
     * @param values                 An array of values to be included into the query component. Tentatively, this is a value of all available asset types, rooms or individual assets within one type.
     * @param dashboardDisplayOption The display options for the variable on the dashboard by Grafana. 0 - Display both label and values; 1 - Display only value; 2 - Display nothing.
     * @param isMultiValue           A boolean to indicate if multiple values are allowed for the variable in Grafana.
     * @param selectAllOption        A boolean to indicate if the "All" option should be enabled in Grafana.
     */
    protected CustomVariable(String name, String description, String[] values, Integer dashboardDisplayOption, boolean isMultiValue, boolean selectAllOption) {
        // Construct the super class
        super(name, dashboardDisplayOption, isMultiValue, selectAllOption);
        this.LABEL = StringHelper.addSpaceBetweenCapitalWords(name);
        this.DESCRIPTION = description;
        // A boolean to indicate if the option is the default selected
        boolean isDefaultOption = true;
        TextValueOption option;
        // If requiring all selection options, please
        if (selectAllOption) {
            // Create a default option for all values
            option = new TextValueOption(isDefaultOption, "All", "$__all");
            this.VARIABLE_SELECTION_OPTIONS.append(option.construct());
            isDefaultOption = false;
        }
        // Append each value in the array in the required format
        for (String value : values) {
            // Only append a comma before if it is not the first value
            if (this.QUERY_SYNTAX.length() != 0) this.QUERY_SYNTAX.append(",");
            this.QUERY_SYNTAX.append(value);
            // Append the individual option for these values
            option = new TextValueOption(isDefaultOption, value, value);
            if (isDefaultOption) {
                super.setDefaultSelectedTextValue(value);
                isDefaultOption = false;
            }
            // Add a comma as this is not the first item
            else {this.VARIABLE_SELECTION_OPTIONS.append(",");}
            this.VARIABLE_SELECTION_OPTIONS.append(option.construct());
        }
    }

    /**
     * Construct the custom variable as a String.
     *
     * @return The custom variable syntax as a String.
     */
    @Override
    protected String construct() {
        // Construct the common elements
        StringBuilder builder = super.genCommonJson()
                // Variable display label
                .append("\"label\": \"").append(this.LABEL).append("\",")
                // Description for this variable
                .append("\"description\": \"").append(this.DESCRIPTION).append("\",")
                // Array of variable text/value pairs available for selection on dashboard
                .append("\"options\": [").append(this.VARIABLE_SELECTION_OPTIONS).append("],")
                // Query values of this variable
                .append("\"query\": \"").append(this.QUERY_SYNTAX).append("\",")
                // Default settings but unsure what they are for
                .append("\"queryValue\": \"\",")
                // Variable type must be set as custom to work
                .append("\"type\": \"custom\"")
                .append("}");
        return builder.toString();
    }
}
