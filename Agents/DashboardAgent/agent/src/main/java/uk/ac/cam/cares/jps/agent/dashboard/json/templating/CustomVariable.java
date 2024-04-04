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
    private final String label;
    private final String description;
    private final StringBuilder variableSelectionOptions = new StringBuilder();
    private final StringBuilder querySyntax = new StringBuilder();

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
        this.label = StringHelper.addSpaceBetweenCapitalWords(name);
        this.description = description;
        // A boolean to indicate if the option is the default selected
        boolean isDefaultOption = true;
        TextValueOption option;
        // If requiring all selection options, please
        if (selectAllOption) {
            // Create a default option for all values
            option = new TextValueOption(isDefaultOption, "All", "$__all");
            this.variableSelectionOptions.append(option.construct());
            isDefaultOption = false;
        }
        // Append each value in the array in the required format
        for (String value : values) {
            // Only append a comma before if it is not the first value
            if (this.querySyntax.length() != 0) this.querySyntax.append(",");
            this.querySyntax.append(value);
            // Append the individual option for these values
            option = new TextValueOption(isDefaultOption, value, value);
            if (isDefaultOption) {
                super.setDefaultSelectedTextValue(value);
                isDefaultOption = false;
            }
            // Add a comma as this is not the first item
            else {this.variableSelectionOptions.append(",");}
            this.variableSelectionOptions.append(option.construct());
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
        return super.genCommonJson() +
                // Variable display label
                "\"label\": \"" + this.label + "\"," +
                // Description for this variable
                "\"description\": \"" + this.description + "\"," +
                // Array of variable text/value pairs available for selection on dashboard
                "\"options\": [" + this.variableSelectionOptions + "]," +
                // Query values of this variable
                "\"query\": \"" + this.querySyntax + "\"," +
                // Default settings but unsure what they are for
                "\"queryValue\": \"\"," +
                // Variable type must be set as custom to work
                "\"type\": \"custom\"" +
                "}";
    }
}
