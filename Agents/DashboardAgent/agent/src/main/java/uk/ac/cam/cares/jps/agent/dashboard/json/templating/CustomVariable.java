package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information
 * about custom template variable syntax specific to Grafana dashboard. At the moment,
 * the custom variables are used to filter between various asset types
 * and between individual assets from specific asset type.
 *
 * @author qhouyee
 */
class CustomVariable {
    private final String NAME;
    private final String LABEL;
    private final String DESCRIPTION;
    private final String DASHBOARD_DISPLAY_OPTION;
    private final StringBuilder VARIABLE_SELECTION_OPTIONS = new StringBuilder();
    private final StringBuilder QUERY_SYNTAX = new StringBuilder();
    // An indicator to support the generation of a custom variable for filtering asset types
    private static final String IS_ASSET_TYPE = "Assets";

    /**
     * Constructor for filtering asset types. Only used this for that purpose.
     *
     * @param values                 An array of values to be included into the query component. Tentatively, this is a value of all available asset types or individual assets within one type.
     * @param dashboardDisplayOption The display options for the variable on the dashboard by Grafana. 0 - Display both label and values; 1 - Display only value; 2 - Display nothing.
     */
    public CustomVariable(String[] values, Integer dashboardDisplayOption) {
        this(IS_ASSET_TYPE, values, dashboardDisplayOption);
    }

    /**
     * Basic Constructor that provides customised settings.
     *
     * @param name                   The name of the assets to create for this variable.
     * @param values                 An array of values to be included into the query component. Tentatively, this is a value of all available asset types or individual assets within one type.
     * @param dashboardDisplayOption The display options for the variable on the dashboard by Grafana. 0 - Display both label and values; 1 - Display only value; 2 - Display nothing.
     */
    public CustomVariable(String name, String[] values, Integer dashboardDisplayOption) {
        // Transform name into lower cases and remove all white spaces
        this.NAME = name.toLowerCase().replaceAll("\\s", "");
        // If it is for filtering asset types, just keep a similar label
        this.LABEL = name.equals(IS_ASSET_TYPE) ? IS_ASSET_TYPE :
                // Otherwise add white space before each capital letter for the label, and add " Assets" at the end
                name.replaceAll("(.)([A-Z])", "$1 $2") + " assets";
        // Description should follow label
        this.DESCRIPTION = "Default filters for the " + this.LABEL;
        // Create a default option for all values
        TextValueOption option = new TextValueOption(true, "All", "$_all");
        this.VARIABLE_SELECTION_OPTIONS.append(option.construct());
        // Append each value in the array in the required format
        for (String value : values) {
            // Only append a comma before if it is not the first value
            if (this.QUERY_SYNTAX.length() != 0) this.QUERY_SYNTAX.append(",");
            this.QUERY_SYNTAX.append(value);
            // Append the individual option for these values
            option = new TextValueOption(false, value, value);
            // Requires a comma before as the first option is All selected
            this.VARIABLE_SELECTION_OPTIONS.append(",").append(option.construct());
        }
        this.DASHBOARD_DISPLAY_OPTION = dashboardDisplayOption.toString();
    }

    /**
     * Construct the custom variable as a String.
     *
     * @return The custom variable syntax as a String.
     */
    protected String construct() {
        StringBuilder builder = new StringBuilder();
        builder.append("{")
                // Default selection should be all
                .append("\"current\": {")
                .append("\"selected\": false,")
                .append("\"text\": [\"All\"],")
                .append("\"value\": [\"$__all\"]")
                .append("},")
                // Variable name
                .append("\"name\": \"").append(this.NAME).append("\",")
                // Variable display label
                .append("\"label\": \"").append(this.LABEL).append("\",")
                // Description for this variable
                .append("\"description\": \"").append(this.DESCRIPTION).append("\",")
                // Include option to select all values
                .append("\"includeAll\": true,")
                // Allow multiple value selection eg Value 1 and 2 can be selected but not Value 3
                .append("\"multi\": true,")
                // The display option for this variable
                .append("\"hide\": ").append(this.DASHBOARD_DISPLAY_OPTION).append(",")
                // Array of variable text/value pairs available for selection on dashboard
                .append("\"options\": [").append(this.VARIABLE_SELECTION_OPTIONS).append("],")
                // Query values of this variable
                .append("\"query\": \"").append(this.QUERY_SYNTAX).append("\",")
                // Default settings but unsure what they are for
                .append("\"queryValue\": \"\",")
                .append("\"skipUrlSync\": false,")
                // Variable type must be set as custom to work
                .append("\"type\": \"custom\"")
                .append("}");
        return builder.toString();
    }
}
