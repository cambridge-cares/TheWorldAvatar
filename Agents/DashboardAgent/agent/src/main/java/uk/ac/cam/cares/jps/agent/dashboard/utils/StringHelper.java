package uk.ac.cam.cares.jps.agent.dashboard.utils;

/**
 * A class that provides methods to format strings.
 *
 * @author qhouyee
 */
public class StringHelper {
    public static final String FACILITY_KEY = "facilities";
    public static final String ASSET_KEY = "assets";
    public static final String ROOM_KEY = "Rooms";
    public static final String SYSTEM_KEY = "systems";
    public static final String THRESHOLD_KEY = "threshold";
    public static final String SERVICE_ACCOUNT_NAME = "grafana";
    public static final String INTERVAL_VARIABLE_NAME = "Time Interval";


    // Private constructor to prevent instantiation.
    private StringHelper() {
    }

    /**
     * Formats the SPARQL variable name for a SPARQL query syntax. Note that a space will be appended beforehand.
     */
    public static String formatSparqlVarName(String variable) {
        return " ?" + variable;
    }

    /**
     * Formats the variable names to remove white spaces, dashes, and underscores and use only lower cases for Grafana syntax.
     */
    public static String formatVariableName(String variable) {
        return variable.toLowerCase().replaceAll("[\\s\\-_]", "");
    }

    /**
     * Add space between each word, which is defined by having the first letter be capital. For eg, MyTestCase will return My Test Case.
     * Note that abbreviations will not be separated.
     */
    public static String addSpaceBetweenCapitalWords(String input) {
        // Add a space before an uppercase letter followed by a lowercase letter
        String result = input.replaceAll("([a-z])([A-Z])", "$1 $2");
        // Add a space only if capital letters are followed by lower case.
        // Else keep the capital letters together as they are abbreviations: Example ExhaustVAV = Exhaust VAV
        // See test cases for more information
        result = result.replaceAll("([A-Z])([A-Z][a-z])", "$1 $2");
        return result;
    }

    /**
     * Add another single quote to escape single quote in the input for SQL queries.
     */
    public static String addCharacterEscapingForSingleQuotes(String input) {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            if (c == '\'') {
                // Add a backslash behind the single quote
                result.append('\'');
            }
            result.append(c);
        }
        return result.toString();
    }
}
