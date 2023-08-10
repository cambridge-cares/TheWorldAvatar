package uk.ac.cam.cares.jps.agent.dashboard.utils;

import java.util.regex.Pattern;

/**
 * A class that provides methods to format strings.
 *
 * @author qhouyee
 */
public class StringHelper {
    /**
     * Formats the variable names to remove white spaces and use only lower cases for Grafana syntax.
     */
    public static String formatVariableName(String variable) {return variable.toLowerCase().replaceAll("\\s", "");}

    /**
     * Add space between each word, which is defined by having the first letter be capital. For eg, MyTestCase will return My Test Case.
     */
    public static String addSpaceBetweenCapitalWords(String name) {return name.replaceAll("(.)([A-Z])", "$1 $2");}

    /**
     * Removes the UUID of the input string if there is a UUID.
     *
     * @param input The string of interest.
     * @return A string without its UUID.
     */
    public static String removeUUID(String input) {
        Pattern pattern = Pattern.compile("_.+");
        return pattern.matcher(input).replaceAll("");
    }
}
