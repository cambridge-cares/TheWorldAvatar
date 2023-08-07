package uk.ac.cam.cares.jps.agent.dashboard.utils;

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
}
