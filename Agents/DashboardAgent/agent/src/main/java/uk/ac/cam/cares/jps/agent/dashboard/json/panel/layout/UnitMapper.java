package uk.ac.cam.cares.jps.agent.dashboard.json.panel.layout;

import java.util.*;

/**
 * A class that maps unit to their grafana syntax.
 *
 * @author qhouyee
 */
public class UnitMapper {
    private static final HashMap<String, String> UNIT_MAP = new HashMap<>();

    // Private constructor to prevent instantiation.
    private UnitMapper() {
    }

    // Initialise the mapping in a static block or a static method
    static {
        UNIT_MAP.put("kwh", "kwatth");
        UNIT_MAP.put("°c", "celsius");
        UNIT_MAP.put("%", "percent");
        UNIT_MAP.put("null", "");
    }

    // Method to get the string syntax for a given unit
    public static String getUnitSyntax(String unit) {
        // Get the unit syntax for Grafana if it exists in the map. Otherwise, simply return the unit input
        return UNIT_MAP.getOrDefault(unit.toLowerCase(), unit);
    }
}
