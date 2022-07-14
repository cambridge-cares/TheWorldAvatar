package com.cmclinnovations.featureinfo;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Misc hardcoded lookups
 */
public class Lookups {
    
    /**
     * Map of class IRIs to query files
     */
    public static Map<String, String> FILES = new HashMap<>();

    /**
     * Keys not to show in output meta JSON
     */
    public static Set<String> HIDDEN_META = new HashSet<>(Arrays.asList(
        "measurement",
        "id",
        "report",
        "forecast",
        "measurement unit",
        "measurement parameter",
        "measurement qualifier"
    ));

    /**
     * Units IRIs to labels.
     */
    public static Map<String, String> UNITS = new HashMap<>();

    /**
     * Initialiser.
     */
    static {
        // Environmental Measurement Stations
        FILES.put("https://www.theworldavatar.com/kg/ontoems/WaterLevelReportingStation", "WaterLevelReportingStation");
        FILES.put("https://www.theworldavatar.com/kg/ontoems/ReportingStation", "ReportingStation");

        // Units
        UNITS.put("http://www.ontology-of-units-of-measure.org/resource/om-2/degree", "\\u00B0");
        UNITS.put("http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius", "\\u2103");
        UNITS.put("http://www.ontology-of-units-of-measure.org/resource/om-2/hectopascal", "hPa");
        UNITS.put("http://www.ontology-of-units-of-measure.org/resource/om-2/metre", "m");
        UNITS.put("http://www.ontology-of-units-of-measure.org/resource/om-2/mile-StatutePerHour", "mph");
        UNITS.put("http://www.ontology-of-units-of-measure.org/resource/om-2/one", "-");
        UNITS.put("http://www.ontology-of-units-of-measure.org/resource/om-2/percent", "%");
    }

}