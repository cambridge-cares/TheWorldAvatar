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
        "forecast unit",
        "measurement parameter",
        "measurement qualifier",
        "quantity",
        "quantity1",
        "quantity2",
        "symbol",
        "measurement symbol",
        "forecast symbol",
        "unit",
        "measured quantities",
        "forecast quantities",
        "forecast created",
        "quantity forecasted",
        "quantity measured"
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
        FILES.put("https://www.theworldavatar.com/kg/ontoems/WaterLevelReportingStation", "ReportingStation");
        FILES.put("https://www.theworldavatar.com/kg/ontoems/ReportingStation", "ReportingStation");

        // CReDo Sites
        FILES.put("http://theworldavatar.com/ontology/ontocredo/ontocredo.owl#Site", "credo");

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