package com.cmclinnovations.featureinfo;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.cmclinnovations.featureinfo.queries.AbstractQuery;

/**
 * Misc hardcoded lookups
 */
public class Lookups {
    
    /**
     * Map of class names to AbstractQuery classes
     */
    public static Map<String, Class<? extends AbstractQuery>> CLASSES = new HashMap<>();

    /**
     * Keys not to show in output meta JSON
     */
    public static Set<String> HIDDEN_META = new HashSet<>(Arrays.asList(
        "measurement",
        "id"
    ));

    /**
     * Initialiser
     */
    static {
        // Classes for querying
        // CLASSES.put("WaterLevelReportingStation", ReportingStationQuery.class);
        // CLASSES.put("ReportingStation", ReportingStationQuery.class);
        // CLASSES.put("Station", ReportingStationQuery.class);
    }

}