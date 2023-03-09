package uk.ac.cam.cares.jps.bio.json.parser;

import com.jayway.jsonpath.JsonPath;

/**
 * Parses the supplied input parameters as JSON Object. This step has to be performed so that
 * the provided inputs can be extracted.
 */

public class JSonRequestParser {
    public static String getUPPER_LIMITS(String jsonString) {
        return JsonPath.read(jsonString, "$.job.upper_bounds");
    }

    public static String getLOWER_LIMITS(String jsonString) {
        return JsonPath.read(jsonString, "$.job.lower_bounds");
    }

    public static String getEQUIP_COST(String jsonString) {
        return JsonPath.read(jsonString, "$.job.equip_cost");
    }
}