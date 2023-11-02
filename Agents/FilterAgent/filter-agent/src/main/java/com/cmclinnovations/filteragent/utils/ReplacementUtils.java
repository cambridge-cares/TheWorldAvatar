package com.cmclinnovations.filteragent.utils;

import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang.StringUtils;

public class ReplacementUtils {

    private ReplacementUtils() {
    }

    public static String userReplacements(Map<String, String> subsMap, String query) {
        for (Entry<String, String> subMap : subsMap.entrySet()) {
            query = StringUtils.replace(query, "[" + subMap.getKey() + "]", subMap.getValue());
        }
        return query;
    }
}
