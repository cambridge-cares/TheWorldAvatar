package com.cmclinnovations.featureinfo.core.meta;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

/**
 * This class is responsible for formatting and combining the raw JSONArrays
 * returned from the KG into a single set of usable metadata.
 */
public class MetaParser {
    
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(MetaParser.class);

    /**
     * Allowable names for query result columns.
     */
    private static final String[] KEYS = new String[]{"Property", "Value", "Unit"};

    /**
     * Constructor.
     */
    private MetaParser() {
        // No.
    }

    /**
     * Format the input raw JSONArrays into a single array of usable metadata.
     * 
     * @param rawResults raw results from KGs.
     * 
     * @return single metadata object.
     */
    public static JSONArray formatData(List<JSONArray> rawResults) {
        // Collate under common property names
        Map<String, List<String>> parsedProperties = new LinkedHashMap<>();

        rawResults.forEach(rawResult -> {
            for(int i = 0; i < rawResult.length(); i++) {
                JSONObject entry = rawResult.getJSONObject(i);

                Object property = findField(KEYS[0], entry);
                Object value = findField(KEYS[1], entry);
                Object unit = findField(KEYS[2], entry);

                if(!parsedProperties.containsKey(property)) {
                    parsedProperties.put(property.toString(), new ArrayList<>());
                }

                if(value != null && unit != null) {
                    value = value.toString() + " [" + unit.toString() + "]";
                    parsedProperties.get(property).add(value.toString());
                } else if (value != null) {
                    parsedProperties.get(property).add(value.toString());
                }
            }
        });

        // Combine into single JSON object
        JSONArray finalResult = new JSONArray();
        for(Map.Entry<String, List<String>> entry : parsedProperties.entrySet()) {

            if(entry.getValue().size() > 1) {
                JSONArray array = new JSONArray();
                entry.getValue().forEach(value -> array.put(value));

                JSONObject object = new JSONObject();
                object.put(entry.getKey(), array);
                finalResult.put(object);
            } else {
                JSONObject object = new JSONObject();
                object.put(entry.getKey(), entry.getValue().get(0));
                finalResult.put(object);
            }
        }
        return finalResult;
    }

    /**
     * Find the JSON field for the input key in any case.
     * 
     * @param key JSON key.
     * @param entry JSONObject to search within.
     * 
     * @return Resulting value (or null).
     */
    private static Object findField(String key, JSONObject entry) {
        if(entry.has(key)) return entry.get(key);
        if(entry.has(key.toLowerCase())) return entry.get(key.toLowerCase());
        if(entry.has(key.toUpperCase())) return entry.get(key.toUpperCase());
        
        return null;
    }

}
// End of class.