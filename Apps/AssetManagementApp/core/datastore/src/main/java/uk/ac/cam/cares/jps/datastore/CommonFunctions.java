package uk.ac.cam.cares.jps.datastore;

import androidx.datastore.preferences.core.Preferences;

import java.util.HashMap;
import java.util.Map;

public class CommonFunctions {
    static Map<String, String> convertPreferencesToStringMap(Preferences preferences) {
        Map<String, String> results = new HashMap<>();
        Map<Preferences.Key<?>, Object> preferencesMap =  preferences.asMap();
        for (Preferences.Key key : preferencesMap.keySet()) {
            results.put(key.toString(), preferencesMap.get(key).toString());
        }
        return results;
    }

    static Map<String, Boolean> convertPreferencesToBooleanMap(Preferences preferences) {
        Map<String, Boolean> results = new HashMap<>();
        Map<Preferences.Key<?>, Object> preferencesMap =  preferences.asMap();
        for (Preferences.Key key : preferencesMap.keySet()) {
            results.put(key.toString(), preferencesMap.get(key).toString().equals("true")? Boolean.TRUE : Boolean.FALSE);
        }
        return results;
    }

    static Map<String, Integer> convertPreferencesToIntegerMap(Preferences preferences) {
        Map<String, Integer> results = new HashMap<>();
        Map<Preferences.Key<?>, Object> preferencesMap =  preferences.asMap();
        for (Preferences.Key key : preferencesMap.keySet()) {
            results.put(key.toString(), Integer.valueOf(preferencesMap.get(key).toString()));
        }
        return results;
    }
}
