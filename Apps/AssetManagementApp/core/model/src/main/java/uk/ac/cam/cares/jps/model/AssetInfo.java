package uk.ac.cam.cares.jps.model;

import java.io.Serializable;
import java.util.HashMap;

public class AssetInfo implements Serializable {
    HashMap<String, String> properties;

    public AssetInfo(HashMap<String, String> properties) {
        this.properties = properties;
    }

    public AssetInfo() {
        this.properties = new HashMap<>();
    }

    public HashMap<String, String> getProperties() {
        return properties;
    }
    public void addProperties(String key, String value) {
        this.properties.put(key, value);
    }
    public String getProperty(String key) {
        return this.properties.getOrDefault(key, "");
    }
}
