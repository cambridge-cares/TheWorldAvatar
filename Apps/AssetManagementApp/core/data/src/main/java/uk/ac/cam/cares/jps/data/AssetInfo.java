package uk.ac.cam.cares.jps.data;

import java.util.HashMap;

import uk.ac.cam.cares.jps.network.assetinfo.AssetInfoModel;

public class AssetInfo {
    HashMap<String, String> properties;

    public AssetInfo(AssetInfoModel assetInfoModel) {
        this.properties = assetInfoModel.getProperties();
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
        return this.properties.get(key);
    }
}
