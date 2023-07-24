package uk.ac.cam.cares.jps.network;

import java.util.HashMap;

public class AssetInfoModel {

    private HashMap<String, String> properties;

    public AssetInfoModel(HashMap<String, String> properties) {
        this.properties = properties;
    }

    public HashMap<String, String> getProperties() {
        return properties;
    }
}
