package uk.ac.cam.cares.jps.network.assetinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.HAS_TIME_SERIES;

import java.util.HashMap;

public class AssetInfoModel {

    private HashMap<String, String> properties;

    public AssetInfoModel(HashMap<String, String> properties) {
        this.properties = properties;
    }

    public HashMap<String, String> getProperties() {
        return properties;
    }

    public void setHasTimeSeries(boolean hasTimeSeries) {
        properties.put(HAS_TIME_SERIES, hasTimeSeries? "true" : "false");
    }
}
