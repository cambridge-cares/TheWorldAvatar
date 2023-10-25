package uk.ac.cam.cares.jps.network.assetinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.HAS_TIME_SERIES;

import java.util.HashMap;
import java.util.Map;

public class AssetInfoModel {

    private Map<String, String> properties;

    public AssetInfoModel(Map<String, String> properties) {
        this.properties = properties;
    }

    public Map<String, String> getProperties() {
        return properties;
    }

    public void setHasTimeSeries(boolean hasTimeSeries) {
        properties.put(HAS_TIME_SERIES, hasTimeSeries? "true" : "false");
    }
}
