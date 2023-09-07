package uk.ac.cam.cares.jps.data;

import java.util.HashMap;

import uk.ac.cam.cares.jps.network.assetinfo.AssetInfoModel;

public class AssetInfo {
    HashMap<String, String> properties;

    public AssetInfo(AssetInfoModel assetInfoModel) {
        this.properties = assetInfoModel.getProperties();
    }

    public HashMap<String, String> getProperties() {
        return properties;
    }
}
