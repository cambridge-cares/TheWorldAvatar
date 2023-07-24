package uk.ac.cam.cares.jps.data;

import java.util.HashMap;

import uk.ac.cam.cares.jps.network.AssetInfoModel;

public class AssetInfo {
    HashMap<String, String> properties;

    public AssetInfo(AssetInfoModel assetInfoModel) {
        this.properties = assetInfoModel.getProperties();
    }
}
