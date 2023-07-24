package uk.ac.cam.cares.jps.data;

import com.android.volley.Response;

import java.util.Map;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.network.AssetInfoModel;
import uk.ac.cam.cares.jps.network.AssetNetworkSource;

public class AssetInfoRepository {
    @Inject
    AssetNetworkSource networkSource;

    public Map<String, String> getAssetInfoByIri(String iri, Response.Listener<AssetInfo> onSuccessUpper, Response.ErrorListener onErrorUpper) {
        Response.Listener<AssetInfoModel> onSuccess = asset -> {
            // if have multiple data source, then should resolve conflict here
            AssetInfo assetInfo = new AssetInfo(asset);
            onSuccessUpper.onResponse(assetInfo);
        };

        networkSource.getAssetInfoByIri(iri, onSuccess, onErrorUpper);
        return null;
    }
}