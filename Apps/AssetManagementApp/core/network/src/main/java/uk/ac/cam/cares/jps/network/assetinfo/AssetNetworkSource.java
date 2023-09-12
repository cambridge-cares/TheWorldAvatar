package uk.ac.cam.cares.jps.network.assetinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.HAS_TIME_SERIES;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.lang.reflect.Type;
import java.util.HashMap;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.NetworkConfiguration;

public class AssetNetworkSource {

    private static final Logger LOGGER = Logger.getLogger(AssetNetworkSource.class);

    String path = "feature-info-agent/get";


    Connection connection;

    @Inject
    public AssetNetworkSource(Connection connection) {
        BasicConfigurator.configure();
        this.connection = connection;
    }

    public void getAssetInfoByIri(String iri, Response.Listener<AssetInfoModel> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = NetworkConfiguration.constructUrlBuilder(path)
                .addQueryParameter("iri", iri)
                .build().toString();
        LOGGER.info(requestUri);

        Response.Listener<String> onSuccess = response -> {
            Gson gson = new Gson();
            Type type = new TypeToken<HashMap<String, String>>() {}.getType();
            try {
                JSONObject rawResponse = new JSONObject(response);
                AssetInfoModel assets = new AssetInfoModel(gson.fromJson(rawResponse.getJSONArray("meta").get(0).toString(), type));
                assets.setHasTimeSeries(rawResponse.has("time"));
                onSuccessUpper.onResponse(assets);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

        StringRequest request = new StringRequest(Request.Method.GET, requestUri, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }
}
