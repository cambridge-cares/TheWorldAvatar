package uk.ac.cam.cares.jps.network.assetinfo;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.JsonObjectRequest;
import com.android.volley.toolbox.JsonRequest;
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

    String retrievePath = "feature-info-agent/get";
    String addAssetPath = "asset-manager-agent/instantiate";

    Connection connection;

    @Inject
    public AssetNetworkSource(Connection connection) {
        BasicConfigurator.configure();
        this.connection = connection;
    }

    public void getAssetInfoByIri(String iri, Response.Listener<AssetInfoModel> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = NetworkConfiguration.constructUrlBuilder(retrievePath)
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

    public void addAsset(JSONObject param, Response.Listener<JSONObject> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = NetworkConfiguration.constructUrlBuilder(addAssetPath).build().toString();

        Response.Listener<JSONObject> onSuccess = response -> {
            try {
                if (response.getJSONArray("Result").getJSONObject(0).has("deviceIRI")) {
                    onSuccessUpper.onResponse(response.getJSONArray("Result").getJSONObject(0));
                }
            } catch (JSONException e) {
                onFailureUpper.onErrorResponse(new VolleyError("Failed to create new asset"));
            }
        };

        JsonObjectRequest request = new JsonObjectRequest(Request.Method.POST, requestUri, param, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }
}
