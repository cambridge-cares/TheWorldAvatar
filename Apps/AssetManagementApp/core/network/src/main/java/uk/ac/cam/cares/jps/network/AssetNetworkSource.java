package uk.ac.cam.cares.jps.network;

import android.content.Context;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.util.HashMap;

import javax.inject.Inject;

public class AssetNetworkSource {
    String path = "feature-info-agent/get";

    @Inject
    Connection connection;

    public void getAssetInfoByIri(String iri, Response.Listener<AssetInfoModel> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = NetworkConfiguration.constructUrlBuilder(path)
                .addQueryParameter("iri", iri)
                .build().toString();

        Response.Listener<String> onSuccess = response -> {
            Gson gson = new Gson();
            Type type = new TypeToken<HashMap<String, String>>() {}.getType();
            AssetInfoModel assets = new AssetInfoModel(gson.fromJson(response, type));
            onSuccessUpper.onResponse(assets);
        };

        StringRequest request = new StringRequest(Request.Method.GET, requestUri, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }
}
