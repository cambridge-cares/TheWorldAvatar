package uk.ac.cam.cares.jps.sensor;

import android.content.Context;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.toolbox.JsonObjectRequest;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import okhttp3.HttpUrl;

public class UserPhoneNetworkSource {
    Context context;
    RequestQueue requestQueue;
    Logger LOGGER = Logger.getLogger(UserPhoneNetworkSource.class);

    public UserPhoneNetworkSource(Context context, RequestQueue requestQueue) {
        this.context = context;
        this.requestQueue = requestQueue;
    }

    public void registerAppToUser(String userId, String deviceId, Response.Listener<Boolean> onSuccessListener, Response.ErrorListener errorListener) {

        try {
            JSONObject body = new JSONObject();
            body.put("userId", userId);
            body.put("phoneId", deviceId);

            String url = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
                    .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.useragent_registerphone))
                    .build().toString();

            JsonObjectRequest request = new JsonObjectRequest(Request.Method.POST, url, body, jsonObject -> {
                LOGGER.info(String.format("Phone id %s is register to user %s", deviceId, userId));
                onSuccessListener.onResponse(true);
            }, volleyError -> {
                // todo: how to handle registration failed case?
                LOGGER.error(volleyError.getMessage());
            });

            requestQueue.add(request);
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }

    }
}
