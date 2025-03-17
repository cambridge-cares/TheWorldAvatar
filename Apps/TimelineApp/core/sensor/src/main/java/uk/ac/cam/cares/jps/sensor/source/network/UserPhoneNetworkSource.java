package uk.ac.cam.cares.jps.sensor.source.network;

import android.content.Context;

import com.android.volley.AuthFailureError;
import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.toolbox.JsonObjectRequest;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.Map;

import okhttp3.HttpUrl;

/**
 * A class that manages phone to user registration request
 */
public class UserPhoneNetworkSource {
    Context context;
    RequestQueue requestQueue;
    Logger LOGGER = Logger.getLogger(UserPhoneNetworkSource.class);

    public UserPhoneNetworkSource(Context context, RequestQueue requestQueue) {
        this.context = context;
        this.requestQueue = requestQueue;
    }

    /**
     * Register phone to user
     * @param userId current logged in user
     * @param deviceId current device id
     * @param onSuccessListener operations to perform when success
     * @param errorListener operations to perform when failure
     */
    public void registerAppToUser(String userId, String deviceId, Response.Listener<Boolean> onSuccessListener, Response.ErrorListener errorListener) {

        try {
            JSONObject body = new JSONObject();
            //body.put("userId", userId);
            body.put("phoneId", deviceId);

            String url = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
                    .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.useragent_registerphone))
                    .build().toString();

            JsonObjectRequest request = new JsonObjectRequest(Request.Method.POST, url, body, jsonObject -> {
                
                LOGGER.info(String.format("Phone id %s is register to user %s", deviceId, userId));
                onSuccessListener.onResponse(true);
            }, volleyError -> {
                LOGGER.error(volleyError.getMessage());
            }) {

                @Override
                public Map<String, String> getHeaders() throws AuthFailureError {
                    Map<String, String> headers = new HashMap<>();
                    headers.put("Authorization", "Bearer " + userId); // Ensure this is the correct user
                    return headers;
                }
            };

            requestQueue.add(request);
        } catch (JSONException e) {
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }

    }
}
