package uk.ac.cam.cares.jps.sensor.network;

import android.content.Context;
import android.util.Log;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.nio.charset.StandardCharsets;
import java.util.Random;
import java.util.UUID;

import okhttp3.HttpUrl;

public class SensorNetworkSource {
    Context context;
    RequestQueue requestQueue;
    Logger LOGGER = Logger.getLogger(SensorNetworkSource.class);

    public SensorNetworkSource(Context applicationContext,
                               RequestQueue requestQueue) {
        context = applicationContext;
        this.requestQueue = requestQueue;
    }

    public void sendPostRequest(String deviceId, JSONArray sensorData) {
        String url = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
                .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.sensorloggeragent_update))
                .build().toString();
        String sessionId = UUID.randomUUID().toString();
        int messageId = generateMessageId();

        // Create a JSONObject to structure the data according to the required format
        JSONObject postData = new JSONObject();
        try {
            postData.put("deviceId", deviceId);
            postData.put("messageId", messageId);
            postData.put("payload", sensorData);
            postData.put("sessionId", sessionId);
            LOGGER.info("Send sensor data to agent: " + postData);
        } catch (JSONException e) {
            e.printStackTrace();
            // Handle the error appropriately
        }

        // Convert the structured JSONObject to String
        final String requestBody = postData.toString();

        StringRequest postRequest = new StringRequest(Request.Method.POST, url,
                response -> Log.d("Response", response),
                error -> Log.d("Error.Response", error.toString())
        ) {
            @Override
            public byte[] getBody() {
                return requestBody.getBytes(StandardCharsets.UTF_8);
            }

            @Override
            public String getBodyContentType() {
                return "application/json; charset=utf-8";
            }
        };

        requestQueue.add(postRequest);
    }

    private int generateMessageId() {
        return new Random().nextInt(1000);
    }
}
