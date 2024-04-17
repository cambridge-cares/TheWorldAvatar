package uk.ac.cam.cares.jps.sensor;

import android.content.Context;
import android.os.Handler;
import android.util.Log;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.toolbox.JsonObjectRequest;
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
    private static final long SEND_INTERVAL = 6000;
    private SensorCollectionStateManager sensorCollectionStateManager;
    private SensorManager sensorManager;

    private Runnable sendDataRunnable;
    private Handler handler = new Handler();

    public SensorNetworkSource(Context applicationContext,
                               RequestQueue requestQueue,
                               SensorManager sensorManager,
                               SensorCollectionStateManager sensorCollectionStateManager) {
        context = applicationContext;
        this.requestQueue = requestQueue;
        this.sensorManager = sensorManager;

        this.sensorCollectionStateManager = sensorCollectionStateManager;
        sendDataRunnable = new Runnable() {
            @Override
            public void run() {
                JSONArray allSensorData = sensorManager.collectSensorData();
                sendPostRequest(allSensorData);
                handler.postDelayed(this, SEND_INTERVAL);
            }
        };
    }

    public void registerAppToUser(String userId, Response.ErrorListener errorListener) {

        try {
            String deviceId = sensorCollectionStateManager.getSensorCollectionState(userId).getDeviceId();

            JSONObject body = new JSONObject();
            body.put("userId", userId);
            body.put("phoneId", deviceId);

            String url = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
                    .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.useragent_registerphone))
                    .build().toString();

            JsonObjectRequest request = new JsonObjectRequest(Request.Method.POST, url, body, jsonObject -> {
                LOGGER.info(String.format("Phone id %s is register to user %s", deviceId, userId));
            }, volleyError -> {
                // todo: how to handle registration failed case?
                LOGGER.error(volleyError.getMessage());
            });

            requestQueue.add(request);
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }

    }

    public void startDataCollection() {
        sensorManager.startSensors();
        handler.post(sendDataRunnable);
    }

    public void stopDataCollection() {
        sensorManager.stopSensors();
        handler.removeCallbacks(sendDataRunnable);

        sensorCollectionStateManager.setRecordingState(false);
    }

    public void clearManagers() {
        // todo: need to fix the recording state
        sensorCollectionStateManager.clearState();
        stopDataCollection();

        LOGGER.info("finish clearing sensor related managers");
    }

    private void sendPostRequest(JSONArray sensorData) {
        String url = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
                .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.sensorloggeragent_update))
                .build().toString();
        String sessionId = UUID.randomUUID().toString();
        int messageId = generateMessageId();

        String deviceId = sensorCollectionStateManager.getSensorCollectionState().getDeviceId();

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
