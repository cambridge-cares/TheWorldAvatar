package uk.ac.cam.cares.jps.sensor.source.network;

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
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.UUID;

import okhttp3.HttpUrl;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.UnsentData;

/**
 * A class that commits data to the network database.
 */
public class SensorNetworkSource {
    Context context;
    RequestQueue requestQueue;
    Logger LOGGER = Logger.getLogger(SensorNetworkSource.class);
    SensorLocalSource sensorLocalSource;

    public SensorNetworkSource(Context applicationContext,
                               RequestQueue requestQueue) {
        context = applicationContext;
        this.requestQueue = requestQueue;
        this.sensorLocalSource = new SensorLocalSource(context);
    }

    /**
     * Sends a POST request to upload sensor data to the server.
     * If the network request fails, the data is stored locally as unsent data to be retried later.
     *
     * @param deviceId the ID of the device from which the data is being uploaded.
     * @param sensorData a {@link JSONArray} containing the sensor data to be uploaded.
     */
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
        }

        // Convert the structured JSONObject to String
        final String requestBody = postData.toString();

        Log.e("network source", "data =" + sensorData);
        StringRequest postRequest = new StringRequest(Request.Method.POST, url,
                response -> Log.d("Response", response),
                error -> {
                Log.d("Error.Response", error.toString());
                LOGGER.info("Failed to send data to the server");
                    // convert it to the correct format for the writeToDatabase method
                Map<String, JSONArray> sensorDataMap = convertSensorDataToMap(sensorData);

                    UnsentData unsentData = new UnsentData();
                    String serializedData = serializeMap(sensorDataMap);
                    unsentData.deviceId = deviceId;
                    unsentData.data = serializedData;
                    Log.e("network source", "data =" + serializedData);
                    unsentData.timestamp = System.currentTimeMillis();
                    sensorLocalSource.insertUnsentData(unsentData);
                }
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

    /**
     * Converts a JSONArray into a map so that it can be parsed into the local database.
     * @param sensorData array of all sensor data
     * @return a map of all sensor data
     */
    public Map<String, JSONArray> convertSensorDataToMap(JSONArray sensorData) {
        Map<String, JSONArray> sensorDataMap = new HashMap<>();

        try {
            for (int i = 0; i < sensorData.length(); i++) {
                JSONObject jsonObject = sensorData.getJSONObject(i);
                String sensorType = jsonObject.getString("name");
                JSONArray typeArray = sensorDataMap.getOrDefault(sensorType, new JSONArray());
                assert typeArray != null;
                typeArray.put(jsonObject);
                sensorDataMap.put(sensorType, typeArray);
            }
        } catch (JSONException e) {
            LOGGER.error("Error converting sensor data to map: " + e.getMessage());
        }

        return sensorDataMap;
    }

    /**
     * Creates a serialized string of UnsentData objects to be pushed into the local database.
     * @param map of sensor data
     * @return a serialized string of UnsentData
     */
    public String serializeMap(Map<String, JSONArray> map) {
        JSONObject jsonObject = new JSONObject();
        try {
            for (Map.Entry<String, JSONArray> entry : map.entrySet()) {
                jsonObject.put(entry.getKey(), entry.getValue());
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return jsonObject.toString();
    }
}
