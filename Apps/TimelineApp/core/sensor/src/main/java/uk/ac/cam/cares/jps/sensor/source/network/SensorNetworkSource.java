package uk.ac.cam.cares.jps.sensor.source.network;

import android.content.Context;
import android.util.Base64;
import android.util.Log;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.util.Random;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import okhttp3.HttpUrl;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.UnsentData;
import static uk.ac.cam.cares.jps.utils.di.UtilsModule.computeHash;

/**
 * A class that commits data to the network database.
 */
public class SensorNetworkSource {
    Context context;
    RequestQueue requestQueue;
    Logger LOGGER = Logger.getLogger(SensorNetworkSource.class);
    SensorLocalSource sensorLocalSource;
    private Set<Long> locationTimestamps = ConcurrentHashMap.newKeySet();

    public SensorNetworkSource(Context applicationContext,
                               RequestQueue requestQueue) {
        context = applicationContext;
        this.requestQueue = requestQueue;
        this.sensorLocalSource = new SensorLocalSource(context);
    }

    /**
     * Sends a POST request to upload compressed sensor data to the server.
     * If the network request fails, the data is stored locally as unsent data for future retries.
     * The method also logs the size of the compressed data before uploading.
     *
     * @param deviceId The ID of the device from which the data is being uploaded.
     * @param compressedData A byte array containing the GZIP-compressed sensor data.
     * @param sensorData A {@link JSONArray} containing the sensor data in uncompressed form,
     *                   used for local storage if the network request fails.
     * @throws UnsupportedEncodingException If the encoding is not supported during conversion to UTF-8.
     */
    public void sendPostRequest(String deviceId, byte[] compressedData, JSONArray sensorData) throws UnsupportedEncodingException {
        String url = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
                .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.sensorloggeragent_update))
                .build().toString();
        String sessionId = UUID.randomUUID().toString();
        int messageId = generateMessageId();

        String compressedDataString = Base64.encodeToString(compressedData, Base64.NO_WRAP);
        LOGGER.info("Size of compressed data string: " + compressedDataString.getBytes("UTF-8").length + " bytes");

        // Create a JSON object to include both metadata and the compressed data
        JSONObject postData = new JSONObject();
        try {
            postData.put("deviceId", deviceId);
            postData.put("messageId", messageId);
            postData.put("sessionId", sessionId);
            postData.put("compressedData", compressedDataString); // Include compressed data as a string
            LOGGER.info("Sending sensor data with metadata and compressed data.");
        } catch (JSONException e) {
            e.printStackTrace();
        }

        // Convert the structured JSONObject to String
        final String requestBody = postData.toString();

        Log.e("network source", "data =" + sensorData);
        StringRequest postRequest = new StringRequest(Request.Method.POST, url,
                response ->  Log.d("Response", response),

                error -> {
                Log.d("Error.Response", error.toString());
                LOGGER.info("Failed to send data to the server");

                    String dataHash = computeHash(sensorData.toString());
                    // Check for duplicates before inserting
                    if (!sensorLocalSource.isDataInUnsentData(dataHash)) {
                        UnsentData unsentData = new UnsentData();
                        unsentData.deviceId = deviceId;
                        unsentData.data = sensorData.toString();
                        unsentData.timestamp = System.currentTimeMillis();
                        unsentData.dataHash = dataHash;

                        sensorLocalSource.insertUnsentData(unsentData);
                    } else {
                        LOGGER.info("Data already in UnsentData. Skipping insertion.");
                    }
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

}
