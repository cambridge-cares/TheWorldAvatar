package uk.ac.cam.cares.jps.sensor.source.network;



import static uk.ac.cam.cares.jps.utils.Utils.compressData;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.Network;
import android.net.NetworkCapabilities;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;


import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.UnsentData;

/**
 * Class in charge of listening to changes in network connectivity. When the receiver detects the
 * app is back online onReceive is triggered and the unsent data is committed to the local and
 * network storages. Unsent data is then deleted after all operations are complete.
 */
public class NetworkChangeReceiver extends BroadcastReceiver {

    private final SensorLocalSource sensorLocalSource;
    private final SensorNetworkSource sensorNetworkSource;
    private final ExecutorService executorService = Executors.newSingleThreadExecutor();
    private String taskId;

    public NetworkChangeReceiver(SensorLocalSource sensorLocalSource, SensorNetworkSource sensorNetworkSource) {
        this.sensorLocalSource = sensorLocalSource;
        this.sensorNetworkSource = sensorNetworkSource;
    }

    /**
     * Called when the network status changes. If the network becomes available,
     * this method retrieves all unsent sensor data and attempts to send it to the server.
     * A small delay is introduced between each request to avoid overwhelming the network.
     *
     * @param context the application context
     * @param intent the intent that triggered the receiver
     */
    @Override
    public void onReceive(Context context, Intent intent) {
        if (this.isNetworkAvailable(context)) {
            executorService.execute(() -> {
                List<UnsentData> unsentDataList = sensorLocalSource.retrieveUnsentData();
                for (UnsentData unsentData : unsentDataList) {
                    Handler handler = new Handler(Looper.getMainLooper());
                    handler.postDelayed(() -> {
                        try {
                            Map<String, JSONArray> allSensorDataMap = deserializeMap(unsentData.data);
                            JSONArray allSensorData = convertMapToSensorData(allSensorDataMap);
                            String jsonString = allSensorData.toString();
                            byte[] compressedData = compressData(jsonString);
                            sensorNetworkSource.sendPostRequest(unsentData.deviceId, taskId, compressedData, allSensorData);
                            sensorLocalSource.deleteUnsentData(unsentData);
                            Log.e("network change receiver", "unsent data operations completed");
                        } catch (Exception e) {
                            Log.e("NetworkReceiver", "Error processing unsent data", e);
                        }
                    }, 1000); // 1-second delay between requests
                }
            });
        }
    }

    /**
     * Checks if a network connection is currently available.
     * It verifies the presence of cellular, WiFi, or Ethernet connectivity.
     *
     * @param context the application context
     * @return true if a network is available, false otherwise
     */
    private boolean isNetworkAvailable(Context context) {
        ConnectivityManager connectivityManager = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);

        if (connectivityManager != null) {
            Network network = connectivityManager.getActiveNetwork();
            if (network != null) {
                NetworkCapabilities networkCapabilities = connectivityManager.getNetworkCapabilities(network);
                return networkCapabilities != null && (networkCapabilities.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR)
                        || networkCapabilities.hasTransport(NetworkCapabilities.TRANSPORT_WIFI)
                        || networkCapabilities.hasTransport(NetworkCapabilities.TRANSPORT_ETHERNET));
            }
        }

        return false;
    }

    /**
     * Deserializes a JSON string into a Map where the keys represent sensor types
     * and the values are JSON arrays containing the sensor data.
     *
     * @param jsonString string of unsent data
     * @return map of unsent data
     */
    private Map<String, JSONArray> deserializeMap(String jsonString) {
        Map<String, JSONArray> map = new HashMap<>();
        try {
            JSONArray jsonArray = new JSONArray(jsonString);
            for (int i = 0; i < jsonArray.length(); i++) {
                JSONObject jsonObject = jsonArray.getJSONObject(i);
                String sensorType = jsonObject.getString("name");

                if (!map.containsKey(sensorType)) {
                    map.put(sensorType, new JSONArray());
                }
                map.get(sensorType).put(jsonObject);
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return map;
    }



    /**
     * Combines sensor data from a Map into a single JSONArray.
     * Each entry in the map corresponds to a sensor type and its data.
     *
     * @param sensorDataMap map of unsent data
     * @return array of unsent data
     */
    public JSONArray convertMapToSensorData(Map<String, JSONArray> sensorDataMap) {
        JSONArray combinedSensorData = new JSONArray();

        for (Map.Entry<String, JSONArray> entry : sensorDataMap.entrySet()) {
            JSONArray sensorArray = entry.getValue();
            for (int i = 0; i < sensorArray.length(); i++) {
                try {
                    combinedSensorData.put(sensorArray.getJSONObject(i));
                } catch (JSONException e) {

                }
            }
        }

        return combinedSensorData;
    }

    public void setTaskId(String taskId) {
        this.taskId = taskId;
    }

}
