package uk.ac.cam.cares.jps.sensor.source.network;



import static uk.ac.cam.cares.jps.utils.di.UtilsModule.compressData;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.Network;
import android.net.NetworkCapabilities;
import android.os.Handler;
import android.os.Looper;
import android.util.Log;


import org.apache.log4j.Logger;
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
    private final Logger LOGGER = Logger.getLogger(NetworkChangeReceiver.class);

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
                int limit = 100;
                int offset = 0;
                List<UnsentData> unsentDataList;
                do {
                    LOGGER.info("offset: " + offset);
                    unsentDataList = sensorLocalSource.retrieveUnsentData(limit, offset);
                    offset += unsentDataList.size();

                    // TODO: should send as batches
                    for (UnsentData unsentData : unsentDataList) {
                        Handler handler = new Handler(Looper.getMainLooper());
                        handler.postDelayed(() -> {
                            try {
                                byte[] compressedData = compressData(unsentData.data);
                                sensorNetworkSource.sendPostRequest(unsentData.deviceId, taskId, compressedData, unsentData.data);
                                sensorLocalSource.deleteUnsentData(unsentData);
                                LOGGER.info("unsent data operations completed");
                            } catch (Exception e) {
                                LOGGER.error("Error processing unsent data", e);
                            }
                        }, 1000); // 1-second delay between requests
                    }
                } while (!unsentDataList.isEmpty());
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

    public void setTaskId(String taskId) {
        this.taskId = taskId;
    }

}
