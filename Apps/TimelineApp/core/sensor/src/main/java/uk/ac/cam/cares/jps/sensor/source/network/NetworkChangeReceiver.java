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


import androidx.work.Data;
import androidx.work.OneTimeWorkRequest;
import androidx.work.WorkManager;

import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.database.model.entity.UnsentData;
import uk.ac.cam.cares.jps.sensor.source.worker.SensorUploadWorker;
import uk.ac.cam.cares.jps.sensor.source.worker.UnsentDataUploadWorker;

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
     *
     * @param context the application context
     * @param intent the intent that triggered the receiver
     */
    @Override
    public void onReceive(Context context, Intent intent) {
        if (this.isNetworkAvailable(context)) {
            Data unsentDataWorkerParam = new Data.Builder()
                    .putString("taskId", taskId)
                    .build();
            OneTimeWorkRequest unsentDataUploadRequest = new OneTimeWorkRequest.Builder(
                    UnsentDataUploadWorker.class)
                    .addTag("unsentDataUploadWorker")
                    .setInputData(unsentDataWorkerParam)
                    .build();
            WorkManager.getInstance(context).enqueue(unsentDataUploadRequest);
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
