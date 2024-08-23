package uk.ac.cam.cares.jps.sensor;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ServiceInfo;
import android.net.ConnectivityManager;
import android.net.Uri;
import android.os.Build;
import android.os.Handler;
import android.os.HandlerThread;
import android.os.IBinder;
import android.os.Process;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import androidx.core.app.ServiceCompat;

import org.apache.log4j.Logger;
import org.json.JSONArray;

import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.AndroidEntryPoint;
import kotlin.Pair;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorHandler;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorManager;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.sensor.source.network.NetworkChangeReceiver;
import uk.ac.cam.cares.jps.sensor.source.network.SensorNetworkSource;

/**
 * A foreground service that keeps sensor recording running even when the app is terminated by user or the system.
 * Please refer {@link  <a href="https://developer.android.com/develop/background-work/services">Android Service Overview</a>} for more information
 */
@AndroidEntryPoint
public class SensorService extends Service {

    @Inject
    SensorNetworkSource sensorNetworkSource;
    @Inject
    SensorManager sensorManager;
    @Inject SensorLocalSource sensorLocalSource;

    private final int FOREGROUND_ID = 100;
    private final String CHANNEL_ID = "Sensors";
    private final int SENSOR_FRAGMENT_REQUEST_CODE = 100;
    private static final long SEND_INTERVAL = 5000;
    private final Logger LOGGER = Logger.getLogger(SensorService.class);
    private HandlerThread thread;
    private NetworkChangeReceiver networkChangeReceiver;
    private static final long THIRTY_DAYS_IN_MILLIS = 30L * 24 * 60 * 60 * 1000;

    @Override
    public void onCreate() {
        super.onCreate();

        thread = new HandlerThread("ServiceStartArguments",
                Process.THREAD_PRIORITY_BACKGROUND);
        thread.start();

        // Register the NetworkChangeReceiver
        if (networkChangeReceiver == null) {
            networkChangeReceiver = new NetworkChangeReceiver(sensorLocalSource, sensorNetworkSource);
            IntentFilter filter = new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION);
            registerReceiver(networkChangeReceiver, filter);
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        if (intent == null || intent.getExtras() == null) {
            LOGGER.warn("Self stop because deviceId unknown");
            stopSelf();
            // todo: check whether this return is correct
            return START_STICKY;
        }

        String deviceId = intent.getExtras().getString("deviceId");

        // Get the list of selected sensors
        List<SensorType> selectedSensors = intent.getParcelableArrayListExtra("selectedSensors");

        if (selectedSensors == null || selectedSensors.isEmpty()) {
            LOGGER.warn("No sensors selected or sensor list is empty.");
            stopSelf();
            return START_STICKY;
        }

        if (selectedSensors != null && !selectedSensors.isEmpty()) {
            // Start only the selected sensors
           sensorManager.startSelectedSensors(selectedSensors);
        } else {
            LOGGER.warn("No sensors selected or sensor list is empty.");
            stopSelf();
            return START_STICKY;
        }


        Handler handler = new Handler(thread.getLooper());
        Runnable sendData = new Runnable() {
            @Override
            public void run() {
                Pair<JSONArray, Map<String, JSONArray>> pair = sensorManager.collectSensorData();
                JSONArray allSensorData = pair.getFirst();
                sensorNetworkSource.sendPostRequest(deviceId, allSensorData);

                Map<String, JSONArray> localData = pair.getSecond();
                sensorLocalSource.writeToDatabase(localData);

                // delay the handler so request is sent and write every few seconds
                handler.postDelayed(this, SEND_INTERVAL);

                long currentTime = System.currentTimeMillis();
                long cutoffTime = currentTime - THIRTY_DAYS_IN_MILLIS;

                sensorLocalSource.deleteHistoricalData(cutoffTime);

            }
        };

        Notification notification = getNotification();
        int type = 0;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            type = ServiceInfo.FOREGROUND_SERVICE_TYPE_LOCATION;
        }
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.UPSIDE_DOWN_CAKE) {
            type |= ServiceInfo.FOREGROUND_SERVICE_TYPE_SPECIAL_USE;
        }
        ServiceCompat.startForeground(
                this,
                FOREGROUND_ID,
                notification,
                type
        );

        // run the sendData task on a separate thread
        handler.post(sendData);

        return START_STICKY;
    }

    /**
     * Create and configure notification to be shown when the service is running
     * @return notification
     */
    @NonNull
    private Notification getNotification() {
        NotificationChannel channel = new NotificationChannel(CHANNEL_ID, "SensorDataCollectionChannel", NotificationManager.IMPORTANCE_DEFAULT);
        channel.setDescription("SensorDataCollection channel for foreground service notification");

        NotificationManager notificationManager = getSystemService(NotificationManager.class);
        notificationManager.createNotificationChannel(channel);

        Uri sensorFragmentLink = Uri.parse(getString(uk.ac.cam.cares.jps.utils.R.string.sensor_fragment_link));
        Intent sensorFragmentIntent = new Intent(Intent.ACTION_VIEW, sensorFragmentLink);
        sensorFragmentIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        sensorFragmentIntent.addCategory("uk.ac.cam.cares.jps.app");
        PendingIntent pendingIntent = PendingIntent.getActivity(getApplicationContext(),
                SENSOR_FRAGMENT_REQUEST_CODE,
                sensorFragmentIntent,
                PendingIntent.FLAG_UPDATE_CURRENT|PendingIntent.FLAG_IMMUTABLE);

        Notification notification =
                new NotificationCompat.Builder(getApplicationContext(), CHANNEL_ID)
                        .setContentText(getString(R.string.sensors_are_running_for_data_collection))
                        .setPriority(NotificationCompat.PRIORITY_DEFAULT)
                        .setCategory(NotificationCompat.CATEGORY_SERVICE)
                        .setSmallIcon(uk.ac.cam.cares.jps.ui.R.drawable.twa_notification_icon)
                        .setOngoing(true)
                        .setContentIntent(pendingIntent)
                        .build();
        return notification;
    }

    @Override
    public void onDestroy() {
        // Unregister the NetworkChangeReceiver when the service is destroyed
        if (networkChangeReceiver != null) {
            unregisterReceiver(networkChangeReceiver);
            networkChangeReceiver = null;
        }

        LOGGER.info("Stopping sensor service");
        try {
            sensorManager.stopSensors();
            ServiceCompat.stopForeground(this, ServiceCompat.STOP_FOREGROUND_REMOVE);
            thread.quit();

            LOGGER.info("Sensor service is stopped. Sensors stop recording.");
        } catch (NullPointerException exception) {
            LOGGER.warn("Foreground service has already stopped.");
        }
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }
}
