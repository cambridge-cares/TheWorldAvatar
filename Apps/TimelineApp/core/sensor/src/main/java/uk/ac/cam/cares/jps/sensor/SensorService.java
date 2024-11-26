package uk.ac.cam.cares.jps.sensor;

import android.Manifest;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.content.pm.ServiceInfo;
import android.net.ConnectivityManager;
import android.net.Uri;
import android.os.Build;
import android.os.HandlerThread;
import android.os.IBinder;

import androidx.work.Data;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationCompat;
import androidx.core.app.ServiceCompat;
import androidx.core.content.ContextCompat;
import androidx.work.OneTimeWorkRequest;
import androidx.work.WorkManager;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;


import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.UUID;
import java.util.concurrent.TimeUnit;


import javax.inject.Inject;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.sensor.data.SensorCollectionStateManagerRepository;
import uk.ac.cam.cares.jps.sensor.source.activity.ActivityRecognitionReceiver;
import uk.ac.cam.cares.jps.sensor.source.worker.BufferFlushWorker;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.worker.SensorUploadWorker;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorHandlerManager;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.sensor.source.network.NetworkChangeReceiver;
import uk.ac.cam.cares.jps.sensor.source.network.SensorNetworkSource;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;


import com.google.android.gms.location.ActivityRecognition;
import com.google.android.gms.location.ActivityRecognitionClient;

/**
 * A foreground service that keeps sensor recording running even when the app is terminated by user or the system.
 * Please refer {@link  <a href="https://developer.android.com/develop/background-work/services">Android Service Overview</a>} for more information
 */
@AndroidEntryPoint
public class SensorService extends Service {

    @Inject
    SensorNetworkSource sensorNetworkSource;
    @Inject
    SensorHandlerManager sensorHandlerManager;
    @Inject
    SensorLocalSource sensorLocalSource;
    @Inject
    SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository;


    private final int FOREGROUND_ID = 100;
    private final String CHANNEL_ID = "Sensors";
    private final int SENSOR_FRAGMENT_REQUEST_CODE = 100;
    private final Logger LOGGER = Logger.getLogger(SensorService.class);
    private HandlerThread thread;
    private NetworkChangeReceiver networkChangeReceiver;
    private ActivityRecognitionClient activityRecognitionClient;
    Timer bufferWorkerTimer = new Timer();
    Timer uploadWorkerTimer = new Timer();
    private PendingIntent activityRecognitionPendingIntent;
    private Map<String, Integer> sensorSettingsMap;


    @Override
    public void onCreate() {
        super.onCreate();

        // Register the NetworkChangeReceiver
        if (networkChangeReceiver == null) {
            networkChangeReceiver = new NetworkChangeReceiver(sensorLocalSource, sensorNetworkSource);
            IntentFilter filter = new IntentFilter(ConnectivityManager.CONNECTIVITY_ACTION);
            registerReceiver(networkChangeReceiver, filter);
        }

        sensorSettingsMap = loadSensorSettingsConfig(this);

    }

    private Map<String, Integer> loadSensorSettingsConfig(Context context) {
        Map<String, Integer> settings = new HashMap<>();
        try {
            InputStream is = context.getAssets().open("sensor_settings_config.json");
            int size = is.available();
            byte[] buffer = new byte[size];
            is.read(buffer);
            is.close();
            String json = new String(buffer, StandardCharsets.UTF_8);
            JSONObject configJson = new JSONObject(json);
            settings.put("activity_request_code", configJson.optInt("activity_recognition_request_code", 0));
            settings.put("activity_recognition_update_interval", configJson.optInt("activity_recognition_update_interval"));
            settings.put("buffer_delay", configJson.optInt("buffer_delay"));
            settings.put("upload_delay", configJson.optInt("upload_delay"));

        } catch (Exception e) {
            e.printStackTrace();
        }
        return settings;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {

        if (intent == null || intent.getExtras() == null) {
            LOGGER.warn("Self stop because deviceId unknown");
            stopSelf();
            // todo: check whether this return is correct
            return START_STICKY;
        }

        // Only generate a new task ID if there is no existing recording session
        sensorCollectionStateManagerRepository.getTaskId(new RepositoryCallback<>() {
            @Override
            public void onSuccess(String taskId) {
                if (taskId == null || taskId.isEmpty()) {
                    String newId = UUID.randomUUID().toString();
                    sensorCollectionStateManagerRepository.setTaskId(newId);
                    LOGGER.info("Started new recording with Task ID: " + newId);
                } else {
                    LOGGER.info("Resuming recording with existing Task ID: " + taskId);
                }
            }

            @Override
            public void onFailure(Throwable error) {
                LOGGER.warn("Not able to retrieve task id.");
                stopSelf();
            }
        });

        String deviceId = intent.getExtras().getString("deviceId");

        // Get the list of selected sensors
        List<SensorType> selectedSensors = intent.getParcelableArrayListExtra("selectedSensors");

        if (selectedSensors == null || selectedSensors.isEmpty()) {
            LOGGER.warn("No sensors selected or sensor list is empty.");
            stopSelf();
            return START_STICKY;
        }

        if (selectedSensors.contains(SensorType.ACTIVITY) && !checkActivityRecognitionPermission()) {
            LOGGER.warn("Activity Recognition permission is required but not granted.");
            stopSelf();
            return START_NOT_STICKY;
        }

        sensorHandlerManager.startSelectedSensors(selectedSensors);


        if (selectedSensors.contains(SensorType.ACTIVITY)) {
            // registering the activity recognition client
            Intent activityIntent = new Intent(this, ActivityRecognitionReceiver.class);
            activityRecognitionClient = ActivityRecognition.getClient(this);
            activityRecognitionPendingIntent = PendingIntent.getBroadcast(
                    this,
                    sensorSettingsMap.get("activity_request_code"),
                    activityIntent,
                    PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_MUTABLE
            );

            // Request activity updates
            activityRecognitionClient.requestActivityUpdates(
                    sensorSettingsMap.get("activity_recognition_update_interval"),
                    activityRecognitionPendingIntent
            ).addOnSuccessListener(aVoid -> {
                LOGGER.info("Successfully requested activity updates");
            }).addOnFailureListener(e -> {
                LOGGER.error("Failed to request activity updates", e);
            });
        }

        // Convert the list of sensors to a JSONArray string
        JSONArray jsonArray = new JSONArray();
        for (SensorType sensor : selectedSensors) {
            jsonArray.put(sensor.name());
        }

        // Create Data object for passing parameters to the worker
        Data uploadData = new Data.Builder()
                .putString("deviceId", deviceId)
                .putString("selectedSensors", jsonArray.toString())
                .build();


        long BUFFER_DELAY = sensorSettingsMap.get("buffer_delay"); // delay in milliseconds

        bufferWorkerTimer.schedule(new TimerTask() {
            @Override
            public void run() {
                OneTimeWorkRequest bufferFlushWork = new OneTimeWorkRequest.Builder(BufferFlushWorker.class)
                        .addTag("bufferFlushWork")
                        .setInitialDelay(1, TimeUnit.MINUTES)
                        .build();
                WorkManager.getInstance(getApplicationContext()).enqueue(bufferFlushWork);

            }
        }, BUFFER_DELAY, BUFFER_DELAY);


        long upload_delay = sensorSettingsMap.get("upload_delay"); // delay in milliseconds

        uploadWorkerTimer.schedule(new TimerTask() {
            @Override
            public void run() {
                OneTimeWorkRequest dataUploadWork = new OneTimeWorkRequest.Builder(
                        SensorUploadWorker.class)
                        .addTag("dataUploadWork")
                        .setInitialDelay(1, TimeUnit.MINUTES)
                        .setInputData(uploadData)
                        .build();
                WorkManager.getInstance(getApplicationContext()).enqueue(dataUploadWork);

            }
        }, upload_delay, upload_delay);


        // Determine if location sensor is toggled
        boolean isLocationSensorToggled = selectedSensors.contains(SensorType.LOCATION);
        int type = 0;
        if (isLocationSensorToggled) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                LOGGER.info("location toggled true" + type);
                type = ServiceInfo.FOREGROUND_SERVICE_TYPE_LOCATION;
            }
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.UPSIDE_DOWN_CAKE) {
                type |= ServiceInfo.FOREGROUND_SERVICE_TYPE_SPECIAL_USE;
            }
        } else {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                type = ServiceInfo.FOREGROUND_SERVICE_TYPE_SPECIAL_USE;
            }
        }

        Notification notification = getNotification();

        if (isLocationSensorToggled && type != 0) {
            // make sure that the permissions are actually enabled before recording so app doesn't crash
            if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
                ServiceCompat.startForeground(this, FOREGROUND_ID, notification, type);
            } else {
                LOGGER.warn("Location permission not granted. Unable to start service with location type.");
                stopSelf();
            }
        } else {
            // For non-location sensors or if no specific type is required
            ServiceCompat.startForeground(this, FOREGROUND_ID, notification, type);
        }


        return START_STICKY;
    }


    /**
     * Create and configure notification to be shown when the service is running
     *
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
                PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_IMMUTABLE);

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


    /**
     * Checks if activity permission has been granted.
     *
     * @return a boolean indicating false if permission has not been granted and true if it has.
     */
    private boolean checkActivityRecognitionPermission() {
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACTIVITY_RECOGNITION)
                != PackageManager.PERMISSION_GRANTED) {
            LOGGER.warn("Activity Recognition permission not granted. Unable to start service.");
            return false; // Permission not granted
        }
        return true; // Permission granted
    }


    @Override
    public void onDestroy() {

        // cancel timers for workers
        if (bufferWorkerTimer != null) {
            bufferWorkerTimer.cancel();
        }
        if (uploadWorkerTimer != null) {
            uploadWorkerTimer.cancel();
        }

        // Unregister the NetworkChangeReceiver when the service is destroyed
        if (networkChangeReceiver != null) {
            unregisterReceiver(networkChangeReceiver);
            networkChangeReceiver = null;
        }

        // Stop activity recognition updates
        if (activityRecognitionClient != null && activityRecognitionPendingIntent != null) {
            if (ActivityCompat.checkSelfPermission(this, Manifest.permission.ACTIVITY_RECOGNITION) != PackageManager.PERMISSION_GRANTED) {
                LOGGER.warn("Activity Recognition permission not granted. Unable to remove activity updates.");
                return;
            }
            activityRecognitionClient.removeActivityUpdates(activityRecognitionPendingIntent);
        }


        LOGGER.info("Stopping sensor service");
        try {
            if (sensorHandlerManager != null) {
                sensorHandlerManager.stopSensors();
                LOGGER.info("Sensors have been stopped.");
            }

            sensorCollectionStateManagerRepository.getTaskId(new RepositoryCallback<>() {
                @Override
                public void onSuccess(String taskId) {
                    LOGGER.info("Stopping sensor service with Task ID: " + taskId);
                }

                @Override
                public void onFailure(Throwable error) {
                }
            });

            ServiceCompat.stopForeground(this, ServiceCompat.STOP_FOREGROUND_REMOVE);
            sensorCollectionStateManagerRepository.clearManager();

            LOGGER.info("Sensor service is stopped. Sensors stop recording.");
        } catch (NullPointerException exception) {
            LOGGER.warn("Foreground service has already stopped.");
        }
        super.onDestroy();
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

}
