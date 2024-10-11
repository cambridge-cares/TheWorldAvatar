package uk.ac.cam.cares.jps.sensor;

import android.Manifest;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.content.pm.ServiceInfo;
import android.net.ConnectivityManager;
import android.net.Uri;
import android.os.Build;
import android.os.HandlerThread;
import android.os.IBinder;
import android.os.Process;
import androidx.work.Data;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationCompat;
import androidx.core.app.ServiceCompat;
import androidx.core.content.ContextCompat;
import androidx.work.OneTimeWorkRequest;
import androidx.work.PeriodicWorkRequest;
import androidx.work.WorkManager;

import org.apache.log4j.Logger;
import org.json.JSONArray;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.zip.GZIPOutputStream;

import javax.inject.Inject;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.sensor.source.worker.BufferFlushWorker;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.worker.SensorUploadWorker;
import uk.ac.cam.cares.jps.sensor.source.database.model.activity.ActivityRecognitionService;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorHandlerManager;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.sensor.source.network.NetworkChangeReceiver;
import uk.ac.cam.cares.jps.sensor.source.network.SensorNetworkSource;
import uk.ac.cam.cares.jps.sensor.source.state.SensorCollectionStateException;
import uk.ac.cam.cares.jps.sensor.source.state.SensorCollectionStateManager;


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
    SensorCollectionStateManager sensorCollectionStateManager;



    private final int FOREGROUND_ID = 100;
    private final String CHANNEL_ID = "Sensors";
    private final int SENSOR_FRAGMENT_REQUEST_CODE = 100;
    private static final long SEND_INTERVAL = 5000;
    private final Logger LOGGER = Logger.getLogger(SensorService.class);
    private HandlerThread thread;
    private NetworkChangeReceiver networkChangeReceiver;
    private ActivityRecognitionClient activityRecognitionClient;
    Timer timerB = new Timer();
    Timer timer = new Timer();

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

        if (!checkActivityRecognitionPermission()) {
            stopSelf();
            return START_NOT_STICKY;
        }

        String userId = intent.getExtras().getString("userId");

        // Reinitialize the sensorCollectionState if it is null
        if (sensorCollectionStateManager.getSensorCollectionState() == null) {
            if (userId != null) {
                sensorCollectionStateManager.initSensorCollectionState(userId);
            } else {
                LOGGER.warn("UserId is null. Stopping service.");
                stopSelf();
                return START_NOT_STICKY;
            }
        }


        String deviceId = intent.getExtras().getString("deviceId");

        // Get the list of selected sensors
        List<SensorType> selectedSensors = intent.getParcelableArrayListExtra("selectedSensors");

        if (selectedSensors == null || selectedSensors.isEmpty()) {
            LOGGER.warn("No sensors selected or sensor list is empty.");
            stopSelf();
            return START_STICKY;
        }

        // Only generate a new task ID if there is no existing recording session
        String taskId;
        try {
            taskId = sensorCollectionStateManager.getTaskId();
            if (taskId == null || taskId.isEmpty()) {
                taskId = UUID.randomUUID().toString();
                sensorCollectionStateManager.setTaskId(taskId);  // Store task ID using SensorCollectionStateManager
                LOGGER.info("Started new recording with Task ID: " + taskId);
            } else {
                LOGGER.info("Resuming recording with existing Task ID: " + taskId);
            }
        } catch (SensorCollectionStateException e) {
            LOGGER.warn("Failed to retrieve Task ID, generating a new one.");
            taskId = UUID.randomUUID().toString();
            sensorCollectionStateManager.setTaskId(taskId);  // Store task ID using SensorCollectionStateManager
        }
        sensorHandlerManager.startSelectedSensors(selectedSensors);


        // registering the activity recognition client
        activityRecognitionClient = ActivityRecognition.getClient(this);
        PendingIntent pendingIntent = PendingIntent.getService(
                this, 0, new Intent(this, ActivityRecognitionService.class),
                PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_IMMUTABLE);

        activityRecognitionClient.requestActivityUpdates(3000, pendingIntent);


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



        long delayB = 60000; // delay in milliseconds

        timerB.schedule(new TimerTask() {
            @Override
            public void run() {
                OneTimeWorkRequest bufferFlushWork = new OneTimeWorkRequest.Builder(BufferFlushWorker.class)
                        .addTag("bufferFlushWork")
                        .setInitialDelay(1, TimeUnit.MINUTES)
                        .build();
                WorkManager.getInstance(getApplicationContext()).enqueue(bufferFlushWork);

            }
        }, delayB, delayB);


        long delay = 60000; // delay in milliseconds

        timer.schedule(new TimerTask() {
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
        }, delay, delay);



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
            if (ContextCompat.checkSelfPermission(this, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED &&
                    ContextCompat.checkSelfPermission(this, Manifest.permission.FOREGROUND_SERVICE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
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
     * Compresses a given string into a GZIP-compressed byte array.
     * This method uses the GZIP compression algorithm to reduce the size of the input string.
     *
     * @param data The input string to be compressed.
     * @return A byte array containing the GZIP-compressed data.
     * @throws IOException If an I/O error occurs during the compression process.
     */
    public static byte[] compressData(String data) throws IOException {
        try {
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            GZIPOutputStream gzipOutputStream = new GZIPOutputStream(byteArrayOutputStream);
            gzipOutputStream.write(data.getBytes("UTF-8"));
            gzipOutputStream.close();
            return byteArrayOutputStream.toByteArray();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
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
        if (timerB != null) {
            timerB.cancel();
        }
        if (timer != null) {
            timer.cancel();
        }

        // Unregister the NetworkChangeReceiver when the service is destroyed
        if (networkChangeReceiver != null) {
            unregisterReceiver(networkChangeReceiver);
            networkChangeReceiver = null;
        }

        // Stop activity recognition updates
        if (activityRecognitionClient != null) {
            PendingIntent pendingIntent = PendingIntent.getService(this, 0,
                    new Intent(this, ActivityRecognitionService.class), PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_IMMUTABLE);
            if (ActivityCompat.checkSelfPermission(this, Manifest.permission.ACTIVITY_RECOGNITION) != PackageManager.PERMISSION_GRANTED) {
                // TODO: Consider calling
                //    ActivityCompat#requestPermissions
                // here to request the missing permissions, and then overriding
                //   public void onRequestPermissionsResult(int requestCode, String[] permissions,
                //                                          int[] grantResults)
                // to handle the case where the user grants the permission. See the documentation
                // for ActivityCompat#requestPermissions for more details.
                return;
            }
            activityRecognitionClient.removeActivityUpdates(pendingIntent);
        }


        LOGGER.info("Stopping sensor service 1");
        try {
            if (sensorHandlerManager != null) {
                sensorHandlerManager.stopSensors();
                LOGGER.info("Sensors have been stopped.");
            }
            String taskId = sensorCollectionStateManager.getTaskId();
            LOGGER.info("Stopping sensor service with Task ID: " + taskId);

            ServiceCompat.stopForeground(this, ServiceCompat.STOP_FOREGROUND_REMOVE);

            if (sensorCollectionStateManager.getSensorCollectionState() != null) {
                LOGGER.info("Clearing sensor collection state.");
                sensorCollectionStateManager.clearState(sensorCollectionStateManager.getUserId());
            } else {
                LOGGER.warn("SensorCollectionState is already null. No need to clear.");
            }

            if (thread != null) {
                thread.quit();
            }

            sensorLocalSource.shutdownExecutor();

            LOGGER.info("Sensor service is stopped. Sensors stop recording.");
        } catch (NullPointerException exception) {
            LOGGER.warn("Foreground service has already stopped.");
        } catch (SensorCollectionStateException e) {
            throw new RuntimeException(e);
        } finally {
            super.onDestroy();
        }
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

}
