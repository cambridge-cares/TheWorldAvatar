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
import android.os.Handler;
import android.os.HandlerThread;
import android.os.IBinder;
import android.os.Process;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import androidx.core.app.ServiceCompat;
import androidx.core.content.ContextCompat;

import org.apache.log4j.Logger;
import org.json.JSONArray;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.zip.GZIPOutputStream;

import javax.inject.Inject;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorHandlerManager;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.sensor.source.network.NetworkChangeReceiver;
import uk.ac.cam.cares.jps.sensor.source.network.SensorNetworkSource;
import uk.ac.cam.cares.jps.sensor.source.state.SensorCollectionStateException;
import uk.ac.cam.cares.jps.sensor.source.state.SensorCollectionStateManager;

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
    @Inject SensorLocalSource sensorLocalSource;
    @Inject
    SensorCollectionStateManager sensorCollectionStateManager;



    private final int FOREGROUND_ID = 100;
    private final String CHANNEL_ID = "Sensors";
    private final int SENSOR_FRAGMENT_REQUEST_CODE = 100;
    private static final long SEND_INTERVAL = 5000;
    private final Logger LOGGER = Logger.getLogger(SensorService.class);
    private HandlerThread thread;
    private NetworkChangeReceiver networkChangeReceiver;
    private static final long THIRTY_DAYS_IN_MILLIS = 30L * 24 * 60 * 60 * 1000;
    private static final long BUFFER_FLUSH_INTERVAL = 60000; // Flush buffer every 60 seconds
    private static final long NETWORK_SEND_INTERVAL = 60000; // 300000 - five minutes for non-tests

    private Map<String, JSONArray> memoryBuffer = new HashMap<>();

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



        Handler handler = new Handler(thread.getLooper());
        Runnable bufferAndFlushData = new Runnable() {
            @Override
            public void run() {
                Map<String, JSONArray> localData = sensorHandlerManager.collectSensorData();

                // add data to memory buffer
                for (Map.Entry<String, JSONArray> entry : localData.entrySet()) {
                    String sensorName = entry.getKey();
                    JSONArray newData = entry.getValue();

                    if (!memoryBuffer.containsKey(sensorName)) {
                        memoryBuffer.put(sensorName, new JSONArray());
                    }

                    // Add the new data to the existing buffered data
                    JSONArray bufferedData = memoryBuffer.get(sensorName);
                    for (int i = 0; i < newData.length(); i++) {
                        try {
                            bufferedData.put(newData.get(i));
                        } catch (Exception e) {
                            LOGGER.error("Error adding data to buffer", e);
                        }
                    }
                }

                // Write buffered data to local storage
                sensorLocalSource.writeToDatabase(memoryBuffer);
                // Clear the buffer after flushing to local storage
                memoryBuffer.clear();

                // Periodically flush the buffer to local storage
                handler.postDelayed(this, BUFFER_FLUSH_INTERVAL);

                long currentTime = System.currentTimeMillis();
                long cutoffTime = currentTime - THIRTY_DAYS_IN_MILLIS;
                sensorLocalSource.deleteHistoricalData(cutoffTime);

            }
        };

        // Runnable for sending accumulated data from local storage to the network
        Runnable sendDataToNetwork = new Runnable() {
            private static final int PAGE_SIZE = 100; // # of records per page
            private int offset = 0; // Initial offset
            @Override
            public void run() {
                boolean hasMoreData = true;

                while (hasMoreData) {
                JSONArray allSensorData = sensorLocalSource.retrieveUnUploadedSensorData(selectedSensors, PAGE_SIZE, offset);
                LOGGER.info("Retrieved " + allSensorData.length() + " items from local storage.");

                if (allSensorData.length() < PAGE_SIZE) {
                    hasMoreData = false; // No more data to fetch
                }

                String jsonString = allSensorData.toString();


                    // Compress the JSON string
                try {
                    byte[] compressedData = compressData(jsonString);

                    LOGGER.info("Size of data after compression: " + compressedData.length + " bytes");

                    // Send the accumulated data to the network
                    if (allSensorData.length() > 0) {
                        LOGGER.info("Attempting to send " + allSensorData.length() + " items to the network.");
                        LOGGER.info("All sensor data " + allSensorData);
                        sensorNetworkSource.sendPostRequest(deviceId, compressedData, allSensorData);
                        LOGGER.info("Accumulated data sent to network.");
                    } else {
                        LOGGER.info("No accumulated data to send to the network.");
                    }
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
                    offset += PAGE_SIZE;
                }

                // Schedule the next network send
                handler.postDelayed(this, NETWORK_SEND_INTERVAL);
            }
        };


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




        //  buffer and flush process
        handler.post(bufferAndFlushData);
        handler.post(sendDataToNetwork);


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
        try {        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
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
            sensorHandlerManager.stopSensors();
            String taskId = sensorCollectionStateManager.getTaskId();
            LOGGER.info("Stopping sensor service with Task ID: " + taskId);


            sensorCollectionStateManager.setTaskId(null);
            ServiceCompat.stopForeground(this, ServiceCompat.STOP_FOREGROUND_REMOVE);
            thread.quit();

            LOGGER.info("Sensor service is stopped. Sensors stop recording.");
        } catch (NullPointerException exception) {
            LOGGER.warn("Foreground service has already stopped.");
        } catch (SensorCollectionStateException e) {
            throw new RuntimeException(e);
        }
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

}
