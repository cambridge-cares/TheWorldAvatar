package uk.ac.cam.cares.jps.sensor;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.content.pm.ServiceInfo;
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

import javax.inject.Inject;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.sensor.database.SensorLocalSource;

@AndroidEntryPoint
public class SensorService extends Service {

    @Inject SensorNetworkSource sensorNetworkSource;
    @Inject SensorManager sensorManager;
    @Inject SensorLocalSource sensorLocalSource;

    private final int FOREGROUND_ID = 100;
    private final String CHANNEL_ID = "Sensors";
    private final int SENSOR_FRAGMENT_REQUEST_CODE = 100;
    private static final long SEND_INTERVAL = 20000;
    private final Logger LOGGER = Logger.getLogger(SensorService.class);
    private HandlerThread thread;

    @Override
    public void onCreate() {
        super.onCreate();

        thread = new HandlerThread("ServiceStartArguments",
                Process.THREAD_PRIORITY_BACKGROUND);
        thread.start();
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
        Handler handler = new Handler(thread.getLooper());
        Runnable sendData = new Runnable() {
            @Override
            public void run() {
                JSONArray allSensorData = sensorManager.collectSensorData();
                sensorNetworkSource.sendPostRequest(deviceId, allSensorData);
//                sensorLocalSource.writeToDatabase();
                handler.postDelayed(this, SEND_INTERVAL);
            }
        };
//        sensorNetworkSource.initForService(new Handler(thread.getLooper()), deviceId);

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

//        sensorNetworkSource.startDataCollection();
        sensorManager.startSensors();
        handler.post(sendData);

        return START_STICKY;
    }

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
        LOGGER.info("Stopping sensor service");
        try {
            sensorManager.stopSensors();
//            sensorNetworkSource.stopDataCollection();
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
