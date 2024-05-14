package uk.ac.cam.cares.jps.sensor;

import android.app.ActivityManager;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.HandlerThread;
import android.os.IBinder;
import android.os.Looper;
import android.os.Process;

import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import androidx.core.app.ServiceCompat;
import androidx.navigation.NavDeepLinkBuilder;

import org.apache.log4j.Logger;

import javax.inject.Inject;

import dagger.hilt.android.AndroidEntryPoint;

@AndroidEntryPoint
public class SensorService extends Service {

    @Inject SensorNetworkSource sensorNetworkSource;

    private final int FOREGROUND_ID = 100;
    private final String CHANNEL_ID = "Sensors";
    private final int SENSOR_FRAGMENT_REQUEST_CODE = 100;
    private final Logger LOGGER = Logger.getLogger(SensorService.class);
    private HandlerThread thread;
    private String userId;

    @Override
    public void onCreate() {
        super.onCreate();

        thread = new HandlerThread("ServiceStartArguments",
                Process.THREAD_PRIORITY_BACKGROUND);
        thread.start();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        sensorNetworkSource.setHandler(new Handler(thread.getLooper()));

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
                        .setContentTitle("Foreground Service")
                        .setContentText("This is a foreground service notification")
                        .setPriority(NotificationCompat.PRIORITY_DEFAULT)
                        .setCategory(NotificationCompat.CATEGORY_SERVICE)
                        .setSmallIcon(uk.ac.cam.cares.jps.ui.R.drawable.twa_notification_icon)
                        .setOngoing(true)
                        .setContentIntent(pendingIntent)
                        .build();
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

        Bundle bundle = intent.getExtras();
        if (bundle != null) {
            userId = bundle.getString("userId");
            sensorNetworkSource.startDataCollection(userId);
        } else {
            // todo: do better error handle to make it more robust
            throw new RuntimeException("Failed to start data collection because no user id passed for sensor collection state manager.");
        }

        return START_STICKY;
    }

    @Override
    public void onDestroy() {
        LOGGER.info("Stopping sensor service");
        try {
            sensorNetworkSource.stopDataCollection(userId);
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
