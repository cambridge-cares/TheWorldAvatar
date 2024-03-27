package com.example.notsensorlogger2;

import android.app.Notification;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Intent;
import android.hardware.SensorManager;
import android.os.IBinder;
import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;

public class SensorService extends Service {
    private SensorManager sensorManager;
    private SensorHandler accelerometerHandler;
    private SensorHandler gravitySensorHandler;
    private SensorHandler gyroscopeHandler;
    private SensorHandler lightSensorHandler;
    private SensorHandler magnetometerHandler;
    private SensorHandler pressureSensorHandler;
    private SensorHandler relativeHumiditySensorHandler;
    private LocationHandler locationHandler;

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        sensorManager = (SensorManager) getSystemService(SENSOR_SERVICE);

        accelerometerHandler = new AccelerometerHandler(sensorManager);
        gravitySensorHandler = new GravitySensorHandler(sensorManager);
        gyroscopeHandler = new GyroscopeHandler(sensorManager);
        lightSensorHandler = new LightSensorHandler(sensorManager);
        magnetometerHandler = new MagnetometerHandler(sensorManager);
        pressureSensorHandler = new PressureSensorHandler(sensorManager);
        relativeHumiditySensorHandler = new RelativeHumiditySensorHandler(sensorManager);
        locationHandler = new LocationHandler(this);

        accelerometerHandler.start();
        gravitySensorHandler.start();
        gyroscopeHandler.start();
        lightSensorHandler.start();
        magnetometerHandler.start();
        pressureSensorHandler.start();
        relativeHumiditySensorHandler.start();
        locationHandler.start();

        startForeground(1, createNotification());

        return START_STICKY;
    }

    @Override
    public void onDestroy() {
        accelerometerHandler.stop();
        gravitySensorHandler.stop();
        gyroscopeHandler.stop();
        lightSensorHandler.stop();
        magnetometerHandler.stop();
        pressureSensorHandler.stop();
        relativeHumiditySensorHandler.stop();
        locationHandler.stop();
        super.onDestroy();
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    private Notification createNotification() {
        Intent notificationIntent = new Intent(this, MainActivity.class);
        PendingIntent pendingIntent = PendingIntent.getActivity(this, 0, notificationIntent, PendingIntent.FLAG_IMMUTABLE);

        return new NotificationCompat.Builder(this, "sensor_service_channel")
                .setContentTitle("Sensor Service")
                .setContentText("Recording sensor data")
                .setSmallIcon(android.R.drawable.ic_dialog_info) // Use a default system icon
                .setContentIntent(pendingIntent)
                .build();
    }

}
