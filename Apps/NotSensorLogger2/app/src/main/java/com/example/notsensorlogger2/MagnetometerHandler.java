package com.example.notsensorlogger2;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;
import android.util.Log;

public class MagnetometerHandler extends AbstractSensorHandler {
    public MagnetometerHandler(SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_MAGNETIC_FIELD);
        this.sensorName = "magnetometer";
    }
    public void start() {
    }

    @Override
    public void onSensorChanged(SensorEvent event) {
        super.onSensorChanged(event);
       // Log.d("MagnetometerData", "X: " + event.values[0] + " Y: " + event.values[1] + " Z: " + event.values[2]);
    }

}