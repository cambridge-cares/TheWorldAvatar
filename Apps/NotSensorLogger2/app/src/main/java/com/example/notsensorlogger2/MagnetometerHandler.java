package com.example.notsensorlogger2;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;
import org.json.JSONArray;

public class MagnetometerHandler extends AbstractSensorHandler {
    public MagnetometerHandler(SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_MAGNETIC_FIELD);
        this.sensorName = "magnetometer";
    }

    @Override
    public void onSensorChanged(SensorEvent event) {
        super.onSensorChanged(event);
    }

    @Override
    public JSONArray getSensorData() {
        return sensorData;
    }
}
