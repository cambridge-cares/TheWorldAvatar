package com.example.notsensorlogger2;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.util.Log;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractSensorHandler implements SensorHandler, SensorEventListener {
    protected SensorManager sensorManager;
    protected Sensor sensor;
    protected JSONArray sensorData;
    protected long startTime;
    protected String sensorName;

    public AbstractSensorHandler(SensorManager sensorManager, int sensorType) {
        this.sensorManager = sensorManager;
        this.sensor = sensorManager.getDefaultSensor(sensorType);
        this.sensorData = new JSONArray();
        if (this.sensor == null) {
            Log.e("SensorError", "Sensor not available on this device.");
        }
    }

    @Override
    public void start() {
        sensorManager.registerListener(this, sensor, SensorManager.SENSOR_DELAY_NORMAL);
        startTime = System.currentTimeMillis();
        sensorName = sensor.getName();
    }

    @Override
    public void stop() {
        sensorManager.unregisterListener(this);
        long endTime = System.currentTimeMillis();
        long duration = endTime - startTime;
        Log.d("SensorRecording", "Sensor: " + sensorName + ", Duration: " + duration + "ms");
    }

    @Override
    public JSONObject getSensorData() {
        JSONObject sensorDataObject = new JSONObject();
        try {
            sensorDataObject.put("name", sensorName);
            sensorDataObject.put("time", System.currentTimeMillis() * 1000000); // Convert to nanoseconds
            sensorDataObject.put("values", sensorData); // Assuming sensorData is a JSONArray of sensor values
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return sensorDataObject;
    }


    @Override
    public void onSensorChanged(SensorEvent event) {
        JSONObject dataPoint = new JSONObject();
        try {
            dataPoint.put("sensor", sensorName);
            dataPoint.put("time", System.nanoTime());
            dataPoint.put("seconds_elapsed", (System.currentTimeMillis() - startTime) / 1000.0);
            dataPoint.put("x", event.values[0]);
            dataPoint.put("y", event.values[1]);
            dataPoint.put("z", event.values[2]);
            sensorData.put(dataPoint);
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {
        // Log.d("SensorAccuracy", "Accuracy of sensor " + sensor.getName() + " has changed to " + accuracy);
        // only implement if necessary
    }
}
