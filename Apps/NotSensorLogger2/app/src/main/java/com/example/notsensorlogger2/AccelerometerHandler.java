package com.example.notsensorlogger2;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;
import android.util.Log;

import org.json.JSONException;
import org.json.JSONObject;

public class AccelerometerHandler extends AbstractSensorHandler {
    private float[] lastValues = new float[3]; // Array to hold the latest sensor values
    public AccelerometerHandler(SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_ACCELEROMETER);
        this.sensorName = "Accelerometer";
    }

    @Override
    public void onSensorChanged(SensorEvent event) {
        super.onSensorChanged(event);
        // Log.d("AccelerometerData", "X: " + event.values[0] + " Y: " + event.values[1] + " Z: " + event.values[2]);
        System.arraycopy(event.values, 0, this.lastValues, 0, this.lastValues.length);
    }

    // Method to retrieve the latest sensor values
    public float[] getLastSensorValues() {
        return lastValues;
    }

    @Override
    public JSONObject getSensorData() {
        // Create a new JSON object to store sensor data
        JSONObject sensorData = new JSONObject();
        try {
            // Put the sensor name, which should match the expected key from SensorLoggerMobileAppAgent
            sensorData.put("name", "accelerometer"); // Change this to match the name used by SensorLoggerMobileAppAgent

            // Create the values object
            JSONObject values = new JSONObject();
            // Assume that you have a method getLastSensorValues() that returns the latest sensor values as float[]
            float[] lastValues = this.getLastSensorValues(); // You'll need to implement this method
            values.put("x", lastValues[0]);
            values.put("y", lastValues[1]);
            values.put("z", lastValues[2]);

            // Put the values object into sensorData
            sensorData.put("values", values);

            // Put the timestamp. You need to ensure this timestamp matches the format used by SensorLoggerMobileAppAgent
            // The timestamp should be in nanoseconds since epoch
            sensorData.put("time", System.currentTimeMillis() * 1000000); // Example, adjust as necessary
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return sensorData;
    }

}
