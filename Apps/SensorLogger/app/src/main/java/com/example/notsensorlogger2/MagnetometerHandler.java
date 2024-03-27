package com.example.notsensorlogger2;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;
import android.util.Log;

import org.json.JSONException;
import org.json.JSONObject;

public class MagnetometerHandler extends AbstractSensorHandler {
    private float[] lastValues = new float[3];
    public MagnetometerHandler(SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_MAGNETIC_FIELD);
        this.sensorName = "Magnetometer";
    }

    @Override
    public void onSensorChanged(SensorEvent event) {
        super.onSensorChanged(event);
       // Log.d("MagnetometerData", "X: " + event.values[0] + " Y: " + event.values[1] + " Z: " + event.values[2]);
        System.arraycopy(event.values, 0, this.lastValues, 0, this.lastValues.length);
    }

    @Override
    public JSONObject getSensorData() {
        // Create a new JSON object to store sensor data
        JSONObject sensorData = new JSONObject();
        try {
            // Put the sensor name, which should match the expected key from SensorLoggerMobileAppAgent
            sensorData.put("name", "magnetometer"); // Name used by SensorLoggerMobileAppAgent

            // Create the values object
            JSONObject values = new JSONObject();
            values.put("x", lastValues[0]);
            values.put("y", lastValues[1]);
            values.put("z", lastValues[2]);

            // Put the values object into sensorData
            sensorData.put("values", values);

            // Put the timestamp in nanoseconds since epoch (adjust as necessary)
            sensorData.put("time", System.currentTimeMillis() * 1000000);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return sensorData;
    }

    // Method to retrieve the latest sensor values
    public float[] getLastSensorValues() {
        return lastValues;
    }

}