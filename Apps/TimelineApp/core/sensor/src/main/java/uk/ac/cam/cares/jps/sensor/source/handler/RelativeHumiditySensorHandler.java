package uk.ac.cam.cares.jps.sensor.source.handler;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Handles the collection of relative humidity data from the device's humidity sensor.
 * This class extends {@link AbstractSensorHandler} to implement sensor-specific handling
 * for relative humidity measurements.
 */
public class RelativeHumiditySensorHandler extends AbstractSensorHandler {

    /**
     * Initializes a new instance of the RelativeHumiditySensorHandler for accessing the device's humidity sensor.
     *
     * @param sensorManager The system's sensor service manager used to access the humidity sensor.
     */
    public RelativeHumiditySensorHandler(SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_RELATIVE_HUMIDITY);
        this.sensorName = "humidity";
    }

    /**
     * Processes sensor event updates by reading the relative humidity values from the sensor event.
     * Each sensor event data is encapsulated as a JSON object that includes the sensor name,
     * the timestamp of the reading, and the humidity value.
     *
     * @param event The sensor event that includes the latest sensor readings.
     */
    @Override
    public void onSensorChanged(SensorEvent event) {
        JSONObject dataPoint = new JSONObject();
        try {
            JSONObject values = new JSONObject();
            values.put("humidity", event.values[0]);

            dataPoint.put("name", this.sensorName);
            dataPoint.put("time", System.currentTimeMillis() * 1000000);
            dataPoint.put("values", values);

            synchronized (this) {
                sensorData.put(dataPoint);
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }
}




















//package com.example.notsensorlogger2;
//
//import android.hardware.Sensor;
//import android.hardware.SensorEvent;
//import android.hardware.SensorManager;
//import android.util.Log;
//
//import org.json.JSONException;
//import org.json.JSONObject;
//
//public class RelativeHumiditySensorHandler extends AbstractSensorHandler {
//    private float lastValue;
//    public RelativeHumiditySensorHandler(SensorManager sensorManager) {
//        super(sensorManager, Sensor.TYPE_RELATIVE_HUMIDITY);
//        this.sensorName = "Relative Humidity";
//    }
//
//    @Override
//    public void onSensorChanged(SensorEvent event) {
//        // Save the latest sensor value
//        lastValue = event.values[0];
//    }
//
//    @Override
//    public JSONObject getSensorData() {
//        // Create a new JSON object to store sensor data
//        JSONObject sensorData = new JSONObject();
//        try {
//            // Put the sensor name, which should match the expected key from SensorLoggerMobileAppAgent
//            sensorData.put("name", "humidity"); // Name used by SensorLoggerMobileAppAgent
//
//            // Create the values object
//            JSONObject values = new JSONObject();
//            values.put("humidity", lastValue);
//
//            // Put the values object into sensorData
//            sensorData.put("values", values);
//
//            // Put the timestamp in nanoseconds since epoch (adjust as necessary)
//            sensorData.put("time", System.currentTimeMillis() * 1000000);
//        } catch (JSONException e) {
//            e.printStackTrace();
//        }
//        return sensorData;
//    }
//
//    // Method to retrieve the latest sensor value
//    public float getLastSensorValue() {
//        return lastValue;
//    }
//}
