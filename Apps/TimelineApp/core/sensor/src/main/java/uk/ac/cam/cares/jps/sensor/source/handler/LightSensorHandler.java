package uk.ac.cam.cares.jps.sensor.source.handler;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Handles sensor events and data for the light sensor. This class extends {@link AbstractSensorHandler}
 * and manages the registration of sensor events, data collection,
 * and provides access to the collected data.
 *
 * The light sensor measures the ambient light level (illumination) in lux.
 */
public class LightSensorHandler extends AbstractSensorHandler {

    /**
     * Constructs a new LightSensorHandler.
     *
     * @param sensorManager The sensor manager used to access the light sensor.
     */
    public LightSensorHandler(SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_LIGHT);
        this.sensorName = "light";
    }

    /**
     * Handles changes in the light sensor values. This method captures the ambient light level in lux
     * and logs it along with the sensor name and timestamp.
     *
     * @param event The sensor event containing the new sensor readings.
     */
    @Override
    public void onSensorChanged(SensorEvent event) {
        JSONObject dataPoint = new JSONObject();
        try {
            JSONObject values = new JSONObject();
            values.put("lux", event.values[0]);

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

    @Override
    public SensorType getSensorType() {
        return SensorType.LIGHT;
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
//public class LightSensorHandler extends AbstractSensorHandler {
//    private float lastValue;
//    public LightSensorHandler(SensorManager sensorManager) {
//        super(sensorManager, Sensor.TYPE_LIGHT);
//        this.sensorName = "Light";
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
//            sensorData.put("name", "light"); // Name used by SensorLoggerMobileAppAgent
//
//            // Create the values object
//            JSONObject values = new JSONObject();
//            values.put("lux", lastValue);
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
//
