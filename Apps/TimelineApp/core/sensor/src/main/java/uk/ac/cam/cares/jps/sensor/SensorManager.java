package uk.ac.cam.cares.jps.sensor;

import static android.content.Context.SENSOR_SERVICE;

import android.content.Context;

import org.json.JSONArray;
import org.json.JSONException;

import uk.ac.cam.cares.jps.sensor.handler.AccelerometerHandler;
import uk.ac.cam.cares.jps.sensor.handler.GravitySensorHandler;
import uk.ac.cam.cares.jps.sensor.handler.GyroscopeHandler;
import uk.ac.cam.cares.jps.sensor.handler.LightSensorHandler;
import uk.ac.cam.cares.jps.sensor.handler.LocationHandler;
import uk.ac.cam.cares.jps.sensor.handler.MagnetometerHandler;
import uk.ac.cam.cares.jps.sensor.handler.PressureSensorHandler;
import uk.ac.cam.cares.jps.sensor.handler.RelativeHumiditySensorHandler;
import uk.ac.cam.cares.jps.sensor.handler.SensorHandler;

public class SensorManager {

    private SensorHandler accelerometerHandler;
    private SensorHandler gyroscopeHandler;
    private SensorHandler magnetometerHandler;
    private SensorHandler lightSensorHandler;
    private SensorHandler humiditySensorHandler;
    private SensorHandler pressureSensorHandler;
    private SensorHandler gravitySensorHandler;
    private LocationHandler locationTracker;

    public SensorManager(Context applicationContext) {
        android.hardware.SensorManager sensorManager = (android.hardware.SensorManager) applicationContext.getSystemService(SENSOR_SERVICE);
        accelerometerHandler = new AccelerometerHandler(sensorManager);
        gyroscopeHandler = new GyroscopeHandler(sensorManager);
        magnetometerHandler = new MagnetometerHandler(sensorManager);
        lightSensorHandler = new LightSensorHandler(sensorManager);
        humiditySensorHandler = new RelativeHumiditySensorHandler(sensorManager);
        pressureSensorHandler = new PressureSensorHandler(sensorManager);
        gravitySensorHandler = new GravitySensorHandler(sensorManager);
        locationTracker = new LocationHandler(applicationContext);
    }

    protected void startSensors() {
        accelerometerHandler.start();
        gyroscopeHandler.start();
        magnetometerHandler.start();
        lightSensorHandler.start();
        humiditySensorHandler.start();
        pressureSensorHandler.start();
        gravitySensorHandler.start();
        locationTracker.start();
    }

    protected void stopSensors() {
        accelerometerHandler.stop();
        gyroscopeHandler.stop();
        magnetometerHandler.stop();
        lightSensorHandler.stop();
        humiditySensorHandler.stop();
        pressureSensorHandler.stop();
        gravitySensorHandler.stop();
        locationTracker.stop();
    }

    protected JSONArray collectSensorData() {
        JSONArray allSensorData = new JSONArray();
        try {
            // Directly add all elements of each sensor's data to the single array
            addAllSensorData(allSensorData, accelerometerHandler.getSensorData());
            addAllSensorData(allSensorData, gyroscopeHandler.getSensorData());
            addAllSensorData(allSensorData, lightSensorHandler.getSensorData());
            addAllSensorData(allSensorData, humiditySensorHandler.getSensorData());
            addAllSensorData(allSensorData, pressureSensorHandler.getSensorData());
            addAllSensorData(allSensorData, gravitySensorHandler.getSensorData());
            addAllSensorData(allSensorData, magnetometerHandler.getSensorData());
            addAllSensorData(allSensorData, locationTracker.getLocationData());
            // add other sensors similarly
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return allSensorData;
    }

    private void addAllSensorData(JSONArray allSensorData, JSONArray sensorData) throws JSONException {
        for (int i = 0; i < sensorData.length(); i++) {
            allSensorData.put(sensorData.get(i));
        }
    }
}
