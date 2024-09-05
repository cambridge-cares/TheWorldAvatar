package uk.ac.cam.cares.jps.sensor.source.handler;

import static android.content.Context.SENSOR_SERVICE;

import android.content.Context;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;

import java.util.HashMap;
import java.util.Map;

import kotlin.Pair;

/**
 * A class to manage sensor handlers. It is considered as a data source level component.
 * Functionalities:
 * 1. Start/stop all sensor handlers
 * 2. Collect all sensor data and prepare them for network upload (by SensorNetworkSource) and local storage (by SensorLocalSource) and clear the in memory accumulated data
 */
public class SensorManager {

    private final SensorHandler accelerometerHandler;
    private final SensorHandler gyroscopeHandler;
    private final SensorHandler magnetometerHandler;
    private final SensorHandler lightSensorHandler;
    private final SensorHandler humiditySensorHandler;
    private final SensorHandler pressureSensorHandler;
    private final SensorHandler gravitySensorHandler;
    private final LocationHandler locationTracker;
    private final SoundLevelHandler soundLevelHandler;

    private Logger LOGGER = Logger.getLogger(SensorManager.class);

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
        soundLevelHandler = new SoundLevelHandler(applicationContext, sensorManager);
    }

    /**
     * Start all sensor handlers
     */
    public void startSensors() {
        accelerometerHandler.start();
        gyroscopeHandler.start();
        magnetometerHandler.start();
        lightSensorHandler.start();
        humiditySensorHandler.start();
        pressureSensorHandler.start();
        gravitySensorHandler.start();
        locationTracker.start();
        soundLevelHandler.start();

        LOGGER.info("sensors started");
    }

    /**
     * Stop all sensor handlers
     */
    public void stopSensors() {
        accelerometerHandler.stop();
        gyroscopeHandler.stop();
        magnetometerHandler.stop();
        lightSensorHandler.stop();
        humiditySensorHandler.stop();
        pressureSensorHandler.stop();
        gravitySensorHandler.stop();
        locationTracker.stop();
        soundLevelHandler.stop();

        LOGGER.info("sensors stopped");
    }

    /**
     * Collect in memory sensor data from handler objects and prepare them in the format for network upload and local storage.
     * Network format: a JSONArray
     * Local format: a Map
     * @return pair (network data, local data)
     */
    public Pair<JSONArray, Map<String, JSONArray>> collectSensorData() {
        JSONArray allSensorData = new JSONArray();
        Map<String, JSONArray> localStorageData = new HashMap<>();
        try {
            // Directly add all elements of each sensor's data to the single array
            addAllSensorData(allSensorData, accelerometerHandler.getSensorData());
            addAllSensorData(allSensorData, gyroscopeHandler.getSensorData());
            addAllSensorData(allSensorData, lightSensorHandler.getSensorData());
            addAllSensorData(allSensorData, humiditySensorHandler.getSensorData());
            addAllSensorData(allSensorData, pressureSensorHandler.getSensorData());
            addAllSensorData(allSensorData, gravitySensorHandler.getSensorData());
            addAllSensorData(allSensorData, magnetometerHandler.getSensorData());
            addAllSensorData(allSensorData, locationTracker.getSensorData());
            addAllSensorData(allSensorData, soundLevelHandler.getSensorData());
            // add other sensors similarly

            localStorageData.put(accelerometerHandler.getSensorName(), accelerometerHandler.getSensorData());
            localStorageData.put(gyroscopeHandler.getSensorName(), gyroscopeHandler.getSensorData());
            localStorageData.put(lightSensorHandler.getSensorName(), lightSensorHandler.getSensorData());
            localStorageData.put(humiditySensorHandler.getSensorName(), humiditySensorHandler.getSensorData());
            localStorageData.put(pressureSensorHandler.getSensorName(), pressureSensorHandler.getSensorData());
            localStorageData.put(gravitySensorHandler.getSensorName(), gravitySensorHandler.getSensorData());
            localStorageData.put(magnetometerHandler.getSensorName(), magnetometerHandler.getSensorData());
            localStorageData.put(locationTracker.getSensorName(), locationTracker.getSensorData());
            localStorageData.put(soundLevelHandler.getSensorName(), soundLevelHandler.getSensorData());

        } catch (JSONException e) {
            e.printStackTrace();
        }

        clearAllSensorData();
        return new Pair<>(allSensorData, localStorageData);
    }

    private void addAllSensorData(JSONArray allSensorData, JSONArray sensorData) throws JSONException {
        for (int i = 0; i < sensorData.length(); i++) {
            allSensorData.put(sensorData.get(i));
        }
    }

    /**
     * Clears all sensor data from the memory.
     */
    private void clearAllSensorData() {
        accelerometerHandler.clearSensorData();
        gyroscopeHandler.clearSensorData();
        magnetometerHandler.clearSensorData();
        lightSensorHandler.clearSensorData();
        humiditySensorHandler.clearSensorData();
        pressureSensorHandler.clearSensorData();
        gravitySensorHandler.clearSensorData();
        locationTracker.clearSensorData();
        soundLevelHandler.clearSensorData();
    }
}
