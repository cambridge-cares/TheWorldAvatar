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

    private Logger LOGGER = Logger.getLogger(SensorManager.class);
    private final SensorHandler[] sensorHandlers;

    public SensorManager(Context applicationContext) {
        android.hardware.SensorManager sensorManager = (android.hardware.SensorManager) applicationContext.getSystemService(SENSOR_SERVICE);
        sensorHandlers = new SensorHandler[]{
                new AccelerometerHandler(sensorManager),
                new GyroscopeHandler(sensorManager),
                new MagnetometerHandler(sensorManager),
                new LightSensorHandler(sensorManager),
                new RelativeHumiditySensorHandler(sensorManager),
                new PressureSensorHandler(sensorManager),
                new GravitySensorHandler(sensorManager),
                new LocationHandler(applicationContext),
                new SoundLevelHandler(applicationContext, sensorManager)
        };

    }

    /**
     * Start all sensor handlers
     */
    public void startSensors() {

        for(SensorHandler handler : sensorHandlers) {
            handler.start();
        }

        LOGGER.info("sensors started");
    }

    /**
     * Stop all sensor handlers
     */
    public void stopSensors() {

        for(SensorHandler handler : sensorHandlers) {
            handler.stop();
        }

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
            for(SensorHandler handler : sensorHandlers) {
                JSONArray sensorData = handler.getSensorData();
                addAllSensorData(allSensorData, sensorData);
                localStorageData.put(handler.getSensorName(), sensorData);
            }

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
        for (SensorHandler handler : sensorHandlers) {
            handler.clearSensorData();
        }
    }
}
