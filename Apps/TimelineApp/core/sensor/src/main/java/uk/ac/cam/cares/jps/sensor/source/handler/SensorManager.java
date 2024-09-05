package uk.ac.cam.cares.jps.sensor.source.handler;

import static android.content.Context.SENSOR_SERVICE;

import android.content.Context;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;

import java.util.HashMap;
import java.util.List;
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
    private Map<SensorType, SensorHandler> sensorHandlersMap;

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

        sensorHandlersMap = new HashMap<>();
        sensorHandlersMap.put(SensorType.ACCELEROMETER, new AccelerometerHandler(sensorManager));
        sensorHandlersMap.put(SensorType.GYROSCOPE, new GyroscopeHandler(sensorManager));
        sensorHandlersMap.put(SensorType.MAGNETOMETER, new MagnetometerHandler(sensorManager));
        sensorHandlersMap.put(SensorType.LIGHT, new LightSensorHandler(sensorManager));
        sensorHandlersMap.put(SensorType.HUMIDITY, new RelativeHumiditySensorHandler(sensorManager));
        sensorHandlersMap.put(SensorType.PRESSURE, new PressureSensorHandler(sensorManager));
        sensorHandlersMap.put(SensorType.GRAVITY, new GravitySensorHandler(sensorManager));
        sensorHandlersMap.put(SensorType.LOCATION, new LocationHandler(applicationContext));
        sensorHandlersMap.put(SensorType.SOUND, new SoundLevelHandler(applicationContext, sensorManager));

    }

    /**
     * Start selected sensor handlers
     */
    public void startSelectedSensors(List<SensorType> selectedSensorTypes) {
        for (SensorType type : selectedSensorTypes) {
            SensorHandler handler = sensorHandlersMap.get(type);
            if (handler != null) {
                handler.start();
            }
        }
        LOGGER.info("Selected sensors started");
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
            for(SensorHandler handler : sensorHandlersMap.values()) {
                if (handler.isRunning()) {
                    JSONArray sensorData = handler.getSensorData();
                    addAllSensorData(allSensorData, sensorData);
                    localStorageData.put(handler.getSensorName(), sensorData);
                }
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
        for (SensorHandler handler : sensorHandlersMap.values()) {
            if (handler.isRunning()) {
                handler.clearSensorData();
            }
        }
    }

    // returns sensor handler to SensorSettingFragment class
    public SensorHandler getSensorHandler(SensorType type) {
        return sensorHandlersMap.get(type);
    }

}
