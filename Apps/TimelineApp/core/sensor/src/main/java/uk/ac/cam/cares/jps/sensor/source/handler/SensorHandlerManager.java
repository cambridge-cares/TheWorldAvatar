package uk.ac.cam.cares.jps.sensor.source.handler;

import static android.content.Context.SENSOR_SERVICE;

import android.content.Context;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

/**
 * A class to manage sensor handlers. It is considered as a data source level component.
 * Functionalities:
 * 1. Start/stop all sensor handlers
 * 2. Collect all sensor data and prepare them for network upload (by SensorNetworkSource) and local storage (by SensorLocalSource) and clear the in memory accumulated data
 */
public class SensorHandlerManager {

    private Logger LOGGER = Logger.getLogger(SensorHandlerManager.class);
    private final SensorHandler[] sensorHandlers;
    private Map<SensorType, SensorHandler> sensorHandlersMap;
    private Map<SensorType, Integer> samplingRatesMap;


    @Inject
    public SensorHandlerManager(Context applicationContext) {
        android.hardware.SensorManager sensorManager = (android.hardware.SensorManager) applicationContext.getSystemService(SENSOR_SERVICE);
        samplingRatesMap = loadSamplingRatesConfig(applicationContext);
        sensorHandlersMap = new HashMap<>();

        AccelerometerHandler accelerometerHandler = new AccelerometerHandler(sensorManager);
        GyroscopeHandler gyroscopeHandler = new GyroscopeHandler(sensorManager);
        MagnetometerHandler magnetometerHandler = new MagnetometerHandler(sensorManager);
        LightSensorHandler lightSensorHandler = new LightSensorHandler(sensorManager);
        RelativeHumiditySensorHandler humidityHandler = new RelativeHumiditySensorHandler(sensorManager);
        PressureSensorHandler pressureHandler = new PressureSensorHandler(sensorManager);
        GravitySensorHandler gravityHandler = new GravitySensorHandler(sensorManager);
        LocationHandler locationHandler = new LocationHandler(applicationContext);
        SoundLevelHandler soundHandler = new SoundLevelHandler(applicationContext, sensorManager);

        sensorHandlersMap.put(SensorType.ACCELEROMETER, accelerometerHandler);
        sensorHandlersMap.put(SensorType.GYROSCOPE, gyroscopeHandler);
        sensorHandlersMap.put(SensorType.MAGNETOMETER, magnetometerHandler);
        sensorHandlersMap.put(SensorType.LIGHT, lightSensorHandler);
        sensorHandlersMap.put(SensorType.HUMIDITY, humidityHandler);
        sensorHandlersMap.put(SensorType.PRESSURE, pressureHandler);
        sensorHandlersMap.put(SensorType.GRAVITY, gravityHandler);
        sensorHandlersMap.put(SensorType.LOCATION, locationHandler);
        sensorHandlersMap.put(SensorType.SOUND, soundHandler);

        sensorHandlers = sensorHandlersMap.values().toArray(new SensorHandler[0]);

    }

    private Map<SensorType, Integer> loadSamplingRatesConfig(Context context) {
        Map<SensorType, Integer> rates = new HashMap<>();
        try {
            InputStream is = context.getAssets().open("sensor_config.json");
            int size = is.available();
            byte[] buffer = new byte[size];
            is.read(buffer);
            is.close();
            String json = new String(buffer, StandardCharsets.UTF_8);

            JSONObject config = new JSONObject(json).getJSONObject("samplingRates");
            rates.put(SensorType.ACCELEROMETER, parseSamplingRate(config.getString("accelerometer")));
            rates.put(SensorType.GYROSCOPE, parseSamplingRate(config.getString("gyroscope")));
            rates.put(SensorType.MAGNETOMETER, parseSamplingRate(config.getString("magnetometer")));
            rates.put(SensorType.LIGHT, parseSamplingRate(config.getString("light")));
            rates.put(SensorType.HUMIDITY, parseSamplingRate(config.getString("humidity")));
            rates.put(SensorType.PRESSURE, parseSamplingRate(config.getString("pressure")));
            rates.put(SensorType.GRAVITY, parseSamplingRate(config.getString("gravity")));
            rates.put(SensorType.LOCATION, parseSamplingRate(config.getString("location")));
            rates.put(SensorType.SOUND, parseSamplingRate(config.getString("sound")));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return rates;
    }

    private int parseSamplingRate(String rate) {
        switch (rate) {
            case "SENSOR_DELAY_UI":
                return android.hardware.SensorManager.SENSOR_DELAY_UI;
            case "SENSOR_DELAY_GAME":
                return android.hardware.SensorManager.SENSOR_DELAY_GAME;
            case "SENSOR_DELAY_FASTEST":
                return android.hardware.SensorManager.SENSOR_DELAY_FASTEST;
            case "SENSOR_DELAY_NORMAL":
            default:
                return android.hardware.SensorManager.SENSOR_DELAY_NORMAL;
        }
    }

    /**
     * Start selected sensor handlers
     */
    public void startSelectedSensors(List<SensorType> selectedSensorTypes) {
        for (SensorType type : selectedSensorTypes) {
            SensorHandler handler = sensorHandlersMap.get(type);
            if (handler != null) {
                handler.start(samplingRatesMap.get(type));
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
    public Map<String, JSONArray> collectSensorData() {
        Map<String, JSONArray> localStorageData = new HashMap<>();
        // Directly add all elements of each sensor's data to the single array
        for(SensorHandler handler : sensorHandlersMap.values()) {
            if (handler.isRunning()) {
                JSONArray sensorData;
                synchronized (handler.getSensorDataLock()) {
                    sensorData = handler.getSensorData();
                    handler.clearSensorData();
                }
                localStorageData.put(handler.getSensorName(), sensorData);
            }
        }

        return localStorageData;
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
