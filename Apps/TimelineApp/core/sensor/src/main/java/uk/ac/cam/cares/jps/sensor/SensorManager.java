package uk.ac.cam.cares.jps.sensor;

import static android.content.Context.SENSOR_SERVICE;

import android.content.Context;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;

import java.util.HashMap;
import java.util.Map;

import kotlin.Pair;
import uk.ac.cam.cares.jps.sensor.handler.AccelerometerHandler;
import uk.ac.cam.cares.jps.sensor.handler.GravitySensorHandler;
import uk.ac.cam.cares.jps.sensor.handler.GyroscopeHandler;
import uk.ac.cam.cares.jps.sensor.handler.LightSensorHandler;
import uk.ac.cam.cares.jps.sensor.handler.LocationHandler;
import uk.ac.cam.cares.jps.sensor.handler.MagnetometerHandler;
import uk.ac.cam.cares.jps.sensor.handler.PressureSensorHandler;
import uk.ac.cam.cares.jps.sensor.handler.RelativeHumiditySensorHandler;
import uk.ac.cam.cares.jps.sensor.handler.SensorHandler;
import uk.ac.cam.cares.jps.sensor.handler.SoundLevelHandler;

public class SensorManager {

    private SensorHandler accelerometerHandler;
    private SensorHandler gyroscopeHandler;
    private SensorHandler magnetometerHandler;
    private SensorHandler lightSensorHandler;
    private SensorHandler humiditySensorHandler;
    private SensorHandler pressureSensorHandler;
    private SensorHandler gravitySensorHandler;
    private LocationHandler locationTracker;
    private SoundLevelHandler soundLevelHandler;

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

    protected void startSensors() {
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

    protected void stopSensors() {
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

    protected Pair<JSONArray, Map<String, JSONArray>> collectSensorData() {
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
