package uk.ac.cam.cares.sensorlogger;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.util.Log;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Abstract class for handling sensor data from various sensors provided by the Android platform.
 * This class provides a framework for registering with the sensor service, collecting sensor data,
 * and handling sensor events. It also tracks the duration of the sensor data collection.
 *
 * Subclasses should implement specific sensor handling logic and potentially override
 * the methods for starting and stopping the sensor data collection, as well as handling sensor events.
 */
public abstract class AbstractSensorHandler implements SensorHandler, SensorEventListener {
    protected SensorManager sensorManager;
    protected Sensor sensor;
    protected JSONArray sensorData;
    protected long startTime;
    protected String sensorName;

    /**
     * Constructs an AbstractSensorHandler that initializes the sensor and sensor data management.
     *
     * @param sensorManager The system's sensor service manager.
     * @param sensorType The specific type of sensor to be managed.
     */
    public AbstractSensorHandler(SensorManager sensorManager, int sensorType) {
        this.sensorManager = sensorManager;
        this.sensor = sensorManager.getDefaultSensor(sensorType);
        this.sensorData = new JSONArray();
        if (this.sensor == null) {
            Log.e("SensorError", "Sensor not available on this device.");
        }
    }

    /**
     * Starts the sensor monitoring by registering the sensor event listener.
     * Initializes the start time for tracking sensor data recording duration.
     */
    @Override
    public void start() {
        sensorManager.registerListener(this, sensor, SensorManager.SENSOR_DELAY_NORMAL);
        startTime = System.currentTimeMillis() * 1000000;
    }

    /**
     * Stops the sensor monitoring by unregistering the sensor event listener.
     * Calculates and logs the duration of the sensor data recording.
     */
    @Override
    public void stop() {
        sensorManager.unregisterListener(this);
    }

    /**
     * Retrieves the collected sensor data.
     *
     * @return A JSONArray containing the sensor data collected during the session.
     */
    @Override
    public JSONArray getSensorData() {
        return sensorData;
    }

    /**
     * Handles the sensor data upon receiving a new sensor event.
     * Constructs a JSONObject for each sensor event and adds it to the sensor data JSONArray.
     *
     * @param event The sensor event containing the latest sensor data.
     */
    @Override
    public void onSensorChanged(SensorEvent event) {
        JSONObject dataPoint = new JSONObject();
        try {
            JSONObject values = new JSONObject();
            values.put("x", event.values[0]);
            values.put("y", event.values[1]);
            values.put("z", event.values[2]);

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

    /**
     * Clears all the collected sensor data from the session.
     */
    public void clearSensorData() {
        sensorData = new JSONArray();
    }

    /**
     * Handles changes in sensor accuracy, though not specifically implemented (no need).
     *
     * @param sensor The sensor whose accuracy has changed.
     * @param accuracy The new accuracy of this sensor.
     */
    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {
        // Not needed for implementation
    }
}
