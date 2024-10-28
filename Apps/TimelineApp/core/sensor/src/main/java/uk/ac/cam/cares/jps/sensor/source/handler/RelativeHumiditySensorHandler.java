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
            dataPoint.put("time", System.currentTimeMillis());
            dataPoint.put("values", values);

            synchronized (sensorDataLock) {
                sensorData.put(dataPoint);
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    @Override
    public SensorType getSensorType() {
        return SensorType.HUMIDITY;
    }
}

