package uk.ac.cam.cares.jps.sensor.source.handler;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Handles pressure sensor data collection. This handler is responsible for capturing pressure
 * readings from the device's pressure sensor and storing them in a structured JSON format.
 * Inherits from {@link AbstractSensorHandler}.
 */
public class PressureSensorHandler extends AbstractSensorHandler {

    /**
     * Constructor that initializes the PressureSensorHandler with a specific sensor type.
     *
     * @param sensorManager The system's sensor service manager.
     */
    public PressureSensorHandler(SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_PRESSURE);
        this.sensorName = "pressure";
    }

    /**
     * Captures pressure sensor changes and logs them. Each sensor event data is encapsulated
     * as a JSON object, including the sensor name, the timestamp, and the current pressure reading.
     *
     * @param event The sensor event containing the latest pressure readings.
     */
    @Override
    public void onSensorChanged(SensorEvent event) {
        JSONObject dataPoint = new JSONObject();
        try {
            JSONObject values = new JSONObject();
            values.put("pressure", event.values[0]);

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
        return SensorType.PRESSURE;
    }
}

