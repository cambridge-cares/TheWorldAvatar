package uk.ac.cam.cares.sensorlogger;


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
}
