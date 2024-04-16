package uk.ac.cam.cares.jps.sensor.handler;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.util.Log;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public abstract class AbstractSensorHandler implements SensorHandler, SensorEventListener {
    protected SensorManager sensorManager;
    protected Sensor sensor;
    protected JSONArray sensorData;
    protected long startTime;
    protected String sensorName;

    public AbstractSensorHandler(SensorManager sensorManager, int sensorType) {
        this.sensorManager = sensorManager;
        this.sensor = sensorManager.getDefaultSensor(sensorType);
        this.sensorData = new JSONArray();
        if (this.sensor == null) {
            Log.e("SensorError", "Sensor not available on this device.");
        }
    }

    @Override
    public void start() {
        sensorManager.registerListener(this, sensor, SensorManager.SENSOR_DELAY_NORMAL);
        startTime = System.currentTimeMillis();
    }

    @Override
    public void stop() {
        sensorManager.unregisterListener(this);
        long endTime = System.currentTimeMillis();
        long duration = endTime - startTime;
        Log.d("SensorRecording", "Sensor: " + sensorName + ", Duration: " + duration + "ms");
    }

    @Override
    public JSONArray getSensorData() {
        return sensorData;
    }

    @Override
    public void onSensorChanged(SensorEvent event) {
        JSONObject dataPoint = new JSONObject();
        try {
            JSONObject values = new JSONObject();
            values.put("x", event.values[0]);
            values.put("y", event.values[1]);
            values.put("z", event.values[2]);

            dataPoint.put("name", this.sensorName);
            dataPoint.put("time", System.nanoTime());
            dataPoint.put("values", values);

            synchronized (this) {
                sensorData.put(dataPoint);
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    public void clearSensorData() {
        sensorData = new JSONArray();
    }

    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {
        // Log.d("SensorAccuracy", "Accuracy of sensor " + sensor.getName() + " has changed to " + accuracy);
        // only implement if necessary
    }
}
