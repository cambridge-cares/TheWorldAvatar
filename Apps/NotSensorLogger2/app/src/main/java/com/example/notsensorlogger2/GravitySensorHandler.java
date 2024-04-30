package com.example.notsensorlogger2;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;

/**
 * Handles sensor events and data for the gravity sensor. This class extends {@link AbstractSensorHandler}
 * and manages sensor event registration, data collection,
 * and provides access to the collected data.
 *
 * The gravity sensor provides a three-dimensional vector indicating the direction and magnitude of gravity.
 */
public class GravitySensorHandler extends AbstractSensorHandler {

    /**
     * Constructs a new GravitySensorHandler.
     *
     * @param sensorManager The sensor manager used to access the gravity sensor.
     */
    public GravitySensorHandler(SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_GRAVITY);
        this.sensorName = "gravity";
    }

    /**
     * Handles changes in the gravity sensor values. This method overrides the onSensorChanged method
     * in the superclass to perform processing specific to gravity sensor data.
     *
     * @param event The sensor event containing the new sensor readings.
     */
    @Override
    public void onSensorChanged(SensorEvent event) {
        super.onSensorChanged(event);
    }
}
