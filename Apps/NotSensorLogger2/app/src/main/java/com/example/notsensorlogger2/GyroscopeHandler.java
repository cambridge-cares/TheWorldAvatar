package com.example.notsensorlogger2;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;

/**
 * Handles sensor events and data for the gyroscope sensor. This class extends {@link AbstractSensorHandler}
 * and manages sensor event registration, data collection,
 * and provides access to the collected data.
 *
 * The gyroscope sensor measures the rate of rotation around the device's three physical axes (x, y, and z).
 */
public class GyroscopeHandler extends AbstractSensorHandler {

    /**
     * Constructs a new GyroscopeHandler.
     *
     * @param sensorManager The sensor manager used to access the gyroscope.
     */
    public GyroscopeHandler(SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_GYROSCOPE);
        this.sensorName = "gyroscope";
    }

    /**
     * Handles changes in the gyroscope sensor values. This method overrides the onSensorChanged method
     * in the superclass to perform processing specific to gyroscope data.
     *
     * @param event The sensor event containing the new sensor readings.
     */
    @Override
    public void onSensorChanged(SensorEvent event) {
        super.onSensorChanged(event);
    }
}
