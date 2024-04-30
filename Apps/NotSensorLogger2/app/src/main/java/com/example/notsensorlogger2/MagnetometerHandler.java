package com.example.notsensorlogger2;

import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorManager;

/**
 * Handles magnetometer data collection and processing. This class extends the abstract sensor handler
 * and captures magnetic field data from the device's magnetometer
 * and formats it for logging or further analysis.
 */
public class MagnetometerHandler extends AbstractSensorHandler {

    /**
     * Constructs a MagnetometerHandler with the specified sensor manager. It initializes the handler
     * for the magnetometer sensor.
     *
     * @param sensorManager The sensor manager used to access the magnetometer.
     */
    public MagnetometerHandler(SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_MAGNETIC_FIELD);
        this.sensorName = "magnetometer";
    }

    /**
     * Handles the sensor changed event by capturing magnetometer data and storing it. This method extends
     * the general sensor changed handling by adding magnetometer-specific data processing.
     *
     * @param event The sensor event containing the new magnetometer readings.
     */
    @Override
    public void onSensorChanged(SensorEvent event) {
        super.onSensorChanged(event);
    }
}
