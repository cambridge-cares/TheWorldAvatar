package uk.ac.cam.cares.jps.sensor.source.handler;

import android.hardware.Sensor;
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

    @Override
    public SensorType getSensorType() {
        return SensorType.GYROSCOPE;
    }
}

