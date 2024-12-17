package uk.ac.cam.cares.jps.sensor.source.handler;

import android.hardware.Sensor;
import android.hardware.SensorManager;

/**
 * Handles accelerometer sensor data collection and events. This class extends {@link AbstractSensorHandler}
 * and utilizes the framework provided by the abstract class
 * to manage sensor registration, data collection, and event handling.
 */
public class AccelerometerHandler extends AbstractSensorHandler {

    /**
     * Constructs an AccelerometerHandler for managing accelerometer sensor data.
     *
     * @param sensorManager The system's sensor service manager used to access the accelerometer.
     */
    public AccelerometerHandler(SensorManager sensorManager) {
        super(sensorManager, Sensor.TYPE_ACCELEROMETER);
        this.sensorName = "accelerometer";
    }

    @Override
    public SensorType getSensorType() {
        return SensorType.ACCELEROMETER;
    }
}




