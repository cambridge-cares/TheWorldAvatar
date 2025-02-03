package uk.ac.cam.cares.sensorlogger;


import org.json.JSONArray;

/**
 * Interface defining the basic operations for handling sensor data collection and management.
 * This includes starting and stopping data collection, retrieving collected data, and clearing stored data.
 */
public interface SensorHandler {

    /**
     * Starts the sensor data collection process.
     */
    void start();

    /**
     * Stops the sensor data collection process.
     */
    void stop();

    /**
     * Retrieves the collected sensor data as a JSONArray.
     * Each entry in the JSONArray represents a single reading from the sensor, including time and value metrics.
     *
     * @return A JSONArray containing the collected sensor data.
     */
    JSONArray getSensorData();

    /**
     * Clears all data previously collected by the sensor handler.
     */
    void clearSensorData();
}


