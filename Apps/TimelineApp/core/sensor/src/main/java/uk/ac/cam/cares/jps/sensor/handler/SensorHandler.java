package uk.ac.cam.cares.jps.sensor.handler;

import org.json.JSONArray;

public interface SensorHandler {
    void start();
    void stop();
    JSONArray getSensorData();
}

