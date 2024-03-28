package com.example.notsensorlogger2;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.List;

public interface SensorHandler {
    void start();
    void stop();
    JSONObject getSensorData();
}

