package com.example.notsensorlogger2;

import android.Manifest;
import android.app.Activity;
import android.content.Context;
import android.content.pm.PackageManager;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;

import androidx.core.app.ActivityCompat;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class LocationHandler implements LocationListener, SensorHandler, SensorEventListener {
    private Context context;
    private LocationManager locationManager;
    private SensorManager sensorManager;
    private Sensor pressureSensor;
    private float currentPressure = SensorManager.PRESSURE_STANDARD_ATMOSPHERE; // Default sea-level pressure
    private JSONArray locationData;
    private long startTime;

    public LocationHandler(Context context, PressureSensorHandler pressureSensorHandler) {
        this.context = context;
        this.sensorManager = (SensorManager) context.getSystemService(Context.SENSOR_SERVICE);
        this.pressureSensor = sensorManager.getDefaultSensor(Sensor.TYPE_PRESSURE);
        this.locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
        this.locationData = new JSONArray();
    }

        public void start() {
            Log.d("LocationHandler", "Starting location updates");
            if (ActivityCompat.checkSelfPermission(context, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
                ActivityCompat.requestPermissions((Activity) context, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, 1);
                return;
            }
            locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 500, 0, this);
            if (pressureSensor != null) {
                sensorManager.registerListener(this, pressureSensor, SensorManager.SENSOR_DELAY_NORMAL);
            }
            startTime = System.currentTimeMillis();
            Log.d("LocationHandler", "Location and pressure updates requested");
        }


    public void stop() {
        locationManager.removeUpdates(this);
        sensorManager.unregisterListener(this);
    }

    @Override
    public void onSensorChanged(SensorEvent event) {
        if (event.sensor.getType() == Sensor.TYPE_PRESSURE) {
            currentPressure = event.values[0];
            Log.d("Pressure Reading", "Current pressure: " + currentPressure + " hPa");
        }
    }

    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {

    }

    @Override
    public JSONArray getSensorData() {
        return locationData;
    }

    @Override
    public void onLocationChanged(Location location) {
        double altitude = SensorManager.getAltitude(SensorManager.PRESSURE_STANDARD_ATMOSPHERE, currentPressure);

        try {
            JSONObject locationObject = new JSONObject();
            locationObject.put("name", "location");
            locationObject.put("time", System.nanoTime());

            JSONObject values = new JSONObject();
            values.put("latitude", location.getLatitude());
            values.put("longitude", location.getLongitude());
            values.put("altitude", altitude);
            values.put("speed", location.getSpeed());
            values.put("bearing", location.getBearing());
            values.put("horizontalAccuracy", location.hasAccuracy() ? location.getAccuracy() : null);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                values.put("bearingAccuracy", location.hasBearingAccuracy() ? location.getBearingAccuracyDegrees() : null);
                values.put("speedAccuracy", location.hasSpeedAccuracy() ? location.getSpeedAccuracyMetersPerSecond() : null);
                values.put("verticalAccuracy", location.hasVerticalAccuracy() ? location.getVerticalAccuracyMeters() : null);
            }

            locationObject.put("values", values);
            locationData.put(locationObject);

            Log.d("Location Update", "Altitude: " + altitude);
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

        public JSONArray getLocationData() {
        return locationData;
    }

    @Override
    public void clearSensorData() {
        locationData = new JSONArray();
    }


    @Override
    public void onStatusChanged(String provider, int status, Bundle extras) {
        // Not used
    }

    @Override
    public void onProviderEnabled(String provider) {
        // Not used
    }

    @Override
    public void onProviderDisabled(String provider) {
        // Not used
    }

    private static float getAltitude(float seaLevelPressure, float pressure) {
        return SensorManager.getAltitude(seaLevelPressure, pressure);
    }
}
























//package com.example.notsensorlogger2;
//
//import android.Manifest;
//import android.app.Activity;
//import android.content.Context;
//import android.content.pm.PackageManager;
//import android.hardware.SensorManager;
//import android.location.Location;
//import android.location.LocationListener;
//import android.location.LocationManager;
//import android.os.Build;
//import android.os.Bundle;
//import android.util.Log;
//
//import androidx.core.app.ActivityCompat;
//
//import org.json.JSONArray;
//import org.json.JSONException;
//import org.json.JSONObject;
//
//public class LocationHandler implements LocationListener, SensorHandler {
//    private Context context;
//    private LocationManager locationManager;
//    private JSONArray locationData; // Change to JSONArray
//    private long startTime;
//    private String sensorName; // Added sensorName field
//
//    private static final float SEA_LEVEL_PRESSURE = SensorManager.PRESSURE_STANDARD_ATMOSPHERE;
//
//    public LocationHandler(Context context) {
//        this.context = context;
//        locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
//        locationData = new JSONArray();
//        this.sensorName = "location";
//    }
//
//    public void start() {
//        Log.d("LocationHandler", "Starting location updates");
//        if (ActivityCompat.checkSelfPermission(context, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
//            Log.d("LocationHandler", "Location permission not granted");
//            ActivityCompat.requestPermissions((Activity) context, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, 1);
//            return;
//        }
//        Log.d("LocationHandler", "Location permission granted");
//        locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 500, 0, this);
//        startTime = System.currentTimeMillis();
//        Log.d("LocationHandler", "Location updates requested");
//    }
//
//
//    public void stop() {
//        locationManager.removeUpdates(this);
//    }
//
//    @Override
//    public JSONArray getSensorData() {
//        return locationData;
//    }
//
//    @Override
//    public void onLocationChanged(Location location) {
//
//        try {
//            JSONObject locationObject = new JSONObject();
//            locationObject.put("name", this.sensorName);
//            locationObject.put("time", System.nanoTime());
//
//            JSONObject values = new JSONObject();
//            values.put("latitude", location.getLatitude());
//            values.put("longitude", location.getLongitude());
//            values.put("altitude", location.getAltitude());           // getAltitude(SEA_LEVEL_PRESSURE, (float) location.getAltitude()));
//            values.put("speed", location.getSpeed());
//            values.put("bearing", location.getBearing());
//            values.put("horizontalAccuracy", location.hasAccuracy() ? location.getAccuracy() : null);
//            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
//                values.put("bearingAccuracy", location.hasBearingAccuracy() ? location.getBearingAccuracyDegrees() : null);
//                values.put("speedAccuracy", location.hasSpeedAccuracy() ? location.getSpeedAccuracyMetersPerSecond() : null);
//                values.put("verticalAccuracy", location.hasVerticalAccuracy() ? location.getVerticalAccuracyMeters() : null);
//            }
//
//            locationObject.put("values", values);
//            locationData.put(locationObject);
//
//            // Log altitude in real-time
//            Log.d("GPS Data", "Alt: " + location.getAltitude());
//            Log.d("Latitude", "Latitude" + values.getDouble("latitude"));
//            Log.d("Longitude", "Longitude" + values.getDouble("longitude"));
//
//        } catch (JSONException e) {
//            e.printStackTrace();
//        }
//    }
//
//    public JSONArray getLocationData() {
//        return locationData;
//    }
//
//    @Override
//    public void clearSensorData() {
//        locationData = new JSONArray();
//    }
//
//
//    @Override
//    public void onStatusChanged(String provider, int status, Bundle extras) {
//        // Not used
//    }
//
//    @Override
//    public void onProviderEnabled(String provider) {
//        // Not used
//    }
//
//    @Override
//    public void onProviderDisabled(String provider) {
//        // Not used
//    }
//
//    private static float getAltitude(float seaLevelPressure, float pressure) {
//        return SensorManager.getAltitude(seaLevelPressure, pressure);
//    }
//}
