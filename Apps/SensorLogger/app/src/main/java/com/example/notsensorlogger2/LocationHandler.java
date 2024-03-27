package com.example.notsensorlogger2;

import android.Manifest;
import android.app.Activity;
import android.content.Context;
import android.content.pm.PackageManager;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;

import androidx.core.app.ActivityCompat;

import org.json.JSONException;
import org.json.JSONObject;

public class LocationHandler implements LocationListener {
    private Context context;
    private LocationManager locationManager;
    private JSONObject lastLocationData; // To store the latest location data
    private long startTime;

    public LocationHandler(Context context) {
        this.context = context;
        locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
        lastLocationData = new JSONObject();
    }

    public void start() {
        if (ActivityCompat.checkSelfPermission(context, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions((Activity) context, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, 1);
            return;
        }
        locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 1000, 0, this);
        startTime = System.currentTimeMillis();
    }

    public void stop() {
        locationManager.removeUpdates(this);
    }

    @Override
    public void onLocationChanged(Location location) {
        try {
            lastLocationData.put("name", "location");
            lastLocationData.put("time", System.currentTimeMillis() * 1000000); // Adjust timestamp format if necessary

            JSONObject values = new JSONObject();
            values.put("latitude", location.getLatitude());
            values.put("longitude", location.getLongitude());
            values.put("altitude", location.getAltitude());
            values.put("speed", location.getSpeed());
            values.put("bearing", location.getBearing());
            values.put("horizontalAccuracy", location.hasAccuracy() ? location.getAccuracy() : null);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                values.put("bearingAccuracy", location.hasBearingAccuracy() ? location.getBearingAccuracyDegrees() : null);
                values.put("speedAccuracy", location.hasSpeedAccuracy() ? location.getSpeedAccuracyMetersPerSecond() : null);
                values.put("verticalAccuracy", location.hasVerticalAccuracy() ? location.getVerticalAccuracyMeters() : null);
            }
            // Remove unsupported fields for Android versions before UPSIDE_DOWN_CAKE
            // values.put("mslAltitude", location.hasMslAltitude() ? location.getMslAltitudeMeters() : null);
            // values.put("mslAltitudeAccuracy", location.hasMslAltitudeAccuracy() ? location.getMslAltitudeAccuracyMeters() : null);

            lastLocationData.put("values", values);
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    public JSONObject getSensorData() {
        return lastLocationData;
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
}

















// Code for LocationHandler extending the abstract class, verify if this works later:



/*
package com.example.notsensorlogger2;

        import android.Manifest;
        import android.app.Activity;
        import android.content.Context;
        import android.content.pm.PackageManager;
        import android.hardware.SensorManager;
        import android.location.Location;
        import android.location.LocationListener;
        import android.location.LocationManager;
        import android.os.Build;
        import android.os.Bundle;
        import android.util.Log;

        import androidx.core.app.ActivityCompat;

        import org.json.JSONException;
        import org.json.JSONObject;

public class LocationHandler extends AbstractSensorHandler implements LocationListener {
    private LocationManager locationManager;

    public LocationHandler(Context context) {
        super((SensorManager) context.getSystemService(Context.SENSOR_SERVICE), 0); // Dummy sensorType, not used
        this.context = context;
        locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
    }

    @Override
    public void start() {
        super.start(); // Call the superclass start method
        if (ActivityCompat.checkSelfPermission(context, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions((Activity) context, new String[]{Manifest.permission.ACCESS_FINE_LOCATION}, 1);
            return;
        }
        locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 1000, 0, this);
    }

    @Override
    public void stop() {
        super.stop(); // Call the superclass stop method
        locationManager.removeUpdates(this);
    }

    @Override
    public void onSensorChanged(SensorEvent event) {
        // Not used for LocationHandler
    }

    @Override
    public void onLocationChanged(Location location) {
        super.onSensorChanged(null); // Call the superclass method to handle common functionality
        try {
            lastLocationData.put("name", "location");
            lastLocationData.put("time", System.currentTimeMillis() * 1000000); // Adjust timestamp format if necessary

            JSONObject values = new JSONObject();
            values.put("latitude", location.getLatitude());
            values.put("longitude", location.getLongitude());
            values.put("altitude", location.getAltitude());
            values.put("speed", location.getSpeed());
            values.put("bearing", location.getBearing());
            values.put("horizontalAccuracy", location.hasAccuracy() ? location.getAccuracy() : null);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                values.put("bearingAccuracy", location.hasBearingAccuracy() ? location.getBearingAccuracyDegrees() : null);
                values.put("speedAccuracy", location.hasSpeedAccuracy() ? location.getSpeedAccuracyMetersPerSecond() : null);
                values.put("verticalAccuracy", location.hasVerticalAccuracy() ? location.getVerticalAccuracyMeters() : null);
            }

            lastLocationData.put("values", values);
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    @Override
    public JSONObject getSensorData() {
        return lastLocationData;
    }
}
*/

