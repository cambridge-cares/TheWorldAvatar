package uk.ac.cam.cares.jps.sensor.source.handler;

import android.Manifest;
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

import androidx.core.app.ActivityCompat;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Handles location updates and atmospheric pressure readings. This class integrates with both the
 * LocationManager for location updates and the SensorManager for pressure data, providing comprehensive
 * environmental data through a unified interface.
 *
 * It provides functionality to start and stop location and pressure monitoring, handle changes, and
 * manage the collected data in a structured JSON format.
 */
public class LocationHandler implements LocationListener, SensorHandler, SensorEventListener {
    private Context context;
    private LocationManager locationManager;
    private SensorManager sensorManager;
    private Sensor pressureSensor;
    private float currentPressure = SensorManager.PRESSURE_STANDARD_ATMOSPHERE;
    private JSONArray locationData;
    private int mslConstant; // Mean sea level pressure constant for altitude calculations
    private Logger LOGGER = Logger.getLogger(LocationHandler.class);
    private boolean isRunning = false;

    /**
     * Constructs a LocationHandler with a specified context and initializes location and pressure sensors.
     *
     * @param context The application context used for accessing system services.
     */
    public LocationHandler(Context context) {
        this.context = context;
        this.sensorManager = (SensorManager) context.getSystemService(Context.SENSOR_SERVICE);
        this.pressureSensor = sensorManager.getDefaultSensor(Sensor.TYPE_PRESSURE);
        this.locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
        this.locationData = new JSONArray();
        this.mslConstant = 1006;
    }

    /**
     * Starts location and pressure data updates. Requires fine location permission to function properly.
     */
    public void start() {
        if (ActivityCompat.checkSelfPermission(context, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            LOGGER.warn("Location permission not granted. Location handler failed to start. Request for permission should be handled in fragment.");
            return;
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 2000, 2, this);
        } else {
            locationManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 2000, 2, this);
        }

        if (pressureSensor != null) {
            sensorManager.registerListener(this, pressureSensor, SensorManager.SENSOR_DELAY_NORMAL);
        }
        this.isRunning = true;
    }

    /**
     * Stops location and pressure data updates.
     */
    @Override
    public void stop() {
        locationManager.removeUpdates(this);
        sensorManager.unregisterListener(this);
        this.isRunning = false;
    }

    /**
     * Callback for sensor data changes, specifically for the atmospheric pressure sensor.
     *
     * @param event The sensor event containing the new readings.
     */
    @Override
    public void onSensorChanged(SensorEvent event) {
        if (event.sensor.getType() == Sensor.TYPE_PRESSURE) {
            currentPressure = event.values[0];
        }
    }

    /**
     * Callback for location changes, which includes calculation of altitude using atmospheric pressure.
     *
     * @param location The new location object containing updated latitude, longitude, and other data.
     */
    @Override
    public void onLocationChanged(Location location) {
        double altitude = SensorManager.getAltitude(mslConstant, currentPressure);

        try {
            JSONObject locationObject = new JSONObject();
            locationObject.put("name", "location");
            locationObject.put("time", System.currentTimeMillis() * 1000000);
            JSONObject values = new JSONObject();
            values.put("latitude", location.getLatitude());
            values.put("longitude", location.getLongitude());
            values.put("altitude", altitude);
            values.put("speed", location.getSpeed());
            values.put("bearing", location.getBearing());
            values.put("horizontalAccuracy", location.hasAccuracy() ? location.getAccuracy() : null);
            values.put("bearingAccuracy", location.hasBearingAccuracy() ? location.getBearingAccuracyDegrees() : null);
            values.put("speedAccuracy", location.hasSpeedAccuracy() ? location.getSpeedAccuracyMetersPerSecond() : null);
            values.put("verticalAccuracy", location.hasVerticalAccuracy() ? location.getVerticalAccuracyMeters() : null);

            locationObject.put("values", values);
            locationData.put(locationObject);

        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    /**
     * Clears the stored sensor data.
     */
    @Override
    public void clearSensorData() {
        locationData = new JSONArray();
    }

    /**
     * Retrieves the collected location data.
     *
     * @return A {@link JSONArray} containing structured JSON objects of the location data.
     */
    @Override
    public JSONArray getSensorData() {
        return locationData;
    }

    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {
        // Not needed to implement
    }

    @Override
    public String getSensorName() {
        return "location";
    }

    @Override
    public Boolean isRunning() {
        return isRunning;
    }

    @Override
    public SensorType getSensorType() {
        return SensorType.LOCATION;
    }
}
