package uk.ac.cam.cares.jps.routing;

import android.content.Context;
import android.content.pm.PackageManager;
import android.location.Location;
import android.os.Looper;

import androidx.core.app.ActivityCompat;
import androidx.fragment.app.Fragment;

import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.Priority;
import com.mapbox.geojson.Point;
import com.mapbox.maps.MapView;
import com.mapbox.maps.plugin.Plugin;
import com.mapbox.maps.plugin.locationcomponent.LocationComponentPlugin;
import com.mapbox.maps.plugin.locationcomponent.OnIndicatorPositionChangedListener;

import org.apache.log4j.Logger;

public class LocationManager {
    private Context context;
    private LocationRequest locationRequest;
    private FusedLocationProviderClient fusedLocationClient;
    private Point currentLocation;

    private Logger LOGGER = Logger.getLogger(LocationManager.class);

    public LocationManager(MapView mapView, Fragment fragment) {
        this.context = fragment.requireContext();

        requestLocationPermission();

        LocationComponentPlugin locationComponent = mapView.getPlugin(Plugin.MAPBOX_LOCATION_COMPONENT_PLUGIN_ID);
        locationComponent.addOnIndicatorPositionChangedListener(onIndicatorPositionChangedListener);
        locationComponent.setEnabled(true);
    }

    private OnIndicatorPositionChangedListener onIndicatorPositionChangedListener = positionPoint -> {
//        mapView.getMapboxMap().setCamera(new CameraOptions.Builder().center(positionPoint).build());
        currentLocation = Point.fromLngLat(positionPoint.longitude(), positionPoint.latitude());
    };

    private void requestLocationPermission() {
        locationRequest = new LocationRequest.Builder(1000)
                .setMinUpdateIntervalMillis(1000)
                .setPriority(Priority.PRIORITY_HIGH_ACCURACY)
                .build();
        fusedLocationClient = LocationServices.getFusedLocationProviderClient(context);
    }

    public void requestLastLocation() {
        checkPermission();

        fusedLocationClient.getLastLocation().addOnSuccessListener(location -> {
            if (location != null) {
                double latitude = location.getLatitude();
                double longitude = location.getLongitude();
                // Add user location marker

                currentLocation = Point.fromLngLat(longitude, latitude);
            }
        });

    }

    public void startLocationUpdates() {
        checkPermission();
        fusedLocationClient.requestLocationUpdates(locationRequest, locationCallback, Looper.getMainLooper());
    }

    public void stopLocationUpdates() {
        fusedLocationClient.removeLocationUpdates(locationCallback);
    }

    private LocationCallback locationCallback = new LocationCallback() {
        @Override
        public void onLocationResult(LocationResult locationResult) {
            if (locationResult == null) {
                return;
            }
            for (Location location : locationResult.getLocations()) {
                double latitude = location.getLatitude();
                double longitude = location.getLongitude();
                currentLocation = Point.fromLngLat(longitude, latitude);
                LOGGER.debug("Update Location ");
            }
        }
    };

    private void checkPermission() {
        if (ActivityCompat.checkSelfPermission(context,
                android.Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED
                && ActivityCompat.checkSelfPermission(context,
                android.Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            requestLocationPermission();
        }
    }

    public Point getCurrentLocation() {
        return currentLocation;
    }
}
