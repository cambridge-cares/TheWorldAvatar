package uk.ac.cam.cares.jps.routing.ui.manager;

import android.content.Context;
import android.location.Location;
import android.os.Looper;

import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.Priority;
import com.mapbox.geojson.Point;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.routing.viewmodel.LocationViewModel;

public class UserLocationManager {
    private Context context;
    private LocationRequest locationRequest;
    private FusedLocationProviderClient fusedLocationClient;
    private LocationViewModel locationViewModel;

    private Logger LOGGER = Logger.getLogger(UserLocationManager.class);

    public UserLocationManager(Fragment fragment) {
        this.context = fragment.requireContext();

        initUserLocationManager();

        locationViewModel = new ViewModelProvider(fragment).get(LocationViewModel.class);
    }

    private void initUserLocationManager() {
        locationRequest = new LocationRequest.Builder(1000)
                .setMinUpdateIntervalMillis(1000)
                .setPriority(Priority.PRIORITY_HIGH_ACCURACY)
                .build();
        fusedLocationClient = LocationServices.getFusedLocationProviderClient(context);
    }

    public void requestLastLocation() {

        fusedLocationClient.getLastLocation().addOnSuccessListener(location -> {
            if (location != null) {
                double latitude = location.getLatitude();
                double longitude = location.getLongitude();

                locationViewModel.setCurrentLocation(Point.fromLngLat(longitude, latitude));
            }
        });

    }

    public void startLocationUpdates() {
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
                locationViewModel.setCurrentLocation(Point.fromLngLat(longitude, latitude));
                LOGGER.debug("Update Location ");
            }
        }
    };
}
