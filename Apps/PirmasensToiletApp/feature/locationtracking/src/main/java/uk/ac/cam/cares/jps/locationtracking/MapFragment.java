package uk.ac.cam.cares.jps.locationtracking;

import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.content.res.AppCompatResources;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.mapbox.android.core.location.LocationEngine;
import com.mapbox.android.core.location.LocationEngineProvider;
import com.mapbox.android.gestures.MoveGestureDetector;
import com.mapbox.maps.CameraOptions;
import com.mapbox.maps.MapView;
import com.mapbox.maps.MapboxMap;
import com.mapbox.maps.Style;
import com.mapbox.maps.extension.style.expressions.generated.Expression;
import com.mapbox.maps.plugin.LocationPuck2D;
import com.mapbox.maps.plugin.gestures.GesturesUtils;
import com.mapbox.maps.plugin.gestures.OnMoveListener;
import com.mapbox.maps.plugin.locationcomponent.LocationComponentPlugin;
import com.mapbox.maps.plugin.locationcomponent.LocationComponentUtils;
import com.mapbox.maps.plugin.locationcomponent.OnIndicatorBearingChangedListener;
import com.mapbox.maps.plugin.locationcomponent.OnIndicatorPositionChangedListener;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.locationtracking.databinding.FragmentLocationTrackingBinding;

import android.annotation.SuppressLint;
import android.location.Location;
import android.os.Bundle;
import androidx.appcompat.app.AppCompatActivity;
import com.mapbox.geojson.Point;
import com.mapbox.maps.CameraOptions;
import com.mapbox.maps.EdgeInsets;




@AndroidEntryPoint
public class MapFragment extends Fragment {
    private FragmentLocationTrackingBinding binding;
    private MapView mapView;
    private MapboxMap map;
    LocationEngine locationEngine;



    private Style.OnStyleLoaded onStyleLoaded = style -> {
        setupGestureListener();
        initLocationComponent();
    };

    private OnMoveListener onMoveListener = new OnMoveListener() {
        @Override
        public void onMoveBegin(@NonNull MoveGestureDetector moveGestureDetector) {
            onCameraTrackingDismissed();
        }

        @Override
        public boolean onMove(@NonNull MoveGestureDetector moveGestureDetector) {
            return false;
        }

        @Override
        public void onMoveEnd(@NonNull MoveGestureDetector moveGestureDetector) {

        }
    };

    private void onCameraTrackingDismissed() {
        LocationComponentPlugin locationComponentPlugin = LocationComponentUtils.getLocationComponent(mapView);
        locationComponentPlugin.removeOnIndicatorBearingChangedListener(onIndicatorBearingChangedListener);
        locationComponentPlugin.removeOnIndicatorPositionChangedListener(onIndicatorPositionChangedListener);
        GesturesUtils.getGestures(mapView).removeOnMoveListener(onMoveListener);
    }

    private OnIndicatorBearingChangedListener onIndicatorBearingChangedListener = direction -> {
        mapView.getMapboxMap().setCamera(new CameraOptions.Builder().bearing(direction).build());
    };

    private OnIndicatorPositionChangedListener onIndicatorPositionChangedListener = positionPoint -> {
        mapView.getMapboxMap().setCamera(new CameraOptions.Builder().center(positionPoint).build());
        GesturesUtils.getGestures(mapView).setFocalPoint(mapView.getMapboxMap().pixelForCoordinate(positionPoint));
    };

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentLocationTrackingBinding.inflate(inflater);

        mapView = binding.getRoot().findViewById(R.id.mapView);

        requestLocationPermission();

        return binding.getRoot();
    }

    private void requestLocationPermission() {
        if (ContextCompat.checkSelfPermission(requireContext(), android.Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            mapView.getMapboxMap().loadStyleUri(Style.MAPBOX_STREETS, onStyleLoaded);
        } else {
            requestPermissionLauncher.launch(android.Manifest.permission.ACCESS_FINE_LOCATION);
        }
    }

    private final ActivityResultLauncher<String> requestPermissionLauncher =
            registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
                if (isGranted) {
                    Toast.makeText(requireContext(), "Location permission granted",Toast.LENGTH_SHORT)
                            .show();
                    mapView.getMapboxMap().loadStyleUri(Style.MAPBOX_STREETS, onStyleLoaded);
                } else {
                    showLocationPermissionDeniedDialog();
                }
            });

    private void showLocationPermissionDeniedDialog() {
        new MaterialAlertDialogBuilder(requireContext())
                .setTitle(R.string.fail_to_get_location_permission)
                .setMessage(R.string.retry)
                .setPositiveButton(R.string.yes, (dialogInterface, i) -> requestPermissionLauncher.launch(android.Manifest.permission.ACCESS_FINE_LOCATION))
                .setNegativeButton(R.string.no, ((dialogInterface, i) -> mapView.getMapboxMap().loadStyleUri(Style.MAPBOX_STREETS)))
                .show();
    }

    private void setupGestureListener() {
        GesturesUtils.getGestures(mapView).addOnMoveListener(onMoveListener);
    }



    private void initLocationComponent() {
        LocationComponentPlugin locationPlugin = LocationComponentUtils.getLocationComponent(mapView);
        locationPlugin.updateSettings(locationComponentSettings -> {
            Expression.ExpressionBuilder builder = new Expression.ExpressionBuilder("interpolate");
            builder.stop(expressionBuilder -> {
                expressionBuilder.literal(0.0);
                expressionBuilder.literal(0.6);
                return null;
            });
            builder.stop(expressionBuilder -> {
                expressionBuilder.literal(20.0);
                expressionBuilder.literal(1.0);
                return null;
            });
            builder.zoom();
            builder.interpolate(interpolatorBuilder -> {
                interpolatorBuilder.linear();
                return null;
            });

            locationComponentSettings.setEnabled(true);
            locationComponentSettings.setLocationPuck(new LocationPuck2D(null,
                    AppCompatResources.getDrawable(requireContext(), R.drawable.mapbox_user_puck_icon),
                    AppCompatResources.getDrawable(requireContext(), R.drawable.mapbox_user_icon_shadow)
                    ));
            return null;
        });

        locationPlugin.addOnIndicatorBearingChangedListener(onIndicatorBearingChangedListener);
        locationPlugin.addOnIndicatorPositionChangedListener(onIndicatorPositionChangedListener);
    }



    @Override
    public void onStart() {
        super.onStart();
        mapView.onStart();
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        mapView.onDestroy();
        onCameraTrackingDismissed();
    }

    @Override
    public void onStop() {
        super.onStop();
        mapView.onStop();
    }

    @Override
    public void onLowMemory() {
        super.onLowMemory();
        mapView.onLowMemory();
    }
}
