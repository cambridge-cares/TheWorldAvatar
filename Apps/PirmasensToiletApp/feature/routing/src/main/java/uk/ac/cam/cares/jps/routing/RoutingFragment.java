package uk.ac.cam.cares.jps.routing;

import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.mapbox.maps.MapView;
import com.mapbox.maps.Style;

import org.apache.log4j.Logger;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.routing.bottomsheet.ToiletBottomSheet;
import uk.ac.cam.cares.jps.routing.databinding.FragmentMapBinding;
import uk.ac.cam.cares.jps.routing.ui.manager.LocationPuckManager;
import uk.ac.cam.cares.jps.routing.ui.manager.UserLocationManager;
import uk.ac.cam.cares.jps.routing.ui.manager.RouteManager;
import uk.ac.cam.cares.jps.routing.ui.manager.ToiletMarkerManager;


@AndroidEntryPoint
public class RoutingFragment extends Fragment {
    private Logger LOGGER = Logger.getLogger( RoutingFragment.class);

    private FragmentMapBinding binding;
    private MapView mapView;
    private ImageView imageView;

    // UI manager
    private ToiletMarkerManager toiletMarkerManager;
    private RouteManager routeManager;
    private UserLocationManager userLocationManager;
    private LocationPuckManager locationPuckManager;


    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentMapBinding.inflate(inflater);
        imageView = binding.getRoot().findViewById(R.id.imageView);
        imageView.setVisibility(View.INVISIBLE);

        mapView = binding.getRoot().findViewById(R.id.mapView);
        mapView.getMapboxMap().loadStyleUri(Style.MAPBOX_STREETS);

        checkLocationPermission();

        ToiletBottomSheet toiletBottomSheet = new ToiletBottomSheet(this, binding.bottomSheet.getRoot().findViewById(R.id.bottom_sheet_linear_layout));

        // init UI manager
        userLocationManager = new UserLocationManager(this);
        routeManager = new RouteManager(mapView, this);
        toiletMarkerManager = new ToiletMarkerManager(mapView, this, toiletBottomSheet);
        locationPuckManager = new LocationPuckManager(mapView, this);

        return binding.getRoot();
    }


    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        toiletMarkerManager.getToiletsData();
    }

    private void checkLocationPermission() {
        if (ContextCompat.checkSelfPermission(requireContext(), android.Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
            mapView.getMapboxMap().addOnStyleLoadedListener(style -> {
                locationPuckManager.enableLocationTracking();
                userLocationManager.requestLastLocation();
                userLocationManager.startLocationUpdates();
            });
        } else {
            requestPermissionLauncher.launch(android.Manifest.permission.ACCESS_FINE_LOCATION);
        }
    }

    private final ActivityResultLauncher<String> requestPermissionLauncher =
            registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted -> {
                if (isGranted) {
                    Toast.makeText(requireContext(), "Location permission granted",Toast.LENGTH_SHORT)
                            .show();
                    mapView.getMapboxMap().addOnStyleLoadedListener(style -> {
                        locationPuckManager.enableLocationTracking();
                        userLocationManager.requestLastLocation();
                        userLocationManager.startLocationUpdates();
                    });
                } else {
                    showLocationPermissionDeniedDialog();
                }
            });

    public void showLocationPermissionDeniedDialog() {
        new MaterialAlertDialogBuilder(requireContext())
                .setTitle(R.string.location_permission_not_granted)
                .setMessage(R.string.please_allow_location_permission)
                .setNeutralButton(R.string.ok, ((dialogInterface, i) -> {}))
                .show();
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
