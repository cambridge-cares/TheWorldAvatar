package uk.ac.cam.cares.jps.timeline;

import android.Manifest;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.content.res.ColorStateList;
import android.content.res.Configuration;
import android.graphics.Color;
import android.graphics.RectF;
import android.graphics.drawable.ColorDrawable;
import android.location.Location;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageButton;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.fragment.NavHostFragment;

import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.google.android.material.floatingactionbutton.FloatingActionButton;
import com.mapbox.geojson.Point;
import com.mapbox.maps.CameraOptions;
import com.mapbox.maps.MapView;
import com.mapbox.maps.Style;
import com.mapbox.maps.plugin.Plugin;
import com.mapbox.maps.plugin.compass.CompassPlugin;
import com.mapbox.maps.plugin.scalebar.ScaleBarPlugin;

import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.sensor.permission.PermissionHelper;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.user.UserDialogFragment;
import uk.ac.cam.cares.jps.user.viewmodel.SensorViewModel;
import uk.ac.cam.cares.jps.timeline.ui.manager.BottomSheetManager;
import uk.ac.cam.cares.jps.timeline.ui.manager.TrajectoryManager;
import uk.ac.cam.cares.jps.timelinemap.R;
import uk.ac.cam.cares.jps.timelinemap.databinding.FragmentTimelineBinding;
import uk.ac.cam.cares.jps.ui.tooltip.TooltipManager;
import uk.ac.cam.cares.jps.ui.tooltip.TooltipManager.TooltipStyle;

@AndroidEntryPoint
public class TimelineFragment extends Fragment {

    private FragmentTimelineBinding binding;
    private MapView mapView;
    private final Logger LOGGER = Logger.getLogger(TimelineFragment.class);
    private final String TAG = "TooltipDebug";

    private final int MAP_BOTTOM_FLOATING_COMPONENT_MARGIN = 100;
    private BottomSheetBehavior<LinearLayoutCompat> bottomSheetBehavior;
    private ScaleBarPlugin scaleBarPlugin;
    private CompassPlugin compassPlugin;

    private static final String PREF_NAME = "app_preferences";
    private static final String PREF_LOCATION_PROMPTED = "location_permission_prompted";

    private FusedLocationProviderClient fusedLocationClient;
    private LocationCallback locationCallback;
    private ActivityResultLauncher<String> locationPermissionLauncher;

    private SensorViewModel sensorViewModel;
    private boolean isRecording = false;

    private PermissionHelper permissionHelper;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentTimelineBinding.inflate(inflater);
        setupMenu();
        setupBackPress();
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        mapView = binding.mapView;
        fusedLocationClient = LocationServices.getFusedLocationProviderClient(requireContext());

        sensorViewModel = new ViewModelProvider(requireActivity()).get(SensorViewModel.class);
        permissionHelper = new PermissionHelper(this);

        sensorViewModel.getHasAccountError().observe(getViewLifecycleOwner(), hasError -> {
            if (hasError != null && hasError) {
                Toast.makeText(requireContext(), "Please select at least one sensor before recording.", Toast.LENGTH_SHORT).show();
            }
        });

        registerPermissionLauncher();
        handleInitialLocationPrompt();

        updateUIForThemeMode(isDarkModeEnabled());

        TrajectoryManager trajectoryManager = new TrajectoryManager(this, mapView);
        BottomSheetManager bottomSheetManager = new BottomSheetManager(this, binding.bottomSheetContainer);

        compassPlugin = mapView.getPlugin(Plugin.MAPBOX_COMPASS_PLUGIN_ID);
        compassPlugin.setEnabled(true);
        compassPlugin.updateSettings(compassSettings -> {
            compassSettings.setMarginTop(400);
            return null;
        });

        scaleBarPlugin = mapView.getPlugin(Plugin.MAPBOX_SCALEBAR_PLUGIN_ID);

        setupRecordingButton();

        binding.getRoot().post(() -> {
            binding.bottomSheetContainer.post(() -> {
                if (isResumed()) {
                    Log.d(TAG, "Triggering tooltip after layout settled");
                    showIntroTooltips();
                } else {
                    Log.d(TAG, "Fragment not resumed. Tooltip aborted.");
                }
            });
        });
    }

    private void setupRecordingButton() {
        FloatingActionButton recordingFab = binding.recordingFab;

        recordingFab.setOnClickListener(v -> {
            Boolean isRecording = sensorViewModel.getIsRecording().getValue();

            if (isRecording != null && isRecording) {
                sensorViewModel.stopRecording();
                sensorViewModel.toggleAllSensors(false);
            } else {
                sensorViewModel.toggleAllSensors(true);

                List<SensorType> selectedSensors = sensorViewModel.getSelectedSensors().getValue();
                if (selectedSensors == null || selectedSensors.isEmpty()) {
                    Toast.makeText(requireContext(), "No sensors enabled. Cannot start recording.", Toast.LENGTH_SHORT).show();
                    return;
                }

                List<String> permissions = new ArrayList<>();
                if (selectedSensors.contains(SensorType.LOCATION) && needToPermissionGranted(Manifest.permission.ACCESS_FINE_LOCATION)) {
                    permissions.add(Manifest.permission.ACCESS_FINE_LOCATION);
                }
                if (selectedSensors.contains(SensorType.SOUND) && needToPermissionGranted(Manifest.permission.RECORD_AUDIO)) {
                    permissions.add(Manifest.permission.RECORD_AUDIO);
                }
                if (selectedSensors.contains(SensorType.ACTIVITY) && needToPermissionGranted(Manifest.permission.ACTIVITY_RECOGNITION)) {
                    permissions.add(Manifest.permission.ACTIVITY_RECOGNITION);
                }
                if (needToPermissionGranted(Manifest.permission.POST_NOTIFICATIONS)) {
                    permissions.add(Manifest.permission.POST_NOTIFICATIONS);
                }

                permissionHelper.requestPermissionsInChain(permissions, () -> sensorViewModel.startRecording());
            }
        });

        sensorViewModel.getIsRecording().observe(getViewLifecycleOwner(), isRecording -> {
            if (isRecording != null) {
                int icon = isRecording ? R.drawable.ic_stop : R.drawable.ic_start;
                recordingFab.setImageResource(icon);

                int backgroundColor = isRecording
                        ? ContextCompat.getColor(requireContext(), R.color.fab_recording)
                        : ContextCompat.getColor(requireContext(), R.color.fab_idle);
                recordingFab.setBackgroundTintList(ColorStateList.valueOf(backgroundColor));
            }
        });
    }

    private boolean needToPermissionGranted(String permission) {
        return ContextCompat.checkSelfPermission(requireContext(), permission) != PackageManager.PERMISSION_GRANTED;
    }

    private void registerPermissionLauncher() {
        locationPermissionLauncher = registerForActivityResult(
                new ActivityResultContracts.RequestPermission(),
                isGranted -> {
                    if (isGranted) {
                        getAndCenterUserLocation();
                    } else {
                        setCameraToDefaultLocation();
                        Toast.makeText(requireContext(), "Location permission denied. Showing default view.", Toast.LENGTH_SHORT).show();
                    }
                }
        );
    }

    private void handleInitialLocationPrompt() {
        SharedPreferences prefs = requireContext().getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);
        boolean wasPrompted = prefs.getBoolean(PREF_LOCATION_PROMPTED, false);

        if (!wasPrompted) {
            prefs.edit().putBoolean(PREF_LOCATION_PROMPTED, true).apply();
            locationPermissionLauncher.launch(Manifest.permission.ACCESS_FINE_LOCATION);
        } else {
            if (ContextCompat.checkSelfPermission(requireContext(), Manifest.permission.ACCESS_FINE_LOCATION)
                    == PackageManager.PERMISSION_GRANTED) {
                getAndCenterUserLocation();
            } else {
                setCameraToDefaultLocation();
            }
        }
    }

    private void getAndCenterUserLocation() {
        LocationRequest locationRequest = LocationRequest.create()
                .setPriority(LocationRequest.PRIORITY_HIGH_ACCURACY)
                .setInterval(1000)
                .setNumUpdates(1);

        locationCallback = new LocationCallback() {
            @Override
            public void onLocationResult(@NonNull LocationResult locationResult) {
                Location location = locationResult.getLastLocation();
                if (location != null) {
                    CameraOptions cameraOptions = new CameraOptions.Builder()
                            .center(Point.fromLngLat(location.getLongitude(), location.getLatitude()))
                            .zoom(15.0)
                            .build();
                    mapView.getMapboxMap().setCamera(cameraOptions);
                    Log.d("LOCATION", "Fresh Coordinates: " + location.getLatitude() + ", " + location.getLongitude());
                } else {
                    Log.d("LOCATION", "Location is null. Using default location.");
                    setCameraToDefaultLocation();
                }
            }
        };

        fusedLocationClient.requestLocationUpdates(locationRequest, locationCallback, Looper.getMainLooper());
    }

    private void setCameraToDefaultLocation() {
        double defaultLat = 1.2966;
        double defaultLng = 103.7764;

        CameraOptions defaultCamera = new CameraOptions.Builder()
                .center(Point.fromLngLat(defaultLng, defaultLat))
                .zoom(13.0)
                .build();
        mapView.getMapboxMap().setCamera(defaultCamera);

        Point camCenter = mapView.getMapboxMap().getCameraState().getCenter();
        Log.d("MAPBOX_CAMERA", "Camera set to DEFAULT → Lat: " + camCenter.latitude() + ", Lng: " + camCenter.longitude());
    }

    private void showIntroTooltips() {
        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            TooltipManager tooltipManager = new TooltipManager(requireActivity(), () -> Log.d(TAG, "Tooltips finished"));

            View firstTarget = binding.mapTopAppbar.findViewById(R.id.user_menu_item);
            View secondTarget = binding.recordingFab;

            tooltipManager.addStep(firstTarget,
                    "User Menu",
                    "Tap here to access your account settings and sensor preferences.",
                    TooltipStyle.UP);

            tooltipManager.addStep(secondTarget,
                    "Quick Start Recording",
                    "Instantly toggle all sensors. Customize your selection in Sensor Settings.",
                    TooltipStyle.UP);

            tooltipManager.start();
        }, 500);
    }

    private void setupMenu() {
        binding.mapTopAppbar.setNavigationOnClickListener(view ->
                NavHostFragment.findNavController(this).navigateUp()
        );

        binding.mapTopAppbar.setOnMenuItemClickListener(menuItem -> {
            if (menuItem.getItemId() == R.id.user_menu_item) {
                UserDialogFragment.show(getParentFragmentManager());
                return true;
            }
            return false;
        });
    }

    private void setupBackPress() {
        requireActivity().getOnBackPressedDispatcher().addCallback(getViewLifecycleOwner(), new OnBackPressedCallback(true) {
            private boolean doubleBackToExitPressedOnce;

            @Override
            public void handleOnBackPressed() {
                if (doubleBackToExitPressedOnce) {
                    requireActivity().finishAffinity();
                    return;
                }

                this.doubleBackToExitPressedOnce = true;
                Toast.makeText(requireContext(), "Please click BACK again to exit", Toast.LENGTH_SHORT).show();

                new Handler(Looper.getMainLooper()).postDelayed(() -> doubleBackToExitPressedOnce = false, 2000);
            }
        });
    }

    private boolean isDarkModeEnabled() {
        int nightModeFlags = getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
        return nightModeFlags == Configuration.UI_MODE_NIGHT_YES;
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        if (newConfig.uiMode != getResources().getConfiguration().uiMode) {
            updateUIForThemeMode(isDarkModeEnabled());
        }
    }

    private void updateUIForThemeMode(boolean isDarkMode) {
        if (isDarkMode) {
            mapView.getMapboxMap().loadStyleUri(Style.DARK);
        } else {
            mapView.getMapboxMap().loadStyleUri(Style.LIGHT);
        }
    }
}
