package uk.ac.cam.cares.jps.timeline;

import android.Manifest;
import android.content.pm.PackageManager;
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
import android.widget.PopupWindow;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavDeepLinkRequest;
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

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.user.SensorSettingFragment;
import uk.ac.cam.cares.jps.user.viewmodel.SensorViewModel;
import uk.ac.cam.cares.jps.timeline.ui.manager.BottomSheetManager;
import uk.ac.cam.cares.jps.timeline.ui.manager.TrajectoryManager;
import uk.ac.cam.cares.jps.timelinemap.R;
import uk.ac.cam.cares.jps.timelinemap.databinding.FragmentTimelineBinding;
import uk.ac.cam.cares.jps.ui.tooltip.TooltipManager;
import uk.ac.cam.cares.jps.ui.tooltip.TooltipManager.TooltipStyle;
import uk.ac.cam.cares.jps.ui.UiUtils;


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

    private static final int LOCATION_PERMISSION_REQUEST_CODE = 1001;
    private FusedLocationProviderClient fusedLocationClient;
    private LocationCallback locationCallback;

    private SensorViewModel sensorViewModel;
    private boolean isRecording = false;

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
        mapView.getMapboxMap().addOnStyleLoadedListener(style -> {});
        fusedLocationClient = LocationServices.getFusedLocationProviderClient(requireContext());
        sensorViewModel = new ViewModelProvider(requireActivity()).get(SensorViewModel.class);
        binding.userDropdownButton.setOnClickListener(v -> showUserDropdown(v));


        sensorViewModel.getHasAccountError().observe(getViewLifecycleOwner(), hasError -> {
            if (hasError != null && hasError) {
                Toast.makeText(requireContext(), "Please select at least one sensor before recording.", Toast.LENGTH_SHORT).show();
            }
        });

        getAndCenterUserLocation();
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

            if (sensorViewModel.getSelectedSensors().getValue() == null || sensorViewModel.getSelectedSensors().getValue().isEmpty()) {
                Toast.makeText(requireContext(), "Please enable at least one sensor or click Toggle All in the sensor settings", Toast.LENGTH_SHORT).show();
            } else {
                sensorViewModel.toggleRecording();
            }
        });

        sensorViewModel.getIsRecording().observe(getViewLifecycleOwner(), isRecording -> {
            if (isRecording != null) {
                int icon = isRecording ? R.drawable.ic_stop : R.drawable.ic_start;
                recordingFab.setImageResource(icon);
            }
        });
    }

    private void getAndCenterUserLocation() {
        if (ContextCompat.checkSelfPermission(requireContext(), Manifest.permission.ACCESS_FINE_LOCATION)
                != PackageManager.PERMISSION_GRANTED) {
            Log.d("LOCATION", "Permission not granted. Using default location.");
            setCameraToDefaultLocation();
            return;
        }

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

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions,
                                           @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == LOCATION_PERMISSION_REQUEST_CODE &&
                grantResults.length > 0 &&
                grantResults[0] == PackageManager.PERMISSION_GRANTED) {
            getAndCenterUserLocation();
        } else {
            Toast.makeText(requireContext(), "Location permission denied", Toast.LENGTH_SHORT).show();
        }
    }

    private void showIntroTooltips() {
        new Handler(Looper.getMainLooper()).postDelayed(() -> {
            TooltipManager tooltipManager = new TooltipManager(requireActivity(), () -> Log.d(TAG, "Tooltips finished"));

            View firstTarget = binding.userDropdownButton;;
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
        binding.mapTopAppbar.setNavigationOnClickListener(view -> NavHostFragment.findNavController(this).navigateUp());

        binding.mapTopAppbar.setOnMenuItemClickListener(menuItem -> {
            if (menuItem.getItemId() == R.id.user_menu_item) {
                NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                        .fromUri(Uri.parse(getString(uk.ac.cam.cares.jps.utils.R.string.user_fragment_link)))
                        .build();
                NavHostFragment.findNavController(this).navigate(request);
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

    private void showUserDropdown(View anchor) {
        View popupView = LayoutInflater.from(requireContext()).inflate(R.layout.view_user_dropdown_menu, null);
        PopupWindow popupWindow = new PopupWindow(
                popupView,
                ViewGroup.LayoutParams.WRAP_CONTENT,
                ViewGroup.LayoutParams.WRAP_CONTENT,
                true
        );

        popupWindow.setElevation(10f);
        popupWindow.setOutsideTouchable(true);
        popupWindow.setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));

        // Link dropdown items
        popupView.findViewById(R.id.menu_account).setOnClickListener(v -> {
            popupWindow.dismiss();
            navigateTo("https://jps.cam.cares/account");
        });

        popupView.findViewById(R.id.menu_sensor).setOnClickListener(v -> {
            popupWindow.dismiss();
            navigateTo("https://jps.cam.cares/sensor");
        });

        popupView.findViewById(R.id.menu_timeline).setOnClickListener(v -> {
            popupWindow.dismiss();
            showNotImplemented();
        });

        popupView.findViewById(R.id.menu_help).setOnClickListener(v -> {
            popupWindow.dismiss();
            navigateTo("https://jps.cam.cares/help");
        });

        popupView.findViewById(R.id.menu_privacy).setOnClickListener(v -> {
            popupWindow.dismiss();
            navigateTo("https://jps.cam.cares/privacy");
        });

        popupView.findViewById(R.id.menu_logout).setOnClickListener(v -> {
            popupWindow.dismiss();
            // TODO: Add logout logic here
            showNotImplemented();
        });

        popupWindow.showAsDropDown(anchor, -100, 20);
    }

    private void navigateTo(String uriString) {
        NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                .fromUri(Uri.parse(uriString))
                .build();
        NavHostFragment.findNavController(this).navigate(request);
    }


    private void showNotImplemented() {
        UiUtils.showNotImplementedDialog(requireContext());
    }

}
