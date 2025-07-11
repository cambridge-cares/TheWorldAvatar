package uk.ac.cam.cares.jps.timeline;

import android.Manifest;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.content.res.ColorStateList;
import android.content.res.Configuration;
import android.location.Location;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver;
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
import uk.ac.cam.cares.jps.ui.tooltip.TooltipSequence;
import uk.ac.cam.cares.jps.user.UserDialogFragment;
import uk.ac.cam.cares.jps.user.viewmodel.SensorViewModel;
import uk.ac.cam.cares.jps.user.viewmodel.TooltipTriggerViewModel;
import uk.ac.cam.cares.jps.timeline.ui.manager.BottomSheetManager;
import uk.ac.cam.cares.jps.timeline.ui.manager.TrajectoryManager;
import uk.ac.cam.cares.jps.timelinemap.R;
import uk.ac.cam.cares.jps.timelinemap.databinding.FragmentTimelineBinding;

@AndroidEntryPoint
public class TimelineFragment extends Fragment {

    private FragmentTimelineBinding binding;
    private final Logger LOGGER = Logger.getLogger(TimelineFragment.class);
    private final String TAG = "TooltipDebug";

    private MapView mapView;
    private BottomSheetBehavior<LinearLayoutCompat> bottomSheetBehavior;
    private ScaleBarPlugin scaleBarPlugin;
    private CompassPlugin compassPlugin;

    private FusedLocationProviderClient fusedLocationClient;
    private LocationCallback locationCallback;
    private ActivityResultLauncher<String> locationPermissionLauncher;

    private SensorViewModel sensorViewModel;
    private TooltipTriggerViewModel tooltipTriggerViewModel;
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
        tooltipTriggerViewModel = new ViewModelProvider(requireActivity()).get(TooltipTriggerViewModel.class);
        if (tooltipTriggerViewModel.isTriggerPending()) {
            tooltipTriggerViewModel.clearTrigger();
            showIntroTooltips();
        }

        permissionHelper = new PermissionHelper(this);

        handleTooltipTrigger();

        setupRecordingButton();
        registerPermissionLauncher();
        handleInitialLocationPrompt();
        updateUIForThemeMode(isDarkModeEnabled());

        new TrajectoryManager(this, mapView);
        new BottomSheetManager(this, binding.bottomSheetContainer);

        compassPlugin = mapView.getPlugin(Plugin.MAPBOX_COMPASS_PLUGIN_ID);
        if (compassPlugin != null) {
            compassPlugin.setEnabled(true);
            compassPlugin.updateSettings(settings -> {
                settings.setMarginTop(400);
                return null;
            });
        }

        scaleBarPlugin = mapView.getPlugin(Plugin.MAPBOX_SCALEBAR_PLUGIN_ID);

        SharedPreferences prefs = requireContext().getSharedPreferences("TimelinePrefs", Context.MODE_PRIVATE);
        if (prefs.getBoolean("autostart_enabled", false)) {
            autoStartRecordingIfPossible();
        }

        sensorViewModel.getHasAccountError().observe(getViewLifecycleOwner(), hasError -> {
            if (Boolean.TRUE.equals(hasError)) {
                Toast.makeText(requireContext(), "Please select at least one sensor before recording.", Toast.LENGTH_SHORT).show();
            }
        });
    }

    private void handleTooltipTrigger() {
        tooltipTriggerViewModel.getShouldTriggerTooltips().observe(getViewLifecycleOwner(), shouldTrigger -> {
            if (Boolean.TRUE.equals(shouldTrigger)) {
                tooltipTriggerViewModel.clearTrigger();
                showIntroTooltips();
            }
        });
    }

    private void showIntroTooltips() {
        View userIcon = binding.mapTopAppbar.findViewById(R.id.user_menu_item);
        View recordingFab = binding.recordingFab;
        View bottomSheet = binding.bottomSheetContainer;

        ViewTreeObserver observer = bottomSheet.getViewTreeObserver();
        observer.addOnGlobalLayoutListener(new ViewTreeObserver.OnGlobalLayoutListener() {
            @Override
            public void onGlobalLayout() {
                if (userIcon.isShown() && recordingFab.isShown() && bottomSheet.isShown()) {
                    bottomSheet.getViewTreeObserver().removeOnGlobalLayoutListener(this);
                    TooltipSequence.launch(requireActivity(), userIcon, recordingFab, bottomSheet, userIcon);
                }
            }
        });
    }


    private void setupRecordingButton() {
        FloatingActionButton fab = binding.recordingFab;
        sensorViewModel.getIsRecording().observe(getViewLifecycleOwner(), isRecording -> {
            if (isRecording != null) {
                fab.setImageResource(isRecording ? R.drawable.ic_stop : R.drawable.ic_start);
                int bgColor = ContextCompat.getColor(requireContext(), isRecording ? R.color.fab_recording : R.color.fab_idle);
                fab.setBackgroundTintList(ColorStateList.valueOf(bgColor));
            }
        });

        fab.setOnClickListener(v -> {
            Boolean isRecording = sensorViewModel.getIsRecording().getValue();
            if (Boolean.TRUE.equals(isRecording)) {
                sensorViewModel.stopRecording();
            } else {
                sensorViewModel.toggleAllSensors(true);
                List<SensorType> sensors = sensorViewModel.getSelectedSensors().getValue();
                if (sensors == null || sensors.isEmpty()) {
                    Toast.makeText(requireContext(), "No sensors enabled. Cannot start recording.", Toast.LENGTH_SHORT).show();
                    return;
                }

                List<String> permissions = new ArrayList<>();
                if (sensors.contains(SensorType.LOCATION) && needToPermissionGranted(Manifest.permission.ACCESS_FINE_LOCATION))
                    permissions.add(Manifest.permission.ACCESS_FINE_LOCATION);
                if (sensors.contains(SensorType.SOUND) && needToPermissionGranted(Manifest.permission.RECORD_AUDIO))
                    permissions.add(Manifest.permission.RECORD_AUDIO);
                if (sensors.contains(SensorType.ACTIVITY) && needToPermissionGranted(Manifest.permission.ACTIVITY_RECOGNITION))
                    permissions.add(Manifest.permission.ACTIVITY_RECOGNITION);
                if (needToPermissionGranted(Manifest.permission.POST_NOTIFICATIONS))
                    permissions.add(Manifest.permission.POST_NOTIFICATIONS);

                permissionHelper.requestPermissionsInChain(permissions, sensorViewModel::startRecording);
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
                });
    }

    private void handleInitialLocationPrompt() {
        SharedPreferences prefs = requireContext().getSharedPreferences("app_preferences", Context.MODE_PRIVATE);
        boolean wasPrompted = prefs.getBoolean("location_permission_prompted", false);

        if (!wasPrompted) {
            prefs.edit().putBoolean("location_permission_prompted", true).apply();
            locationPermissionLauncher.launch(Manifest.permission.ACCESS_FINE_LOCATION);
        } else if (ContextCompat.checkSelfPermission(requireContext(), Manifest.permission.ACCESS_FINE_LOCATION)
                == PackageManager.PERMISSION_GRANTED) {
            getAndCenterUserLocation();
        } else {
            setCameraToDefaultLocation();
        }
    }

    private void getAndCenterUserLocation() {
        LocationRequest locationRequest = LocationRequest.create().setPriority(LocationRequest.PRIORITY_HIGH_ACCURACY).setInterval(1000).setNumUpdates(1);
        locationCallback = new LocationCallback() {
            @Override
            public void onLocationResult(@NonNull LocationResult result) {
                Location loc = result.getLastLocation();
                if (loc != null) {
                    mapView.getMapboxMap().setCamera(
                            new CameraOptions.Builder()
                                    .center(Point.fromLngLat(loc.getLongitude(), loc.getLatitude()))
                                    .zoom(15.0).build());
                } else {
                    setCameraToDefaultLocation();
                }
            }
        };
        fusedLocationClient.requestLocationUpdates(locationRequest, locationCallback, Looper.getMainLooper());
    }

    private void setCameraToDefaultLocation() {
        CameraOptions defaultCam = new CameraOptions.Builder()
                .center(Point.fromLngLat(103.7764, 1.2966)).zoom(13.0).build();
        mapView.getMapboxMap().setCamera(defaultCam);
    }

    private void setupMenu() {
        binding.mapTopAppbar.setNavigationOnClickListener(view ->
                NavHostFragment.findNavController(this).navigateUp());

        binding.mapTopAppbar.setOnMenuItemClickListener(item -> {
            if (item.getItemId() == R.id.user_menu_item) {
                UserDialogFragment.show(getParentFragmentManager());
                return true;
            }
            return false;
        });
    }

    private void setupBackPress() {
        requireActivity().getOnBackPressedDispatcher().addCallback(getViewLifecycleOwner(), new OnBackPressedCallback(true) {
            private boolean doubleBackOnce;

            @Override
            public void handleOnBackPressed() {
                if (doubleBackOnce) {
                    requireActivity().finishAffinity();
                } else {
                    doubleBackOnce = true;
                    Toast.makeText(requireContext(), "Please click BACK again to exit", Toast.LENGTH_SHORT).show();
                    new Handler(Looper.getMainLooper()).postDelayed(() -> doubleBackOnce = false, 2000);
                }
            }
        });
    }

    private void updateUIForThemeMode(boolean isDark) {
        mapView.getMapboxMap().loadStyleUri(isDark ? Style.DARK : Style.LIGHT);
    }

    private boolean isDarkModeEnabled() {
        return (getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK)
                == Configuration.UI_MODE_NIGHT_YES;
    }

    private void autoStartRecordingIfPossible() {
        sensorViewModel.toggleAllSensors(true);
        sensorViewModel.startRecording();
        Toast.makeText(requireContext(), "Auto-start on", Toast.LENGTH_SHORT).show();
    }
}
