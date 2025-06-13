package uk.ac.cam.cares.jps.timeline;

import android.Manifest;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.graphics.RectF;
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
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.mapbox.geojson.Point;
import com.mapbox.maps.CameraOptions;
import com.mapbox.maps.MapView;
import com.mapbox.maps.Style;
import com.mapbox.maps.plugin.Plugin;
import com.mapbox.maps.plugin.compass.CompassPlugin;
import com.mapbox.maps.plugin.scalebar.ScaleBarPlugin;

import org.apache.log4j.Logger;

import dagger.hilt.android.AndroidEntryPoint;
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

    private static final int LOCATION_PERMISSION_REQUEST_CODE = 1001;
    private FusedLocationProviderClient fusedLocationClient;
    private LocationCallback locationCallback;

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
                    Log.d("LOCATION", "Fresh LatitudeLangitude: " + location.getLatitude() + ", " + location.getLongitude());
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
        Log.d(TAG, "Preparing tooltip manager...");
        TooltipManager tooltipManager = new TooltipManager(requireActivity(), () -> {
            Log.d(TAG, "Tooltip sequence completed.");
        });

        DisplayMetrics dm = getResources().getDisplayMetrics();
        float density = dm.density;
        int screenWidth = dm.widthPixels;

        bottomSheetBehavior = BottomSheetBehavior.from(binding.bottomSheetContainer);

        int[] location = new int[2];
        binding.bottomSheetContainer.getLocationOnScreen(location);
        int bottomSheetTopY = location[1];

        float width1 = 300 * density;
        float height1 = 100 * density;
        float x1 = (screenWidth - width1) / 2f;
        float y1 = bottomSheetTopY - height1 - (24 * density);

        RectF tooltip1 = new RectF(x1, y1, x1 + width1, y1 + height1);
        Log.d(TAG, "Tooltip 1 rect (above bottom sheet): " + tooltip1);

        tooltipManager.addStep(
                tooltip1,
                "Track Your Journey",
                "View your past movements and trajectory summary here.",
                TooltipStyle.UP
        );

        float width2 = 48 * density;
        float height2 = 48 * density;
        float marginTop = 48 * density;
        float marginEnd = 16 * density;
        float x2 = screenWidth - marginEnd - width2;
        float y2 = marginTop;

        RectF tooltip2 = new RectF(x2, y2, x2 + width2, y2 + height2);
        Log.d(TAG, "Tooltip 2 rect: " + tooltip2);

        tooltipManager.addStep(
                tooltip2,
                "Manage Your Account",
                "Tap the icon to access settings, export data, or update your profile.",
                TooltipStyle.DOWN
        );

        tooltipManager.start();
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
}
