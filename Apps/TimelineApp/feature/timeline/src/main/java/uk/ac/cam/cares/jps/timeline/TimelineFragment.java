package uk.ac.cam.cares.jps.timeline;

import android.net.ConnectivityManager;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.fragment.app.Fragment;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.mapbox.maps.MapView;
import com.mapbox.maps.Style;
import com.mapbox.maps.plugin.Plugin;
import com.mapbox.maps.plugin.compass.CompassPlugin;
import com.mapbox.maps.plugin.scalebar.ScaleBarPlugin;

import org.apache.log4j.Logger;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.timeline.ui.bottomsheet.BottomSheet;
import uk.ac.cam.cares.jps.timeline.ui.bottomsheet.NormalBottomSheet;
import uk.ac.cam.cares.jps.timeline.ui.manager.TrajectoryManager;
import uk.ac.cam.cares.jps.timelinemap.R;
import uk.ac.cam.cares.jps.timelinemap.databinding.FragmentTimelineBinding;

@AndroidEntryPoint
public class TimelineFragment extends Fragment {
    private FragmentTimelineBinding binding;
    private MapView mapView;
    private BottomSheet bottomSheet;
    private Logger LOGGER = Logger.getLogger(TimelineFragment.class);
    private ConnectivityManager connectivityManager;

    private final int MAP_BOTTOM_FLOATING_COMPONENT_MARGIN = 100;
    private BottomSheetBehavior<LinearLayoutCompat> bottomSheetBehavior;
    private ScaleBarPlugin scaleBarPlugin;
    private CompassPlugin compassPlugin;


    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        connectivityManager = requireContext().getSystemService(ConnectivityManager.class);

        binding = FragmentTimelineBinding.inflate(inflater);
        setupMenu();
        setupBackPress();

        bottomSheet = new NormalBottomSheet(requireContext(), binding.getRoot());
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        bottomSheetBehavior = (BottomSheetBehavior<LinearLayoutCompat>) bottomSheet.getBehavior();
//        bottomSheetBehavior = BottomSheetBehavior.from(binding.bottomSheetWidget.bottomSheet);

        mapView = binding.mapView;
        mapView.getMapboxMap().addOnStyleLoadedListener(style -> {
            // todo
        });
        mapView.getMapboxMap().loadStyleUri(Style.LIGHT);

        compassPlugin = mapView.getPlugin(Plugin.MAPBOX_COMPASS_PLUGIN_ID);
        compassPlugin.setEnabled(true);
        compassPlugin.updateSettings(compassSettings -> {
            compassSettings.setMarginTop(400);
            return null;
        });

        scaleBarPlugin = mapView.getPlugin(Plugin.MAPBOX_SCALEBAR_PLUGIN_ID);

        setupBottomSheet();

        TrajectoryManager trajectoryManager = new TrajectoryManager(this, mapView);
        trajectoryManager.getTrajectory();

    }

    private void setupBottomSheet() {
        bottomSheetBehavior.addBottomSheetCallback(new BottomSheetBehavior.BottomSheetCallback() {
            @Override
            public void onStateChanged(@NonNull View view, int i) {
                LOGGER.info("State is " + i);
                scaleBarPlugin.updateSettings(scaleBarSettings -> {
                    scaleBarSettings.setMarginTop(view.getTop() - MAP_BOTTOM_FLOATING_COMPONENT_MARGIN + binding.appBarLayout.getHeight());
                    return null;
                });

                if (i == BottomSheetBehavior.STATE_EXPANDED) {
                    // todo: change the app bar style
                    TypedValue typedValue = new TypedValue();
                    requireContext().getTheme().resolveAttribute(androidx.appcompat.R.attr.colorPrimary, typedValue, true);

                    binding.appBarLayout.setBackground(getResources().getDrawable(typedValue.resourceId, requireContext().getTheme()));
                } else {
                    binding.appBarLayout.setBackground(getResources().getDrawable(R.drawable.app_bar_background, requireContext().getTheme()));
                }
            }

            @Override
            public void onSlide(@NonNull View view, float v) {
                scaleBarPlugin.updateSettings(scaleBarSettings -> {
                    scaleBarSettings.setMarginTop(view.getTop() - MAP_BOTTOM_FLOATING_COMPONENT_MARGIN + binding.appBarLayout.getHeight());
                    return null;
                });
            }
        });
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

                new Handler(Looper.getMainLooper()).postDelayed(() -> doubleBackToExitPressedOnce=false, 2000);
            }
        });
    }
}
