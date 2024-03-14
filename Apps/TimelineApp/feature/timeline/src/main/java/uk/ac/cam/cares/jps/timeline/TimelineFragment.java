package uk.ac.cam.cares.jps.timeline;

import android.os.Bundle;
import android.util.DisplayMetrics;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.mapbox.bindgen.Expected;
import com.mapbox.bindgen.None;
import com.mapbox.bindgen.Value;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.maps.LayerPosition;
import com.mapbox.maps.MapView;
import com.mapbox.maps.Style;
import com.mapbox.maps.extension.style.sources.generated.GeoJsonSource;
import com.mapbox.maps.plugin.Plugin;
import com.mapbox.maps.plugin.compass.CompassPlugin;
import com.mapbox.maps.plugin.scalebar.ScaleBarPlugin;
import com.squareup.picasso.Picasso;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Objects;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.timeline.databinding.FragmentTimelineBinding;

@AndroidEntryPoint
public class TimelineFragment extends Fragment {
    private FragmentTimelineBinding binding;
    private TrajectoryViewModel trajectoryViewModel;
    private MapView mapView;
    private Logger LOGGER = Logger.getLogger(TimelineFragment.class);

    private final int MAP_BOTTOM_FLOATING_COMPONENT_MARGIN = 100;
    private BottomSheetBehavior<LinearLayoutCompat> bottomSheetBehavior;
    private ScaleBarPlugin scaleBarPlugin;
    private CompassPlugin compassPlugin;

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentTimelineBinding.inflate(inflater);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        trajectoryViewModel = new ViewModelProvider(this).get(TrajectoryViewModel.class);
        bottomSheetBehavior = BottomSheetBehavior.from(binding.bottomSheetWidget.bottomSheet);

        mapView = binding.mapView;
        mapView.getMapboxMap().addOnStyleLoadedListener(style -> {

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

        trajectoryViewModel.trajectory.observe(getViewLifecycleOwner(), trajectory -> {
            mapView.getMapboxMap().getStyle(style -> {
                Expected<String, None> removeLayerSuccess = style.removeStyleLayer("trajectory_layer");
                Expected<String, None> removeSourceSuccess = style.removeStyleSource("trajectory");
                LOGGER.debug("trajectory: layer and source removed result " + (removeSourceSuccess.isError() && removeLayerSuccess.isError() ? removeSourceSuccess.getError() + "\n" + removeLayerSuccess.getError() : "success"));

                JSONObject sourceJson = new JSONObject();
                try {
                    sourceJson.put("type", "geojson");
                    sourceJson.put("data", trajectory);
                } catch (JSONException e) {
                    throw new RuntimeException(e);
                }
                Expected<String, None> success = style.addStyleSource("trajectory", Objects.requireNonNull(Value.fromJson(sourceJson.toString()).getValue()));
                LOGGER.debug("trajectory: source created " + (success.isError() ? success.getError() : "success"));

                JSONObject layerJson = new JSONObject();
                try {
                    layerJson.put("id", "trajectory_layer");
                    layerJson.put("type", "line");
                    layerJson.put("source", "trajectory");
                    layerJson.put("line-join", "bevel");

                    JSONObject paint = new JSONObject();
                    paint.put("line-color", "#00687C");
                    paint.put("line-width", 8);
                    layerJson.put("paint", paint);
                } catch (JSONException e) {
                    throw new RuntimeException(e);
                }
                Expected<String, None> layerSuccess = style.addStyleLayer(Objects.requireNonNull(Value.fromJson(layerJson.toString()).getValue()), new LayerPosition(null, null, null));
                LOGGER.debug("trajectory: layer created " + (layerSuccess.isError() ? layerSuccess.getError() : "success"));
            });
        });
        trajectoryViewModel.getTrajectory();

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
//                    binding.appBarLayout.setBackground(R.attr.colorPrimary);
                } else {

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

}
