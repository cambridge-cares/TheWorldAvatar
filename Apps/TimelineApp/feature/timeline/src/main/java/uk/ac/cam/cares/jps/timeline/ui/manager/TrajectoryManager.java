package uk.ac.cam.cares.jps.timeline.ui.manager;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;

import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.mapbox.bindgen.Expected;
import com.mapbox.bindgen.None;
import com.mapbox.bindgen.Value;
import com.mapbox.geojson.Point;
import com.mapbox.maps.CameraOptions;
import com.mapbox.maps.LayerPosition;
import com.mapbox.maps.MapView;
import com.mapbox.maps.Style;
import com.mapbox.maps.plugin.animation.MapAnimationOptions;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Objects;

import uk.ac.cam.cares.jps.timeline.viewmodel.TrajectoryViewModel;

public class TrajectoryManager {
    private TrajectoryViewModel trajectoryViewModel;
    private Logger LOGGER = Logger.getLogger(TrajectoryManager.class);

    public TrajectoryManager(Fragment fragment, MapView mapView) {
        trajectoryViewModel = new ViewModelProvider(fragment).get(TrajectoryViewModel.class);

        trajectoryViewModel.trajectory.observe(fragment.getViewLifecycleOwner(), trajectoryStr -> {
            mapView.getMapboxMap().getStyle(style -> {
                Expected<String, None> removeLayerSuccess = style.removeStyleLayer("trajectory_layer");
                Expected<String, None> removeSourceSuccess = style.removeStyleSource("trajectory");
                LOGGER.debug("trajectory: layer and source removed result " + (removeSourceSuccess.isError() && removeLayerSuccess.isError() ? removeSourceSuccess.getError() + "\n" + removeLayerSuccess.getError() : "success"));

                if (trajectoryStr.isEmpty()) {
                    return;
                }

                String colorCode = String.format("#%06X", (0xFFFFFF & getColor(fragment.requireContext())));
                paintTrajectory(style, trajectoryStr, colorCode, "trajectory_layer");

            });

            try {
                JSONObject jsonObject = new JSONObject(trajectoryStr);
                JSONArray bbox = jsonObject.getJSONArray("bbox");
                mapView.getMapboxMap().cameraAnimationsPlugin(plugin -> {
                    Point newCenter = getBBoxCenter(bbox);
                    if (newCenter == null) {
                        return null;
                    }

                    plugin.flyTo(new CameraOptions.Builder()
                            .center(getBBoxCenter(bbox))
                            .build(),
                            new MapAnimationOptions.Builder().duration(2000).build(),
                            null);
                    return null;
                });
            } catch (JSONException e) {
                LOGGER.info("No trajectory retrieved, no need to reset camera");
            }

        });
    }

    private int getColor(Context context) {
        TypedArray typedArray = context.getTheme().obtainStyledAttributes(new int[]{com.google.android.material.R.attr.colorPrimary});
        int colorPrimary = typedArray.getColor(0, Color.BLACK);
        typedArray.recycle();
        return colorPrimary;
    }

    private void paintTrajectory(Style style, String trajectory, String colorCode, String mode) {
        Expected<String, None> removeLayerSuccess = style.removeStyleLayer("trajectory_layer_" + mode);
        Expected<String, None> removeSourceSuccess = style.removeStyleSource("trajectory_" + mode);
        LOGGER.debug("trajectory: layer and source removed result " + (removeSourceSuccess.isError() && removeLayerSuccess.isError() ? removeSourceSuccess.getError() + "\n" + removeLayerSuccess.getError() : "success"));

        JSONObject sourceJson = new JSONObject();
        try {
            sourceJson.put("type", "geojson");
            sourceJson.put("data", trajectory);
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
        Expected<String, None> success = style.addStyleSource("trajectory_" + mode, Objects.requireNonNull(Value.fromJson(sourceJson.toString()).getValue()));
        LOGGER.debug("trajectory: source created " + (success.isError() ? success.getError() : "success"));

        JSONObject layerJson = new JSONObject();
        try {
            layerJson.put("id", "trajectory_layer_" + mode);
            layerJson.put("type", "line");
            layerJson.put("source", "trajectory_" + mode);
            layerJson.put("line-join", "bevel");

            JSONObject paint = new JSONObject();
            paint.put("line-color", colorCode);
            paint.put("line-width", 8);
            layerJson.put("paint", paint);
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }
        Expected<String, None> layerSuccess = style.addStyleLayer(Objects.requireNonNull(Value.fromJson(layerJson.toString()).getValue()), new LayerPosition(null, null, null));
        LOGGER.debug("trajectory: layer created " + (layerSuccess.isError() ? layerSuccess.getError() : "success"));
    }

    private Point getBBoxCenter(JSONArray bbox) {
        try {
            double lngAvg = (bbox.getDouble(0) + bbox.getDouble(2)) / 2;
            double latAvg = (bbox.getDouble(1) + bbox.getDouble(3)) / 2;
            return Point.fromLngLat(lngAvg, latAvg);
        } catch (JSONException e) {
            return null;
        }
    }
}
