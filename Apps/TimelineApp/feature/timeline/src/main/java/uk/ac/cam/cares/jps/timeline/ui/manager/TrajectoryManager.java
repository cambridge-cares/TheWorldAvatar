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
import com.mapbox.maps.StyleObjectInfo;
import com.mapbox.maps.plugin.animation.MapAnimationOptions;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import uk.ac.cam.cares.jps.timeline.viewmodel.TrajectoryViewModel;

/**
 * An UI manager that manages the drawing and removing of trajectories received from server
 */
public class TrajectoryManager {
    private TrajectoryViewModel trajectoryViewModel;
    private Logger LOGGER = Logger.getLogger(TrajectoryManager.class);
    private final List<String> layerNames;

    /**
     * Constructor of the class
     * @param fragment Host fragment
     * @param mapView  Mapbox map view
     */
    public TrajectoryManager(Fragment fragment, MapView mapView) {
        trajectoryViewModel = new ViewModelProvider(fragment).get(TrajectoryViewModel.class);
        layerNames = new ArrayList<>();

        trajectoryViewModel.trajectory.observe(fragment.getViewLifecycleOwner(), trajectoryStr -> {
            mapView.getMapboxMap().getStyle(style -> {
                removeAllLayers(style);
                if (trajectoryStr.isEmpty()) {
                    return;
                }

                // String colorCode = String.format("#%06X", (0xFFFFFF & getColor(fragment.requireContext())));
                String colorCode = "#FF0000";
                Map<String, String> activityColors = new HashMap<>();
                activityColors.put("walking", "#0000FF"); // Blue
                activityColors.put("running", "#FF0000"); // Red
                activityColors.put("cycling", "#00FF00"); // Green
                activityColors.put("driving", "#000000"); //Grey
                
                //paintTrajectory(style, trajectoryStr, colorCode, "default");
                paintTrajectoryByActivity(style, trajectoryStr, activityColors, "default");

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

        layerNames.add(mode);
    }

    private void paintTrajectoryByActivity(Style style, String trajectory, Map<String, String> activityColors, String mode) {
    JSONObject sourceJson = new JSONObject();
    try {
        sourceJson.put("type", "geojson");
        sourceJson.put("data", trajectory);
    } catch (JSONException e) {
        throw new RuntimeException(e);
    }

    Expected<String, None> success = style.addStyleSource(
        "trajectory_" + mode,
        Objects.requireNonNull(Value.fromJson(sourceJson.toString()).getValue())
    );
    LOGGER.debug("trajectory: source created " + (success.isError() ? success.getError() : "success"));

    JSONObject layerJson = new JSONObject();
    try {
        layerJson.put("id", "trajectory_layer_" + mode);
        layerJson.put("type", "line");
        layerJson.put("source", "trajectory_" + mode);
        layerJson.put("line-join", "bevel");

        
        JSONArray colorExpression = new JSONArray();
        colorExpression.put("match"); 
        colorExpression.put(new JSONArray().put("get").put("activity_type")); 

        
        for (Map.Entry<String, String> entry : activityColors.entrySet()) {
            if (!entry.getKey().equals("default")) {
                colorExpression.put(entry.getKey());
                colorExpression.put(entry.getValue());
            }
        }

       
        colorExpression.put(activityColors.getOrDefault("default", "#9d9d9d"));

        JSONObject paint = new JSONObject();
        paint.put("line-color", colorExpression);
        paint.put("line-width", 6);
        layerJson.put("paint", paint);
    } catch (JSONException e) {
        throw new RuntimeException(e);
    }

    
    Expected<String, None> layerSuccess = style.addStyleLayer(
        Objects.requireNonNull(Value.fromJson(layerJson.toString()).getValue()),
        new LayerPosition(null, null, null)
    );
    LOGGER.debug("trajectory: layer created " + (layerSuccess.isError() ? layerSuccess.getError() : "success"));

    layerNames.add(mode);
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

    private void removeAllLayers(Style style) {
        List<String> failureLayers = new ArrayList<>();
        List<String> failureSources = new ArrayList<>();
        for (String name : layerNames) {
            Expected<String, None> successLayer = style.removeStyleLayer("trajectory_layer_" + name);
            Expected<String, None> successSource = style.removeStyleSource("trajectory_" + name);
            if (successLayer.isError()) {
                failureLayers.add("trajectory_layer_" + name);
            }
            if (successSource.isError()) {
                failureSources.add("trajectory_" + name);
            }
        }

        if (failureSources.isEmpty() && failureLayers.isEmpty()) {
            return;
        }
        LOGGER.error("Failed to remove layers: " + String.join(", ", failureLayers));
        LOGGER.error("Failed to remove sources: " + String.join(", ", failureSources));

        layerNames.clear();
    }
}