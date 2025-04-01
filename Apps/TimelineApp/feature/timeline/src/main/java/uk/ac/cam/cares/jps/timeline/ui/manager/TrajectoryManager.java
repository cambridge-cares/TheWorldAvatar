package uk.ac.cam.cares.jps.timeline.ui.manager;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.util.Log;

import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;


import com.mapbox.bindgen.Expected;
import com.mapbox.bindgen.None;
import com.mapbox.bindgen.Value;
import com.mapbox.geojson.Feature;
import com.mapbox.geojson.Point;
import com.mapbox.maps.CameraOptions;
import com.mapbox.maps.LayerPosition;
import com.mapbox.maps.MapView;
import com.mapbox.maps.QueriedFeature;
import com.mapbox.maps.RenderedQueryGeometry;
import com.mapbox.maps.RenderedQueryOptions;
import com.mapbox.maps.ScreenBox;
import com.mapbox.maps.ScreenCoordinate;
import com.mapbox.maps.Style;
import com.mapbox.maps.plugin.animation.MapAnimationOptions;
import com.mapbox.maps.plugin.gestures.GesturesPlugin;
import com.mapbox.maps.plugin.gestures.GesturesUtils;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectoryByDate;
import uk.ac.cam.cares.jps.timeline.viewmodel.NormalBottomSheetViewModel;
import uk.ac.cam.cares.jps.timeline.viewmodel.TrajectoryViewModel;
import uk.ac.cam.cares.jps.timelinemap.R;

/**
 * An UI manager that manages the drawing and removing of trajectories received from server
 */
public class TrajectoryManager {
    private final TrajectoryViewModel trajectoryViewModel;
    private final NormalBottomSheetViewModel normalBottomSheetViewModel;
    private final Logger LOGGER = Logger.getLogger(TrajectoryManager.class);
    private final List<String> layerNames = new ArrayList<>();
    private final Map<String, String> activityColors = new HashMap<>();
    private String layerId;

    /**
     * Constructor of the class
     * @param fragment Host fragment
     * @param mapView  Mapbox map view
     */
    public TrajectoryManager(Fragment fragment, MapView mapView) {
        trajectoryViewModel = new ViewModelProvider(fragment).get(TrajectoryViewModel.class);
        normalBottomSheetViewModel = new ViewModelProvider(fragment).get(NormalBottomSheetViewModel.class);

        activityColors.put("walking", getColorHex(fragment.requireContext(), com.google.android.material.R.attr.colorPrimary));
        activityColors.put("still", getColorHex(fragment.requireContext(), R.attr.colorQuaternary));
        activityColors.put("vehicle", getColorHex(fragment.requireContext(), com.google.android.material.R.attr.colorSecondary));
        activityColors.put("bike", getColorHex(fragment.requireContext(), com.google.android.material.R.attr.colorTertiary));
        activityColors.put("default", getColorHex(fragment.requireContext(), R.attr.colorDefault));

        trajectoryViewModel.trajectory.observe(fragment.getViewLifecycleOwner(), trajectoryByDate -> {
            if (!trajectoryByDate.getDate().equals(normalBottomSheetViewModel.selectedDate.getValue())) {
                trajectoryViewModel.setFetching(true);
                return;
            }

            mapView.getMapboxMap().getStyle(style -> {
                removeAllLayers(style);
                trajectoryViewModel.removeAllClicked();
                if (!trajectoryByDate.getTrajectoryStr().isEmpty()) {
                    paintTrajectoryByActivity(style, trajectoryByDate, activityColors, "default");
                    addTrajectoryClickListener(mapView);
                }
            });

            resetCameraCentre(mapView, trajectoryByDate);
        });
    }

    private String getColorHex(Context context, int colorAttr) {
        TypedArray typedArray = context.getTheme().obtainStyledAttributes(new int[]{colorAttr});
        int color = typedArray.getColor(0, Color.BLACK);
        typedArray.recycle();
        return String.format("#%06X", (0xFFFFFF & color));
    }

    /**
     * Adds a click listener to detect which trajectory segment was clicked.
     */
    private void addTrajectoryClickListener(MapView mapView) {
        GesturesPlugin gesturesPlugin = GesturesUtils.getGestures(mapView);

        gesturesPlugin.addOnMapClickListener(point -> {
            Log.d("TrajectoryClick", "Map clicked at: " + point.longitude() + ", " + point.latitude());

            ScreenCoordinate screenPoint = mapView.getMapboxMap().pixelForCoordinate(point);
            ScreenBox screenBox = new ScreenBox(
                    new ScreenCoordinate(screenPoint.getX() - 20, screenPoint.getY() - 20),
                    new ScreenCoordinate(screenPoint.getX() + 20, screenPoint.getY() + 20)
            );

            mapView.getMapboxMap().queryRenderedFeatures(
                    new RenderedQueryGeometry(screenBox),
                    new RenderedQueryOptions(List.of(this.layerId), null),
                    result -> {
                        if (result.getValue() != null && !result.getValue().isEmpty()) {
                            QueriedFeature clickedFeature = result.getValue().get(0).getQueriedFeature();
                            Feature feature = clickedFeature.getFeature();


                            Integer segmentId = feature.hasProperty("id") ? feature.getNumberProperty("id").intValue() : null;

                            if (segmentId != null) {
                                Log.d("TrajectoryClick", "Segment clicked: " + segmentId);
                                trajectoryViewModel.setClicked(segmentId);
                            } else {
                                Log.d("TrajectoryClick", "Feature clicked but no valid segment ID found.");
                                trajectoryViewModel.setClicked(null);
                            }
                        } else {
                            Log.d("TrajectoryClick", "No trajectory segment clicked.");
                            trajectoryViewModel.setClicked(null);
                        }
                    }
            );

            return true;
        });
    }


    /**
     * Paints the trajectory on the map, ensuring each segment gets a unique ID.
     */
    private void paintTrajectoryByActivity(Style style, TrajectoryByDate trajectoryArr, Map<String, String> activityColors, String mode) {
        String trajectory = trajectoryArr.getTrajectoryStr();
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

            this.layerId = "trajectory_layer_" + mode;

            
            JSONArray colorExpression = new JSONArray();
            colorExpression.put("match"); 
            colorExpression.put(new JSONArray().put("get").put("activity_type")); 

            
            for (Map.Entry<String, String> entry : activityColors.entrySet()) {
                if (!entry.getKey().equals("default")) {
                    colorExpression.put(entry.getKey());
                    colorExpression.put(entry.getValue());
                }
            }

        
            colorExpression.put(activityColors.get("default"));

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

    private void resetCameraCentre(MapView mapView, TrajectoryByDate trajectoryByDate) {
        try {
            JSONObject jsonObject = new JSONObject(trajectoryByDate.getTrajectoryStr());
            JSONArray bbox = jsonObject.getJSONArray("bbox");

            mapView.getMapboxMap().cameraAnimationsPlugin(plugin -> {
                Point newCenter = getBBoxCenter(bbox);
                if (newCenter == null) {
                    return null;
                }
                plugin.flyTo(new CameraOptions.Builder()
                                .center(newCenter)
                                .build(),
                        new MapAnimationOptions.Builder().duration(2000).build(),
                        null);
                return null;
            });
        } catch (JSONException e) {
            LOGGER.info("No trajectory retrieved, no need to reset camera");
        }
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

            if (successLayer.isError()) failureLayers.add("trajectory_layer_" + name);
            if (successSource.isError()) failureSources.add("trajectory_" + name);
        }

        if (!failureLayers.isEmpty() || !failureSources.isEmpty()) {
            LOGGER.error("Failed to remove layers: " + String.join(", ", failureLayers));
            LOGGER.error("Failed to remove sources: " + String.join(", ", failureSources));
        }

        layerNames.clear();
    }
}
