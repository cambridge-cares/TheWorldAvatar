package uk.ac.cam.cares.jps.routing.ui.manager;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.LifecycleRegistry;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.bottomsheet.BottomSheetBehavior;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.mapbox.geojson.Point;
import com.mapbox.maps.MapView;
import com.mapbox.maps.plugin.Plugin;
import com.mapbox.maps.plugin.annotation.AnnotationPlugin;
import com.mapbox.maps.plugin.annotation.AnnotationType;
import com.mapbox.maps.plugin.annotation.generated.PointAnnotationManager;
import com.mapbox.maps.plugin.annotation.generated.PointAnnotationOptions;

import org.apache.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.model.Route;
import uk.ac.cam.cares.jps.model.Toilet;
import uk.ac.cam.cares.jps.routing.R;
import uk.ac.cam.cares.jps.routing.RoutingFragment;
import uk.ac.cam.cares.jps.routing.viewmodel.LocationViewModel;
import uk.ac.cam.cares.jps.routing.viewmodel.RoutingViewModel;
import uk.ac.cam.cares.jps.routing.viewmodel.ToiletViewModel;
import uk.ac.cam.cares.jps.routing.bottomsheet.ToiletBottomSheet;

public class ToiletMarkerManager {
    private Logger LOGGER = Logger.getLogger(ToiletMarkerManager.class);
    private PointAnnotationManager pointAnnotationManager;
    private ToiletViewModel toiletViewModel;
    private RoutingViewModel routingViewModel;
    private LocationViewModel locationViewModel;
    private Context context;


    public ToiletMarkerManager(MapView mapView, Fragment fragment, ToiletBottomSheet toiletBottomSheet) {

        AnnotationPlugin annotationPlugin = mapView.getPlugin(Plugin.MAPBOX_ANNOTATION_PLUGIN_ID);
        pointAnnotationManager = (PointAnnotationManager) annotationPlugin.createAnnotationManager(AnnotationType.PointAnnotation, null);
        this.context = fragment.requireContext();

        locationViewModel = new ViewModelProvider(fragment).get(LocationViewModel.class);

        routingViewModel = new ViewModelProvider(fragment).get(RoutingViewModel.class);

        toiletViewModel = new ViewModelProvider(fragment).get(ToiletViewModel.class);

        toiletViewModel.getToiletsLiveData().observe(fragment, toilets -> {
            LOGGER.debug("Received toilets number: " + toilets.size());

            for (Toilet toilet : toilets) {
                addMarker(toilet, context.getResources().getColor(R.color.end_marker_color));
                LOGGER.debug("Longitude=" + toilet.getLocation().longitude() + ", Latitude=" + toilet.getLocation().latitude());
            }
        });

        pointAnnotationManager.addClickListener(pointAnnotation -> {
            LOGGER.debug("DEBUGPOINTX"+pointAnnotation.getData());
            toiletViewModel.getToilet(
                    pointAnnotation.getData().getAsJsonObject().get("osm_id").getAsString());
            toiletBottomSheet.bottomSheetBehavior.setState(BottomSheetBehavior.STATE_EXPANDED);

            if (locationViewModel.getCurrentLocationValue() == null) {
                LOGGER.info("Location permission not granted for distance and estimated time");
                return false;
            }
            routingViewModel.getRouteData(
                    locationViewModel.getCurrentLocationValue().longitude(),
                    locationViewModel.getCurrentLocationValue().latitude(),
                    pointAnnotation.getPoint().longitude(),
                    pointAnnotation.getPoint().latitude()
            );

            // clear current route
            routingViewModel.showRoute.setValue(false);
            return true;
        });
    }

    public void getToiletsData() {
        toiletViewModel.getToiletsData();
    }

    private void addMarker(Toilet toilet, int color) {
        JsonObject jsonObject = new JsonObject();

        // Add some key-value pairs
        jsonObject.addProperty("longitude", toilet.getLocation().longitude());
        jsonObject.addProperty("latitude", toilet.getLocation().latitude());
        jsonObject.addProperty("osm_id", toilet.getId());
        PointAnnotationOptions pointAnnotationOptions = new PointAnnotationOptions()
                .withPoint(toilet.getLocation())
                .withIconImage(drawableToBitmap(R.drawable.source_marker, color))
                .withIconSize(2.0)
                .withData(jsonObject)
                .withDraggable(false);
        pointAnnotationManager.create(pointAnnotationOptions);
    }

    private Bitmap drawableToBitmap(int drawableId, int color) {
        Drawable drawable = ContextCompat.getDrawable(context, drawableId);
        drawable.setTint(color);
        Bitmap bitmap = null;

        if (drawable instanceof BitmapDrawable) {
            BitmapDrawable bitmapDrawable = (BitmapDrawable) drawable;
            if (bitmapDrawable.getBitmap() != null) {
                return bitmapDrawable.getBitmap();
            }
        }

        if (drawable.getIntrinsicWidth() <= 0 || drawable.getIntrinsicHeight() <= 0) {
            bitmap = Bitmap.createBitmap(1, 1, Bitmap.Config.ARGB_8888); // Single color bitmap will be created of 1x1 pixel
        } else {
            bitmap = Bitmap.createBitmap(drawable.getIntrinsicWidth(), drawable.getIntrinsicHeight(), Bitmap.Config.ARGB_8888);
        }

        Canvas canvas = new Canvas(bitmap);
        drawable.setBounds(0, 0, canvas.getWidth(), canvas.getHeight());
        drawable.draw(canvas);
        return bitmap;
    }

}
