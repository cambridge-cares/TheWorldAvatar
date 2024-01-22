package uk.ac.cam.cares.jps.routing;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;

import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;

import com.mapbox.geojson.Point;
import com.mapbox.maps.MapView;
import com.mapbox.maps.plugin.Plugin;
import com.mapbox.maps.plugin.annotation.AnnotationPlugin;
import com.mapbox.maps.plugin.annotation.AnnotationType;
import com.mapbox.maps.plugin.annotation.generated.PointAnnotationManager;
import com.mapbox.maps.plugin.annotation.generated.PointAnnotationOptions;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.model.Toilet;

public class ToiletManager {
    private Logger LOGGER = Logger.getLogger(ToiletManager.class);
    private PointAnnotationManager pointAnnotationManager;
    private ToiletViewModel toiletViewModel;
    private Context context;
    private RouteManager routeManager;

    public ToiletManager(MapView mapView, Fragment fragment, RouteManager routeManager) {
        AnnotationPlugin annotationPlugin = mapView.getPlugin(Plugin.MAPBOX_ANNOTATION_PLUGIN_ID);
        pointAnnotationManager = (PointAnnotationManager) annotationPlugin.createAnnotationManager(AnnotationType.PointAnnotation, null);
        this.context = fragment.requireContext();
        this.routeManager = routeManager;

        toiletViewModel = new ViewModelProvider(fragment).get(ToiletViewModel.class);
        // set observer on the model, when data ready, display points
        toiletViewModel.getToiletsGeoJsonData().observe(fragment, toilets -> {
            LOGGER.debug("Received toilets number: " + toilets.size());

            for (Toilet toilet : toilets) {
                addMarker(toilet.getLocation(), context.getResources().getColor(R.color.end_marker_color));
                LOGGER.debug("Longitude=" + toilet.getLocation().longitude() + ", Latitude=" + toilet.getLocation().latitude());
            }
        });

        pointAnnotationManager.addClickListener(pointAnnotation -> {
            routeManager.requestForRouting(pointAnnotation.getPoint());
            return true;
        });
    }

    public void getToiletsData() {
        toiletViewModel.getToiletsData();
    }

    private void addMarker(Point point, int color) {
        PointAnnotationOptions pointAnnotationOptions = new PointAnnotationOptions()
                .withPoint(point)
                .withIconImage(drawableToBitmap(R.drawable.source_marker, color))
                .withIconSize(2.0)
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
