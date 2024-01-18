package uk.ac.cam.cares.jps.routing;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;

import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.ViewModelProvider;
import androidx.lifecycle.ViewModelStoreOwner;

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
        toiletViewModel.getToiletsGeoJsonData().observe(fragment, data -> {
            LOGGER.debug("route: source created " + data);
            // Add toilets locations
            try {
                JSONObject featureCollection = new JSONObject(data);
                if ("FeatureCollection".equals(featureCollection.optString("type"))) {
                    // todo: process the raw data in network layer
                    JSONArray features = featureCollection.getJSONArray("features");

                    for (int i = 0; i < features.length(); i++) {
                        JSONObject feature = features.getJSONObject(i);
                        JSONObject geometry = feature.getJSONObject("geometry");
                        JSONObject properties = feature.getJSONObject("properties");

                        if ("Point".equals(geometry.optString("type"))) {
                            JSONArray coordinates = geometry.getJSONArray("coordinates");

                            // Extract longitude and latitude
                            double longitude = coordinates.getDouble(0);
                            double latitude = coordinates.getDouble(1);

                            Point toiletPoint = Point.fromLngLat(longitude, latitude);

                            // event listener on markers
                            // when user clicks on toilet marks
                            // then he/she should be able to see the details of it
                            addMarker(toiletPoint, context.getResources().getColor(R.color.end_marker_color));

                            System.out.println("Feature " + i + ": Latitude=" + latitude + ", Longitude=" + longitude);
                            // You can store these values in a data structure or perform any other operations needed
                        }
                    }
                } else {
                    System.out.println("Invalid GeoJSON: Not a FeatureCollection");
                }
            } catch (JSONException e) {
                e.printStackTrace();
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
