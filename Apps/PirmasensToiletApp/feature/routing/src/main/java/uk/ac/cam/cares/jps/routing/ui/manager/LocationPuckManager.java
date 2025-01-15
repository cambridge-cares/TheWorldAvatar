package uk.ac.cam.cares.jps.routing.ui.manager;

import androidx.annotation.NonNull;
import androidx.appcompat.content.res.AppCompatResources;
import androidx.fragment.app.Fragment;

import com.mapbox.android.gestures.MoveGestureDetector;
import com.mapbox.maps.CameraOptions;
import com.mapbox.maps.MapView;
import com.mapbox.maps.extension.style.expressions.generated.Expression;
import com.mapbox.maps.plugin.LocationPuck2D;
import com.mapbox.maps.plugin.gestures.GesturesUtils;
import com.mapbox.maps.plugin.gestures.OnMoveListener;
import com.mapbox.maps.plugin.locationcomponent.LocationComponentPlugin;
import com.mapbox.maps.plugin.locationcomponent.LocationComponentUtils;
import com.mapbox.maps.plugin.locationcomponent.OnIndicatorBearingChangedListener;
import com.mapbox.maps.plugin.locationcomponent.OnIndicatorPositionChangedListener;

import uk.ac.cam.cares.jps.routing.R;

public class LocationPuckManager {

    private MapView mapView;
    private Fragment hostFragment;

    private OnMoveListener onMoveListener = new OnMoveListener() {
        @Override
        public void onMoveBegin(@NonNull MoveGestureDetector moveGestureDetector) {
            disableLocationTracking();
        }

        @Override
        public boolean onMove(@NonNull MoveGestureDetector moveGestureDetector) {
            return false;
        }

        @Override
        public void onMoveEnd(@NonNull MoveGestureDetector moveGestureDetector) {

        }
    };

    private OnIndicatorBearingChangedListener onIndicatorBearingChangedListener = direction -> {
        mapView.getMapboxMap().setCamera(new CameraOptions.Builder().bearing(direction).build());
    };

    private OnIndicatorPositionChangedListener onIndicatorPositionChangedListener = positionPoint -> {
        mapView.getMapboxMap().setCamera(new CameraOptions.Builder().center(positionPoint).build());
        GesturesUtils.getGestures(mapView).setFocalPoint(mapView.getMapboxMap().pixelForCoordinate(positionPoint));
    };

    public LocationPuckManager(MapView mapView, Fragment hostFragment) {
        this.mapView = mapView;
        this.hostFragment = hostFragment;
    }

    private void disableLocationTracking() {
        GesturesUtils.getGestures(mapView).removeOnMoveListener(onMoveListener);

        LocationComponentPlugin locationComponentPlugin = LocationComponentUtils.getLocationComponent(mapView);
        locationComponentPlugin.removeOnIndicatorBearingChangedListener(onIndicatorBearingChangedListener);
        locationComponentPlugin.removeOnIndicatorPositionChangedListener(onIndicatorPositionChangedListener);
    }

    public void enableLocationTracking() {
        GesturesUtils.getGestures(mapView).addOnMoveListener(onMoveListener);

        LocationComponentPlugin locationPlugin = LocationComponentUtils.getLocationComponent(mapView);
        locationPlugin.updateSettings(locationComponentSettings -> {
            Expression.ExpressionBuilder builder = new Expression.ExpressionBuilder("interpolate");
            builder.stop(expressionBuilder -> {
                expressionBuilder.literal(0.0);
                expressionBuilder.literal(0.6);
                return null;
            });
            builder.stop(expressionBuilder -> {
                expressionBuilder.literal(10.0);
                expressionBuilder.literal(1.0);
                return null;
            });
            builder.zoom();
            builder.interpolate(interpolatorBuilder -> {
                interpolatorBuilder.linear();
                return null;
            });

            locationComponentSettings.setEnabled(true);
            locationComponentSettings.setLocationPuck(new LocationPuck2D(null,
                    AppCompatResources.getDrawable(hostFragment.requireContext(), R.drawable.mapbox_user_puck_icon),
                    AppCompatResources.getDrawable(hostFragment.requireContext(), R.drawable.mapbox_user_icon_shadow)
            ));
            return null;
        });

        locationPlugin.addOnIndicatorBearingChangedListener(onIndicatorBearingChangedListener);
        locationPlugin.addOnIndicatorPositionChangedListener(onIndicatorPositionChangedListener);
        locationPlugin.setEnabled(true);
    }
}
