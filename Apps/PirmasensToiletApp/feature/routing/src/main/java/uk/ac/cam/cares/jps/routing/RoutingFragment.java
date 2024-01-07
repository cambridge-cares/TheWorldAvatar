package uk.ac.cam.cares.jps.routing;

import android.Manifest;
import android.annotation.SuppressLint;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.location.Location;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.Toast;


import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.ViewModelProvider;
import androidx.navigation.NavController;
import androidx.navigation.fragment.NavHostFragment;

import com.mapbox.android.core.permissions.PermissionsListener;
import com.mapbox.android.core.permissions.PermissionsManager;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.Priority;
import com.google.android.gms.tasks.OnSuccessListener;
import com.mapbox.android.core.permissions.PermissionsManager;
import com.mapbox.bindgen.Expected;
import com.mapbox.bindgen.None;
import com.mapbox.bindgen.Value;
import com.mapbox.geojson.Point;
import com.mapbox.maps.CameraOptions;
import com.mapbox.maps.LayerPosition;
import com.mapbox.maps.MapView;
import com.mapbox.maps.Style;
import com.mapbox.maps.plugin.LocationPuck;
import com.mapbox.maps.plugin.LocationPuck2D;
import com.mapbox.maps.plugin.gestures.GesturesUtils;
import com.mapbox.maps.plugin.locationcomponent.LocationComponentPlugin;
import com.mapbox.maps.plugin.locationcomponent.LocationConsumer;
import com.mapbox.maps.plugin.locationcomponent.LocationProvider;
import com.mapbox.maps.extension.style.sources.generated.GeoJsonSource;
import com.mapbox.maps.plugin.Plugin;
import com.mapbox.maps.plugin.annotation.Annotation;
import com.mapbox.maps.plugin.annotation.AnnotationPlugin;
import com.mapbox.maps.plugin.annotation.AnnotationType;
import com.mapbox.maps.plugin.annotation.generated.OnPointAnnotationClickListener;
import com.mapbox.maps.plugin.annotation.generated.OnPointAnnotationDragListener;
import com.mapbox.maps.plugin.annotation.generated.PointAnnotation;
import com.mapbox.maps.plugin.annotation.generated.PointAnnotationManager;
import com.mapbox.maps.plugin.annotation.generated.PointAnnotationOptions;
import com.mapbox.maps.plugin.locationcomponent.LocationPuckManager;
import com.mapbox.maps.plugin.locationcomponent.OnIndicatorPositionChangedListener;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Objects;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.routing.databinding.FragmentMapBinding;


@AndroidEntryPoint
public class RoutingFragment extends Fragment {
    private static final int LOCATION_PERMISSION_REQUEST_CODE = 1;
    private Logger LOGGER = Logger.getLogger(RoutingFragment.class);
    private FragmentMapBinding binding;
    private MapView mapView;

    private ImageView imageView;
    private RoutingViewModel viewModel;
    private ToiletViewModel toiletViewModel;
    // For location services
    private FusedLocationProviderClient fusedLocationClient;
    private LocationRequest locationRequest;
    private PointAnnotationManager pointAnnotationManager;
    private Point startPoint = Point.fromLngLat(7.603772613151207, 49.20316818233611);
    private Point endPoint = Point.fromLngLat(7.601090404024156, 49.20406545333821);

    private OnIndicatorPositionChangedListener onIndicatorPositionChangedListener = positionPoint -> {
//        mapView.getMapboxMap().setCamera(new CameraOptions.Builder().center(positionPoint).build());
        startPoint = Point.fromLngLat(positionPoint.longitude(), positionPoint.latitude());
    };

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentMapBinding.inflate(inflater);

        imageView = binding.getRoot().findViewById(R.id.imageView);
        imageView.setVisibility(View.INVISIBLE);
        // Mapbox setups
        mapView = binding.getRoot().findViewById(R.id.mapView);
        mapView.getMapboxMap().loadStyleUri(Style.MAPBOX_STREETS);
        AnnotationPlugin annotationPlugin = mapView.getPlugin(Plugin.MAPBOX_ANNOTATION_PLUGIN_ID);
        pointAnnotationManager = (PointAnnotationManager) annotationPlugin.createAnnotationManager(AnnotationType.PointAnnotation, null);


        // ViewModel setups
        viewModel = new ViewModelProvider(requireActivity()).get(RoutingViewModel.class);
        toiletViewModel = new ViewModelProvider(requireActivity()).get(ToiletViewModel.class);


        // Location Related tasks if we need to get access to location of device and store it

//        createLocationRequest();
//        requestLocation();
//        startLocationUpdates();

        // Shows user location puck, customization can be applied
        LocationComponentPlugin locationComponent = mapView.getPlugin(Plugin.MAPBOX_LOCATION_COMPONENT_PLUGIN_ID);
        locationComponent.addOnIndicatorPositionChangedListener(onIndicatorPositionChangedListener);
        locationComponent.setEnabled(true);
        return binding.getRoot();
    }


    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        // request for toilet data
        toiletViewModel.getToiletsData();
        // set observer on the model, when data ready, display points
        toiletViewModel.getToiletsGeoJsonData().observe(getViewLifecycleOwner(), data -> {
            LOGGER.debug("route: source created " + data);
            // Add toilets locations
            try {
                JSONObject featureCollection = new JSONObject(data);
                if ("FeatureCollection".equals(featureCollection.optString("type"))) {
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
                            addMarker(toiletPoint, getResources().getColor(R.color.end_marker_color));


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
            // This didn't work because i was not able to add event listener
//            mapView.getMapboxMap().getStyle(style -> {
//
//                // Add toilets data as source to mapbox
//                JSONObject sourceJson = new JSONObject();
//                try {
//                    sourceJson.put("type", "geojson");
//                    sourceJson.put("data", data);
//                } catch (JSONException e) {
//                    throw new RuntimeException(e);
//                }
//                Expected<String, None> success = style.addStyleSource("toilets", Objects.requireNonNull(Value.fromJson(sourceJson.toString()).getValue()));
//                LOGGER.debug("route: source created " + (success.isError() ? success.getError() : "success"));
//                style.addImage("toilet_marker",drawableToBitmap(R.drawable.source_marker, R.color.end_marker_color));
//                JSONObject layerJson = new JSONObject();
//                try {
//                    layerJson.put("id", "toilets_layer");
//                    layerJson.put("type", "symbol");
//                    layerJson.put("icon-image", "toilet_marker");
//                    layerJson.put("source", "toilets");
//                } catch (JSONException e) {
//                    throw new RuntimeException(e);
//                }
//                Expected<String, None> layerSuccess = style.addStyleLayer(Objects.requireNonNull(Value.fromJson(layerJson.toString()).getValue()), new LayerPosition(null, null, null));
//                LOGGER.debug("route: layer created " + (layerSuccess.isError() ? layerSuccess.getError() : "success"));
//            });
        });
        pointAnnotationManager.addClickListener(pointAnnotation -> {
            requestForRouting(startPoint, pointAnnotation.getPoint());
            return true;
        });
        viewModel.getRouteGeoJsonData().observe(getViewLifecycleOwner(), data -> {
            mapView.getMapboxMap().getStyle(style -> {
                Expected<String, None> removeLayerSuccess =  style.removeStyleLayer("route_layer");
                LOGGER.debug("route: source created " + (removeLayerSuccess.isError() ? removeLayerSuccess.getError() : "success"));
                Expected<String, None> removeSourceSuccess =  style.removeStyleSource("route");
                LOGGER.debug("route: source created " + (removeSourceSuccess.isError() ? removeSourceSuccess.getError() : "success"));

                JSONObject sourceJson = new JSONObject();
                try {
                    sourceJson.put("type", "geojson");
                    sourceJson.put("data", data);
                } catch (JSONException e) {
                    throw new RuntimeException(e);
                }
                Expected<String, None> success = style.addStyleSource("route", Objects.requireNonNull(Value.fromJson(sourceJson.toString()).getValue()));
                LOGGER.debug("route: source created " + (success.isError() ? success.getError() : "success"));

                JSONObject layerJson = new JSONObject();
                try {
                    layerJson.put("id", "route_layer");
                    layerJson.put("type", "line");
                    layerJson.put("source", "route");
                } catch (JSONException e) {
                    throw new RuntimeException(e);
                }
                Expected<String, None> layerSuccess = style.addStyleLayer(Objects.requireNonNull(Value.fromJson(layerJson.toString()).getValue()), new LayerPosition(null, null, null));
                LOGGER.debug("route: layer created " + (layerSuccess.isError() ? layerSuccess.getError() : "success"));
            });
        });
    }

    private void requestForRouting(Point startPoint, Point endPoint) {
        LOGGER.debug("FROM("+startPoint+") TO ("+endPoint+")");
        viewModel.getRouteData(startPoint.longitude(), startPoint.latitude(),
                endPoint.longitude(), endPoint.latitude());

    }

    private void addMarker(Point point, int color) {
        PointAnnotationOptions pointAnnotationOptions = new PointAnnotationOptions()
                .withPoint(point)
                .withIconImage(drawableToBitmap(R.drawable.source_marker, color))
                .withIconSize(2.0)
                .withDraggable(false);
        pointAnnotationManager.create(pointAnnotationOptions);
    }


    private LocationCallback locationCallback = new LocationCallback() {
        @Override
        public void onLocationResult(LocationResult locationResult) {
            LOGGER.debug("REQUEST");
            if (locationResult == null) {
                return;
            }
            for (Location location : locationResult.getLocations()) {
                // Handle location updates here
                double latitude = location.getLatitude();
                double longitude = location.getLongitude();
                Point rnd = Point.fromLngLat(longitude, latitude);
                LOGGER.debug("Update Location ");
                addMarker(rnd, getResources().getColor(R.color.end_marker_color));
                // Perform actions with latitude and longitude
                // For example: update map, send to server, etc.
            }
        }
    };

    private void createLocationRequest() {
        locationRequest = new LocationRequest.Builder(1000)
                .setMinUpdateIntervalMillis(1000)
                .setPriority(Priority.PRIORITY_HIGH_ACCURACY)
                .build();
    }

    @SuppressLint("MissingPermission")
    private void startLocationUpdates() {
        fusedLocationClient.requestLocationUpdates(locationRequest, locationCallback, null);
    }

    private void stopLocationUpdates() {
        fusedLocationClient.removeLocationUpdates(locationCallback);
    }

    private void requestLocation() {
        if (ActivityCompat.checkSelfPermission(requireContext(),
                android.Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED
                && ActivityCompat.checkSelfPermission(requireContext(),
                android.Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(requireActivity(),
                    new String[]{android.Manifest.permission.ACCESS_FINE_LOCATION,
                            android.Manifest.permission.ACCESS_COARSE_LOCATION},
                    LOCATION_PERMISSION_REQUEST_CODE);
            return;
        }

        fusedLocationClient.getLastLocation()
                .addOnSuccessListener(requireActivity(), location -> {
                    LOGGER.debug("LOCATION");
                    // Got last known location. In some rare situations, this can be null.
                    if (location != null) {
                        double latitude = location.getLatitude();
                        double longitude = location.getLongitude();
                        LOGGER.debug("LOCATION");
                        // Add user location marker

                        // Use latitude and longitude as needed
                        startPoint = Point.fromLngLat(longitude, latitude);

                    }
                });
    }

//    @Override
//    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
//        if (requestCode == LOCATION_PERMISSION_REQUEST_CODE) {
//            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
//                requestLocation();
//            } else {
//                // Handle permission denied case
//            }
//        }
//    }


    private void enableLocationComponent(@NonNull Style loadedMapStyle) {
        LocationComponentPlugin locationComponent = mapView.getPlugin(Plugin.MAPBOX_LOCATION_COMPONENT_PLUGIN_ID);
//        locationComponent.setLocationProvider(navigationLocationProvider);
        locationComponent.setEnabled(true);
    }

    public Bitmap drawableToBitmap(int drawableId, int color) {
        Drawable drawable = ContextCompat.getDrawable(requireContext(), drawableId);
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


    @Override
    public void onStart() {
        super.onStart();
        mapView.onStart();
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        mapView.onDestroy();
    }

    @Override
    public void onStop() {
        super.onStop();
        mapView.onStop();
    }

    @Override
    public void onLowMemory() {
        super.onLowMemory();
        mapView.onLowMemory();
    }


}
