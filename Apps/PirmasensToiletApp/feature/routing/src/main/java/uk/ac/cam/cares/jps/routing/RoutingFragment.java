package uk.ac.cam.cares.jps.routing;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.mapbox.maps.MapView;
import com.mapbox.maps.Style;

import org.apache.log4j.Logger;

import dagger.hilt.android.AndroidEntryPoint;
import uk.ac.cam.cares.jps.routing.bottomsheet.ToiletBottomSheet;
import uk.ac.cam.cares.jps.routing.databinding.FragmentMapBinding;
import uk.ac.cam.cares.jps.routing.ui.manager.UserLocationManager;
import uk.ac.cam.cares.jps.routing.ui.manager.RouteManager;
import uk.ac.cam.cares.jps.routing.ui.manager.ToiletMarkerManager;


@AndroidEntryPoint
public class RoutingFragment extends Fragment {
    private Logger LOGGER = Logger.getLogger( RoutingFragment.class);

    private FragmentMapBinding binding;
    private MapView mapView;
    private ImageView imageView;

    // UI manager
    private ToiletMarkerManager toiletMarkerManager;
    private RouteManager routeManager;
    private UserLocationManager userLocationManager;


    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentMapBinding.inflate(inflater);
        imageView = binding.getRoot().findViewById(R.id.imageView);
        imageView.setVisibility(View.INVISIBLE);

        mapView = binding.getRoot().findViewById(R.id.mapView);
        mapView.getMapboxMap().loadStyleUri(Style.MAPBOX_STREETS);

        ToiletBottomSheet toiletBottomSheet = new ToiletBottomSheet(this);

        // init UI manager
        userLocationManager = new UserLocationManager(mapView, this);
        routeManager = new RouteManager(mapView, this);
        toiletMarkerManager = new ToiletMarkerManager(mapView, this, toiletBottomSheet);



        return binding.getRoot();
    }


    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        userLocationManager.requestLastLocation();
        userLocationManager.startLocationUpdates();

        toiletMarkerManager.getToiletsData();
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
