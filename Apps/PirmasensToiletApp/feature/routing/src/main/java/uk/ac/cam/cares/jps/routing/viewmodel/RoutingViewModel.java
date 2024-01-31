package uk.ac.cam.cares.jps.routing.viewmodel;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.Logger;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.RouteRepository;
import uk.ac.cam.cares.jps.model.Route;

@HiltViewModel
public class RoutingViewModel extends ViewModel {
    private static final Logger LOGGER = Logger.getLogger(RoutingViewModel.class);
    public MutableLiveData<Route> routeGeoJsonData = new MutableLiveData<>();

    RepositoryCallback<Route> routeGeoJsonUrlCallback = new RepositoryCallback<Route>() {
        @Override
        public void onSuccess(Route result) {
            routeGeoJsonData.postValue(result);
        }

        @Override
        public void onFailure(Throwable error) {

        }
    };

    private RouteRepository routeRepository;

    @Inject
    public RoutingViewModel(RouteRepository routeRepository) {
        this.routeRepository = routeRepository;
    }

    public void getRouteData(double startLng, double startLat, double endLng, double endLat) {
        routeRepository.getRouteGeoJsonUrl(startLng, startLat, endLng, endLat, routeGeoJsonUrlCallback);
    }

    public void getRouteData(double lng, double lat, boolean isStart) {
        routeRepository.getRouteGeoJsonUrl(lng, lat, isStart, routeGeoJsonUrlCallback);
    }

    public MutableLiveData<Route> getRouteGeoJsonData() {
        return routeGeoJsonData;
    }
}
