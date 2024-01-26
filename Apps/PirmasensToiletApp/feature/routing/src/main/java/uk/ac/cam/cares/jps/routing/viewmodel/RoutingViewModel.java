package uk.ac.cam.cares.jps.routing.viewmodel;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.RouteRepository;

@HiltViewModel
public class RoutingViewModel extends ViewModel {
    public MutableLiveData<String> routeGeoJsonData = new MutableLiveData<>();
    RepositoryCallback<String> routeGeoJsonUrlCallback = new RepositoryCallback<String>() {
        @Override
        public void onSuccess(String result) {
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

    public MutableLiveData<String> getRouteGeoJsonData() {
        return routeGeoJsonData;
    }
}
