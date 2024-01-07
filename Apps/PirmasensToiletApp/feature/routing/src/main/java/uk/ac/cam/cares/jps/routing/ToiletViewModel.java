package uk.ac.cam.cares.jps.routing;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.Logger;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.ToiletRepository;


@HiltViewModel
public class ToiletViewModel extends ViewModel {
    private Logger LOGGER = Logger.getLogger(ToiletViewModel.class);

    // Holds all toilets GeoJSON data
    private MutableLiveData<String> toiletsGeoJsonData = new MutableLiveData<>();
    private ToiletRepository toiletRepository;
    private RepositoryCallback<String> routeGeoJsonUrlCallback = new RepositoryCallback<String>() {
        @Override
        public void onSuccess(String result) {
            toiletsGeoJsonData.postValue(result);
        }

        @Override
        public void onFailure(Throwable error) {
            error.printStackTrace();
            LOGGER.debug("c to update toilet view model data "+toiletRepository.getRouteGeoJsonUrl());
        }
    };

    @Inject
    public ToiletViewModel(ToiletRepository toiletRepository) {
        this.toiletRepository = toiletRepository;
    }

    public void getToiletsData() {
        toiletRepository.getRouteGeoJsonUrl(routeGeoJsonUrlCallback);
    }

    public MutableLiveData<String> getToiletsGeoJsonData() {
        return toiletsGeoJsonData;
    }
}
