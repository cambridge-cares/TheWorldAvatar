package uk.ac.cam.cares.jps.routing;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.Logger;

import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.ToiletRepository;
import uk.ac.cam.cares.jps.model.Toilet;


@HiltViewModel
public class ToiletViewModel extends ViewModel {
    private Logger LOGGER = Logger.getLogger(ToiletViewModel.class);

    // Holds all toilets GeoJSON data
    private MutableLiveData<List<Toilet>> toiletsGeoJsonData = new MutableLiveData<>();
    private ToiletRepository toiletRepository;
    private RepositoryCallback<List<Toilet>> routeGeoJsonUrlCallback = new RepositoryCallback<List<Toilet>>() {
        @Override
        public void onSuccess(List<Toilet> result) {
            toiletsGeoJsonData.postValue(result);
        }

        @Override
        public void onFailure(Throwable error) {
            error.printStackTrace();
            LOGGER.debug("fail to update toilet view model data " + toiletRepository.getRouteGeoJsonUrl());
        }
    };

    @Inject
    public ToiletViewModel(ToiletRepository toiletRepository) {
        this.toiletRepository = toiletRepository;
    }

    public void getToiletsData() {
        toiletRepository.getRouteGeoJsonUrl(routeGeoJsonUrlCallback);
    }

    public MutableLiveData<List<Toilet>> getToiletsGeoJsonData() {
        return toiletsGeoJsonData;
    }
}
