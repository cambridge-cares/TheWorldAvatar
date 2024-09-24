package uk.ac.cam.cares.jps.routing.viewmodel;

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
    private MutableLiveData<List<Toilet>> toilets = new MutableLiveData<>();
    private MutableLiveData<Toilet> selectedToilet = new MutableLiveData<>();
    private MutableLiveData<String> errorMessage = new MutableLiveData<>();
    private ToiletRepository toiletRepository;

    private RepositoryCallback<List<Toilet>> getAllToiletsCallback = new RepositoryCallback<List<Toilet>>() {
        @Override
        public void onSuccess(List<Toilet> result) {
            toilets.postValue(result);
        }

        @Override
        public void onFailure(Throwable error) {
            error.printStackTrace();
            LOGGER.debug("fail to retrieve all toilet locations " + toiletRepository.getRouteGeoJsonUrl());
        }
    };

    @Inject
    public ToiletViewModel(ToiletRepository toiletRepository) {
        this.toiletRepository = toiletRepository;
    }

    public void getToiletsData() {
        toiletRepository.getAllToilets(getAllToiletsCallback);
    }

    public MutableLiveData<List<Toilet>> getToiletsLiveData() {
        return toilets;
    }

    public void getToilet(String id) {
        toiletRepository.getToiletInfo(id, new RepositoryCallback<Toilet>() {
            @Override
            public void onSuccess(Toilet result) {
                selectedToilet.postValue(result);
            }

            @Override
            public void onFailure(Throwable error) {
                LOGGER.error(error.getMessage());
                errorMessage.postValue(String.format("Failed to get toilet at (%s)", id));
            }
        });
    }

    public MutableLiveData<Toilet> getSelectedToilet() {
        return selectedToilet;
    }
}
