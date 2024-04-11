package uk.ac.cam.cares.jps.timeline.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.TrajectoryRepository;
import uk.ac.cam.cares.jps.sensor.SensorRepository;

@HiltViewModel
public class TrajectoryViewModel extends ViewModel {

    private TrajectoryRepository trajectoryRepository;
    private SensorRepository sensorRepository;
    private MutableLiveData<String> _trajectory = new MutableLiveData<>();
    private MutableLiveData<String> _trajectoryError = new MutableLiveData<>();
    private MutableLiveData<Boolean> _isFetchingTrajectory = new MutableLiveData<>();
    public LiveData<String> trajectory = _trajectory;
    public LiveData<String> trajectoryError = _trajectoryError;
    public LiveData<Boolean> isFetchingTrajecjtory = _isFetchingTrajectory;

    @Inject
    public TrajectoryViewModel(TrajectoryRepository routeRepository, SensorRepository sensorRepository) {
        this.trajectoryRepository = routeRepository;
        this.sensorRepository = sensorRepository;
    }

    public void getTrajectory() {
        // todo: be careful on the concurrency issue because this method is called at multiple places!
        _isFetchingTrajectory.setValue(true);

        trajectoryRepository.getTrajectory(new RepositoryCallback<String>() {
            @Override
            public void onSuccess(String result) {
                _trajectory.postValue(result);
                _isFetchingTrajectory.postValue(false);
            }

            @Override
            public void onFailure(Throwable error) {
                _trajectoryError.postValue(error.getMessage());
            }
        });
    }

    public void registerPhoneToUser() {
        sensorRepository.registerAppToUser();
    }
}
