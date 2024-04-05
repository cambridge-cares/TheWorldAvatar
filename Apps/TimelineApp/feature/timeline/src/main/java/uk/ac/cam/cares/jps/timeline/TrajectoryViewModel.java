package uk.ac.cam.cares.jps.timeline;

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
    public LiveData<String> trajectory = _trajectory;
    public LiveData<String> trajectoryError = _trajectoryError;

    @Inject
    public TrajectoryViewModel(TrajectoryRepository routeRepository, SensorRepository sensorRepository) {
        this.trajectoryRepository = routeRepository;
        this.sensorRepository = sensorRepository;
    }

    public void getTrajectory() {
        trajectoryRepository.getTrajectory(new RepositoryCallback<String>() {
            @Override
            public void onSuccess(String result) {
                _trajectory.postValue(result);
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
