package uk.ac.cam.cares.jps.timeline.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;
import uk.ac.cam.cares.jps.data.TrajectoryRepository;

@HiltViewModel
public class TrajectoryViewModel extends ViewModel {

    private final TrajectoryRepository trajectoryRepository;
    private final MutableLiveData<String> _trajectory = new MutableLiveData<>();
    private final MutableLiveData<Throwable> _trajectoryError = new MutableLiveData<>();
    private final MutableLiveData<Boolean> _isFetchingTrajectory = new MutableLiveData<>();
    public LiveData<String> trajectory = _trajectory;
    public LiveData<Throwable> trajectoryError = _trajectoryError;
    public LiveData<Boolean> isFetchingTrajecjtory = _isFetchingTrajectory;

    @Inject
    public TrajectoryViewModel(TrajectoryRepository routeRepository) {
        this.trajectoryRepository = routeRepository;
    }

    public void getTrajectory(String date) {
        _isFetchingTrajectory.setValue(true);

        trajectoryRepository.getTrajectory(date, new RepositoryCallback<>() {
            @Override
            public void onSuccess(String result) {
                _trajectory.postValue(result);
                _isFetchingTrajectory.postValue(false);
            }

            @Override
            public void onFailure(Throwable error) {
                _trajectoryError.postValue(error);
                _isFetchingTrajectory.postValue(false);
            }
        });
    }
}
