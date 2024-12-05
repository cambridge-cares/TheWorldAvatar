package uk.ac.cam.cares.jps.timeline.viewmodel;


import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import org.apache.log4j.Logger;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;
import uk.ac.cam.cares.jps.data.TrajectoryRepository;

/**
 *
 */
@HiltViewModel
public class TrajectoryViewModel extends ViewModel {

    private final TrajectoryRepository trajectoryRepository;
    private final MutableLiveData<String> _trajectory = new MutableLiveData<>();
    private final MutableLiveData<Throwable> _trajectoryError = new MutableLiveData<>();
    private final MutableLiveData<Boolean> _isFetchingTrajectory = new MutableLiveData<>();
    public LiveData<String> trajectory = _trajectory;
    public LiveData<Throwable> trajectoryError = _trajectoryError;
    public LiveData<Boolean> isFetchingTrajectory = _isFetchingTrajectory;
    private static final Logger LOGGER = Logger.getLogger(String.valueOf(TrajectoryViewModel.class));


    @Inject
    public TrajectoryViewModel(TrajectoryRepository routeRepository) {
        this.trajectoryRepository = routeRepository;
    }

    public void getTrajectory(LocalDate date) {
        _isFetchingTrajectory.setValue(true);

        long lowerbound = calculateLowerbound(date);
        long upperbound = calculateUpperbound(date);

        trajectoryRepository.getTrajectory(lowerbound, upperbound, new RepositoryCallback<>() {
            @Override
            public void onSuccess(String result) {
                _trajectory.postValue(result);
                _isFetchingTrajectory.postValue(false);
                _trajectoryError.postValue(null);
            }

            @Override
            public void onFailure(Throwable error) {
                _trajectoryError.postValue(error);
                _isFetchingTrajectory.postValue(false);
            }
        });
    }

    private long calculateLowerbound(LocalDate date) {
        ZonedDateTime startOfDay = date.atStartOfDay(ZoneId.systemDefault());
        return startOfDay.toInstant().toEpochMilli();
    }

    private long calculateUpperbound(LocalDate date) {
        ZonedDateTime endOfDay = date.atTime(23, 59, 59, 999999999)
                .atZone(ZoneId.systemDefault());
        return endOfDay.toInstant().toEpochMilli();
    }

    //todo: should use the phone's time zone instead of UTC
    private String convertDateFormat(LocalDate date) {
        ZonedDateTime convertedDateStart = date.atStartOfDay(ZoneId.systemDefault())
                .withZoneSameInstant(ZoneId.of("UTC"));
        return convertedDateStart.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSx"));
    }
}
