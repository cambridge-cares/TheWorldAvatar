package uk.ac.cam.cares.jps.timeline.viewmodel;


import android.util.Log;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import com.mapbox.geojson.Point;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import java.util.List;
import java.util.Objects;
;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectoryByDate;
import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectorySegment;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;
import uk.ac.cam.cares.jps.data.TrajectoryRepository;

/**
 *
 */
@HiltViewModel
public class TrajectoryViewModel extends ViewModel {

    private final TrajectoryRepository trajectoryRepository;
    private final MutableLiveData<TrajectoryByDate> _trajectory = new MutableLiveData<>();
    private final MutableLiveData<Throwable> _trajectoryError = new MutableLiveData<>();
    private final MutableLiveData<Boolean> _isFetchingTrajectory = new MutableLiveData<>();
    private final MutableLiveData<TrajectorySegment> _clickedSegment = new MutableLiveData<>(null);

    public LiveData<TrajectoryByDate> trajectory = _trajectory;
    public LiveData<Throwable> trajectoryError = _trajectoryError;
    public LiveData<Boolean> isFetchingTrajectory = _isFetchingTrajectory;
    public LiveData<TrajectorySegment> clickedSegment = _clickedSegment;

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
                TrajectoryByDate trajectoryByDate = new TrajectoryByDate(result, date);
                _trajectory.postValue(trajectoryByDate);
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

    private boolean isClicked(JSONObject geom, Point clickedPoint) {
        try {
            JSONArray coordinates = geom.getJSONArray("coordinates");

            for (int i = 0; i < coordinates.length(); i++) {
                JSONArray point = coordinates.getJSONArray(i);
                double lng = point.getDouble(0);
                double lat = point.getDouble(1);

                if (isCloseToClickedPoint(lng, lat, clickedPoint)) {
                    return true;
                }
            }
            return false;
        }
        catch(Exception e) {
            return false;
        }
    }

    private boolean isCloseToClickedPoint(double lng, double lat, Point clickedPoint) {
        double threshold = 0.0001;
        return Math.abs(lng - clickedPoint.longitude()) < threshold &&
            Math.abs(lat - clickedPoint.latitude()) < threshold;
    }


    public void setClicked(Point p) {
        
        List<TrajectorySegment> trajectorySegments = Objects.requireNonNull(trajectory.getValue()).getTrajectorySegments();

        for(TrajectorySegment segment  : trajectorySegments) {
        
            JSONObject geom = segment.geom();

            if(isClicked(geom, p)) {
                Log.d("valid click", "clicked segment with id " + segment.id());
                _clickedSegment.postValue(segment);
                return;
            }
        }
        _clickedSegment.postValue(null);
        Log.d("invalid click","not a segment.");

    }

    public void removeAllClicked() {
        _clickedSegment.postValue(null);
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

    public void setFetching(boolean isFetching) {
        _isFetchingTrajectory.postValue(isFetching);
    }
}
