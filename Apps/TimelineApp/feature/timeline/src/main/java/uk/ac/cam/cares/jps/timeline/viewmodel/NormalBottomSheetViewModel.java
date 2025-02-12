package uk.ac.cam.cares.jps.timeline.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.DatesWithTrajectoryRepository;
import uk.ac.cam.cares.jps.model.YearMonthCompositeKey;
import uk.ac.cam.cares.jps.model.ActivitySummary;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

/**
 * ViewModel that manages the state of NormalBottomSheet
 */
@HiltViewModel
public class NormalBottomSheetViewModel extends ViewModel {
    private DatesWithTrajectoryRepository datesWithTrajectoryRepository;
    private MutableLiveData<LocalDate> _selectedDate = new MutableLiveData<>(LocalDate.now());
    private MutableLiveData<Map<YearMonthCompositeKey, List<Integer>>> _datesWithTrajectory = new MutableLiveData<>();
    private MutableLiveData<List<ActivitySummary>> _activitySummaryData = new MutableLiveData<>();

    public LiveData<LocalDate> selectedDate = _selectedDate;
    public LiveData<Map<YearMonthCompositeKey, List<Integer>>> datesWithTrajectory = _datesWithTrajectory;
    public LiveData<List<ActivitySummary>> activitySummaryData = _activitySummaryData;

    /**
     * Constructor of the class. Instantiation is done with ViewProvider and dependency injection
     * @param datesWithTrajectoryRepository
     */
    @Inject
    public NormalBottomSheetViewModel(DatesWithTrajectoryRepository datesWithTrajectoryRepository) {
        this.datesWithTrajectoryRepository = datesWithTrajectoryRepository;
    }

    /**
     * Set selected date to the previous date
     */
    public void setToLastDate() {
        _selectedDate.setValue(_selectedDate.getValue().minusDays(1));
    }

    /**
     * Set the selected date to the next date
     */
    public void setToNextDate() {
        _selectedDate.setValue(_selectedDate.getValue().plusDays(1));
    }

    /**
     * Set the selected date
     * @param date selected date
     */
    public void setDate(LocalDate date) {
        _selectedDate.setValue(date);
    }

    /**
     * Get dates which has trajectory data from server
     * @param timezone current timezone
     */
    public void getDatesWithTrajectory(String timezone) {
        datesWithTrajectoryRepository.getDatesWithTrajectory(timezone, new RepositoryCallback<>() {
            @Override
            public void onSuccess(Map<YearMonthCompositeKey, List<Integer>> result) {
                _datesWithTrajectory.postValue(result);
            }

            @Override
            public void onFailure(Throwable error) {
                // do nothing for now
            }
        });
    }

    /**
     * Get the user selected date in millisecond
     * @return selected date in millisecond
     */
    public long getSelectedDateLong() {
        return selectedDate.getValue().atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli();
    }

    /**
     * parses the trajectoryjson to store start and end times for each segment(activity)
     */
    public void parseActivitySummary(String trajectoryJson) {
    List<ActivitySummary> summaries = new ArrayList<>();

    try {
        JSONObject trajectoryStr = new JSONObject(trajectoryJson);
        JSONArray features = trajectoryStr.getJSONArray("features");

        for (int i = 0; i < features.length(); i++) {
            JSONObject feature = features.getJSONObject(i);
            JSONObject properties = feature.getJSONObject("properties");

            String activityType = properties.optString("activity_type", "unknown");
            long startTime = properties.optLong("start_time", 0);
            long endTime = properties.optLong("end_time", 0);

            summaries.add(new ActivitySummary(activityType, startTime, endTime));
        }

    } catch (JSONException e) {
        e.printStackTrace();
    }

    _activitySummaryData.postValue(summaries);
}

}
