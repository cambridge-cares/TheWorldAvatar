package uk.ac.cam.cares.jps.timeline.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.DatesWithTrajectoryRepository;
import uk.ac.cam.cares.jps.model.ActivityItem;
import uk.ac.cam.cares.jps.model.SummaryActivityItem;
import uk.ac.cam.cares.jps.model.UniqueSessions;
import uk.ac.cam.cares.jps.model.YearMonthCompositeKey;
import uk.ac.cam.cares.jps.timelinemap.R;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

/**
 * ViewModel that manages the state of NormalBottomSheet
 */
@HiltViewModel
public class NormalBottomSheetViewModel extends ViewModel {
    private DatesWithTrajectoryRepository datesWithTrajectoryRepository;
    private MutableLiveData<LocalDate> _selectedDate = new MutableLiveData<>(LocalDate.now());
    private MutableLiveData<Map<YearMonthCompositeKey, List<Integer>>> _datesWithTrajectory = new MutableLiveData<>();
    private MutableLiveData<List<ActivityItem>> _activityItemData = new MutableLiveData<>();
    private MutableLiveData<List<UniqueSessions>> _uniqueSessions = new MutableLiveData<>();
    private MutableLiveData<List<SummaryActivityItem>> _activityItemSummaryData = new MutableLiveData<>();

    public LiveData<LocalDate> selectedDate = _selectedDate;
    public LiveData<Map<YearMonthCompositeKey, List<Integer>>> datesWithTrajectory = _datesWithTrajectory;
    public LiveData<List<ActivityItem>> activityItemData = _activityItemData;
    public LiveData<List<UniqueSessions>> uniqueSessions = _uniqueSessions;
    public LiveData<List<SummaryActivityItem>> activityItemSummaryData = _activityItemSummaryData;

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


    public void parseUniqueSessions(String trajectoryJson) {

        List<String> uniqueSessionNames = new ArrayList<>();

        try {
            JSONObject trajectoryStr = new JSONObject(trajectoryJson);
            JSONArray features = trajectoryStr.getJSONArray("features");

            for(int i = 0; i < features.length(); i++) {
                JSONObject feature = features.getJSONObject(i);
                JSONObject properties = feature.getJSONObject("properties");

                String sessionId = properties.optString("session_id", "unknown");

                if(!uniqueSessionNames.contains(sessionId)) {
                    uniqueSessionNames.add(sessionId);
                }

            }
        }

        catch(JSONException e) {
            e.printStackTrace();
        }

        List<UniqueSessions> finalResult = new ArrayList<>();
        for(int i = 1; i <= uniqueSessionNames.size(); i++) {
            List<ActivityItem> activities = parseActivityItemsBySession(trajectoryJson, uniqueSessionNames.get(i-1));
            UniqueSessions uniqueSession = new UniqueSessions(uniqueSessionNames.get(i-1), activities, "Trip " + i);
            finalResult.add(uniqueSession);
         }
          _uniqueSessions.postValue(finalResult);
    }


    public List<ActivityItem> parseActivityItemsBySession(String trajectoryJson, String sessionId) {

        List<ActivityItem> summaries = new ArrayList<>();
        try {
            JSONObject trajectoryStr = new JSONObject(trajectoryJson);
            JSONArray features = trajectoryStr.getJSONArray("features");

            for (int i = 0; i < features.length(); i++) {
                JSONObject feature = features.getJSONObject(i);
                JSONObject properties = feature.getJSONObject("properties");

                String activityType = properties.optString("activity_type", "unknown");
                String session = properties.optString("session_id", "unknown");

                if (session.equals(sessionId)) {
                    int activityImage = R.drawable.baseline_man_24;
                    if (activityType.equals("walking")) activityImage = R.drawable.baseline_directions_walk_24;
                    if (activityType.equals("vehicle")) activityImage = R.drawable.baseline_directions_car_24;
                    if (activityType.equals("bike")) activityImage = R.drawable.baseline_directions_bike_24;
                    long startTime = properties.optLong("start_time", 0);
                    long endTime = properties.optLong("end_time", 0);

                    summaries.add(new ActivityItem(activityImage, startTime, endTime));
                }
            }

        } catch (JSONException e) {
            e.printStackTrace();
        }
        return summaries;
    }


    public void parseActivitySummary(String trajectoryJson) {
        List<SummaryActivityItem> summaries = new ArrayList<>();

        try {
            JSONObject trajectoryStr = new JSONObject(trajectoryJson);
            JSONArray features = trajectoryStr.getJSONArray("features");

            Map<String, List<Integer>> distancePerActivityType = new HashMap<>();
            Map<String, List<Long>> timePerActivityType = new HashMap<>();

            for (int i = 0; i < features.length(); i++) {
                JSONObject feature = features.getJSONObject(i);
                JSONObject properties = feature.getJSONObject("properties");

                String activityType = properties.optString("activity_type", "unknown");
                long startTime = properties.optLong("start_time", 0);
                long endTime = properties.optLong("end_time", 0);
                int distance = properties.optInt("distance_traveled", 0);

                long minutes = (endTime > startTime) ? differenceInTime(startTime, endTime) : 0;

                timePerActivityType.putIfAbsent(activityType, new ArrayList<>());
                timePerActivityType.get(activityType).add(minutes);

                distancePerActivityType.putIfAbsent(activityType, new ArrayList<>());
                distancePerActivityType.get(activityType).add(distance);
            }

            for (String activity : distancePerActivityType.keySet()) {
                int totalDistance = activity.equals("still") ? 0 : addTogether(distancePerActivityType.get(activity));
                long totalTime = addTogetherLong(timePerActivityType.get(activity));

                int activityImage = R.drawable.baseline_man_24;
                if (activity.equals("walking")) activityImage = R.drawable.baseline_directions_walk_24;
                else if (activity.equals("vehicle")) activityImage = R.drawable.baseline_directions_car_24;
                else if (activity.equals("bike")) activityImage = R.drawable.baseline_directions_bike_24;

                summaries.add(new SummaryActivityItem(activityImage, totalDistance, totalTime));
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }

        _activityItemSummaryData.postValue(summaries);
    }

    private long addTogetherLong(List<Long> longs) {
        return longs.stream().mapToLong(Long::longValue).sum();
    }

    private int addTogether(List<Integer> integers) {
        return integers.stream().mapToInt(Integer::intValue).sum();
    }

    private long differenceInTime(long startTime, long endTime) {
        return ChronoUnit.MINUTES.between(
            Instant.ofEpochMilli(startTime).atZone(ZoneId.systemDefault()).toLocalDateTime(),
            Instant.ofEpochMilli(endTime).atZone(ZoneId.systemDefault()).toLocalDateTime()
        );
    }

}
