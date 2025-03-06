package uk.ac.cam.cares.jps.timeline.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.DatesWithTrajectoryRepository;
import uk.ac.cam.cares.jps.model.YearMonthCompositeKey;
import uk.ac.cam.cares.jps.timeline.model.bottomsheet.ActivityItem;
import uk.ac.cam.cares.jps.timeline.model.bottomsheet.TrajectorySummaryByDate;
import uk.ac.cam.cares.jps.timeline.model.bottomsheet.SummaryActivityItem;
import uk.ac.cam.cares.jps.timeline.model.bottomsheet.Session;
import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectoryByDate;
import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectorySegment;
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
    private MutableLiveData<TrajectorySummaryByDate> _sessionSummary = new MutableLiveData<>();

    public LiveData<LocalDate> selectedDate = _selectedDate;
    public LiveData<Map<YearMonthCompositeKey, List<Integer>>> datesWithTrajectory = _datesWithTrajectory;
    public LiveData<TrajectorySummaryByDate> sessionSummary = _sessionSummary;

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

    public void parseSessionSummaries(TrajectoryByDate trajectory) {
        List<Session> uniqueSessions = parseUniqueSessions(trajectory);
        List<SummaryActivityItem> summaryActivityItems = parseActivitySummary(trajectory);
        LocalDate date = trajectory.getDate();

        TrajectorySummaryByDate sessionSummary = new TrajectorySummaryByDate(date, summaryActivityItems, uniqueSessions);

        _sessionSummary.postValue(sessionSummary);
    }

    private List<Session> parseUniqueSessions(TrajectoryByDate trajectory) {

        List<String> uniqueSessionNames = new ArrayList<>();
        List<TrajectorySegment> trajectorySegments = trajectory.getTrajectory();

        for(TrajectorySegment segment : trajectorySegments) {
            String sessionId = segment.sessionId();
            if(!uniqueSessionNames.contains(sessionId)) {
                uniqueSessionNames.add(sessionId);
            }
        }

        List<Session> finalResult = new ArrayList<>();
        for(int i = 1; i <= uniqueSessionNames.size(); i++) {
            List<ActivityItem> activities = parseActivityItemsBySession(trajectory, uniqueSessionNames.get(i-1));
            Session uniqueSession = new Session(uniqueSessionNames.get(i-1), activities, "Trip " + i);
            finalResult.add(uniqueSession);
         }
          return finalResult;
    }


    private List<ActivityItem> parseActivityItemsBySession(TrajectoryByDate trajectory, String sessionId) {

        List<ActivityItem> summaries = new ArrayList<>();
        List<TrajectorySegment> trajectorySegments = trajectory.getTrajectory();

        for(TrajectorySegment segment  : trajectorySegments) {
            String activity = segment.activityType();
            if(sessionId.equals(segment.sessionId())) {
                int activityImage = getActivityImage(activity);

                long startTime = segment.startTime();
                long endTime = segment.endTime();

                summaries.add(new ActivityItem(activityImage, startTime, endTime));
            }
        }
        return summaries;
    }


    private List<SummaryActivityItem> parseActivitySummary(TrajectoryByDate trajectory) {
        List<SummaryActivityItem> summaries = new ArrayList<>();

        Map<String, List<Integer>> distancePerActivityType = new HashMap<>();
        Map<String, List<Long>> timePerActivityType = new HashMap<>();
        long startTime = 0;

        List<TrajectorySegment> trajectorySegments = trajectory.getTrajectory();

        for(TrajectorySegment segment : trajectorySegments) {

            String activityType = segment.activityType();
            startTime = segment.startTime();
            long endTime = segment.endTime();
            int distance = segment.distanceTraveled();

            long minutes = (endTime > startTime) ? differenceInTime(startTime, endTime) : 0;

            timePerActivityType.putIfAbsent(activityType, new ArrayList<>());
            Objects.requireNonNull(timePerActivityType.get(activityType)).add(minutes);

            distancePerActivityType.putIfAbsent(activityType, new ArrayList<>());
            Objects.requireNonNull(distancePerActivityType.get(activityType)).add(distance);
        }
        for (String activity : distancePerActivityType.keySet()) {
            int totalDistance = activity.equals("still") ? 0 : addTogether(Objects.requireNonNull(distancePerActivityType.get(activity)));
            long totalTime = addTogetherLong(Objects.requireNonNull(timePerActivityType.get(activity)));

            int activityImage = getActivityImage(activity);

            summaries.add(new SummaryActivityItem(activityImage, totalDistance, totalTime));
        }

        return summaries;
    }

    private int getActivityImage(String activity) {
        int activityImage = R.drawable.baseline_man_24;
        if (activity.equals("walking")) activityImage = R.drawable.baseline_directions_walk_24;
        else if (activity.equals("vehicle")) activityImage = R.drawable.baseline_directions_car_24;
        else if (activity.equals("bike")) activityImage = R.drawable.baseline_directions_bike_24;
        else if (activity.equals("unknown")) activityImage = R.drawable.baseline_arrow_circle_right_24;

        return activityImage;
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
