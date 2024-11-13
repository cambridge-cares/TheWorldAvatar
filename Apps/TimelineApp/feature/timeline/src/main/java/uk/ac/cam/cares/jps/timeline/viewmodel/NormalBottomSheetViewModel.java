package uk.ac.cam.cares.jps.timeline.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.DatesWithTrajectoryRepository;
import uk.ac.cam.cares.jps.model.YearMonthCompositeKey;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

/**
 * ViewModel that manages the state of NormalBottomSheet
 */
@HiltViewModel
public class NormalBottomSheetViewModel extends ViewModel {
    private DatesWithTrajectoryRepository datesWithTrajectoryRepository;
    private MutableLiveData<LocalDate> _selectedDate = new MutableLiveData<>(LocalDate.now());
    private MutableLiveData<Map<YearMonthCompositeKey, List<Integer>>> _datesWithTrajectory = new MutableLiveData<>();

    public LiveData<LocalDate> selectedDate = _selectedDate;
    public LiveData<Map<YearMonthCompositeKey, List<Integer>>> datesWithTrajectory = _datesWithTrajectory;

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
}
