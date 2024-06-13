package uk.ac.cam.cares.jps.timeline.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import java.time.LocalDate;


public class NormalBottomSheetViewModel extends ViewModel {
    private MutableLiveData<LocalDate> _selectedDate = new MutableLiveData<>(LocalDate.now());

    public LiveData<LocalDate> selectedDate = _selectedDate;


}
