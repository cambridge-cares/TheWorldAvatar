package uk.ac.cam.cares.jps.sensor.ui;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;

public class RecordingState {
    private final MutableLiveData<Boolean> _isRecording = new MutableLiveData<>(false);

    private final MutableLiveData<Boolean> _hasAccountError = new MutableLiveData<>(false);

    private final MutableLiveData<List<SensorType>> _selectedSensors = new MutableLiveData<>(new ArrayList<>());
    private final MutableLiveData<Boolean> _allToggledOn = new MutableLiveData<>(false);

    public LiveData<Boolean> getIsRecording() {
        return _isRecording;
    }

    public LiveData<Boolean> getHasAccountError() {
        return _hasAccountError;
    }

    public LiveData<List<SensorType>> getSelectedSensors() {
        return _selectedSensors;
    }

    public LiveData<Boolean> getAllToggledOn() {
        return _allToggledOn;
    }

    public void setIsRecording(Boolean isRecording) {
        _isRecording.postValue(isRecording);
    }

    public void setHasAccountError(Boolean hasAccountError) {
        _hasAccountError.postValue(hasAccountError);
    }

    public void setSelectedSensors(List<SensorType> selectedSensors) {
        _selectedSensors.postValue(selectedSensors);
    }

    public void setAllToggledOn(Boolean allToggledOn) {
        _allToggledOn.setValue(allToggledOn);
    }
}
