package uk.ac.cam.cares.jps.timeline.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.sensor.data.SensorCollectionStateManagerRepository;
import uk.ac.cam.cares.jps.sensor.data.SensorRepository;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

@HiltViewModel
public class RecordingViewModel extends ViewModel {

    private final SensorRepository sensorRepository;
    private final SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository;

    private final MutableLiveData<Boolean> _isRecording = new MutableLiveData<>(false);
    public final LiveData<Boolean> isRecording = _isRecording;

    private final MutableLiveData<Boolean> _hasAccountError = new MutableLiveData<>(false);
    public final LiveData<Boolean> hasAccountError = _hasAccountError;

    private final MutableLiveData<List<SensorType>> selectedSensors = new MutableLiveData<>(new ArrayList<>());
    private final MutableLiveData<Boolean> allToggledOn = new MutableLiveData<>(false);

    @Inject
    public RecordingViewModel(
            SensorRepository sensorRepository,
            SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository
    ) {
        this.sensorRepository = sensorRepository;
        this.sensorCollectionStateManagerRepository = sensorCollectionStateManagerRepository;
        loadSelectedSensors();
    }

    public LiveData<Boolean> getIsRecording() {
        return isRecording;
    }

    public LiveData<Boolean> getHasAccountError() {
        return hasAccountError;
    }

    public LiveData<List<SensorType>> getSelectedSensors() {
        return selectedSensors;
    }

    public void startRecording() {
        List<SensorType> sensorsToRecord = selectedSensors.getValue();
        if (sensorsToRecord != null && !sensorsToRecord.isEmpty()) {
            sensorRepository.startRecording(sensorsToRecord, new RepositoryCallback<>() {
                @Override
                public void onSuccess(Boolean result) {
                    _isRecording.setValue(result);
                }

                @Override
                public void onFailure(Throwable error) {
                    _hasAccountError.setValue(true);
                    _isRecording.setValue(false);
                }
            });
        } else {
            _hasAccountError.setValue(true);
            _isRecording.setValue(false);
        }
    }

    public void stopRecording() {
        sensorRepository.stopRecording();
        _isRecording.setValue(false);
        sensorCollectionStateManagerRepository.setTaskId("");
        toggleAllSensors(false);
    }

    public void toggleAllSensors(boolean toggle) {
        if (Boolean.TRUE.equals(_isRecording.getValue())) {
            return;
        }

        List<SensorType> updatedSensorTypes = new ArrayList<>();
        for (SensorType type : SensorType.values()) {
            if (toggle) {
                updatedSensorTypes.add(type);
            }
        }
        selectedSensors.setValue(updatedSensorTypes);
        allToggledOn.setValue(toggle);
        saveSelectedSensors(updatedSensorTypes);
    }

    public void clearManagers(String userId) {
        sensorCollectionStateManagerRepository.clearManager(userId);
    }

    private void saveSelectedSensors(List<SensorType> sensors) {
        sensorCollectionStateManagerRepository.setSelectedSensors(sensors);
    }

    private void loadSelectedSensors() {
        sensorCollectionStateManagerRepository.getSelectedSensors(new RepositoryCallback<List<SensorType>>() {
            @Override
            public void onSuccess(List<SensorType> loadedSelectedSensors) {
                selectedSensors.setValue(loadedSelectedSensors);
                allToggledOn.setValue(loadedSelectedSensors.size() == SensorType.values().length);
            }

            @Override
            public void onFailure(Throwable error) {
                selectedSensors.setValue(new ArrayList<>());
                allToggledOn.setValue(false);
            }
        });
    }
}
