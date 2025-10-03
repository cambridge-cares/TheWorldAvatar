package uk.ac.cam.cares.jps.timeline.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.ViewModel;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.sensor.data.SensorCollectionStateManagerRepository;
import uk.ac.cam.cares.jps.sensor.data.SensorRepository;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.sensor.ui.RecordingState;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

@HiltViewModel
public class RecordingViewModel extends ViewModel {

    private final SensorRepository sensorRepository;
    private final SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository;
    private final RecordingState recordingState;

    @Inject
    public RecordingViewModel(
            SensorRepository sensorRepository,
            SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository,
            RecordingState recordingState
    ) {
        this.sensorRepository = sensorRepository;
        this.sensorCollectionStateManagerRepository = sensorCollectionStateManagerRepository;
        this.recordingState = recordingState;
        loadSelectedSensors();
    }

    public LiveData<Boolean> getIsRecording() {
        return recordingState.getIsRecording();
    }

    public LiveData<Boolean> getHasAccountError() {
        return recordingState.getHasAccountError();
    }

    public LiveData<List<SensorType>> getSelectedSensors() {
        return recordingState.getSelectedSensors();
    }

    public void startRecording() {
        List<SensorType> sensorsToRecord = recordingState.getSelectedSensors().getValue();
        if (sensorsToRecord != null && !sensorsToRecord.isEmpty()) {
            sensorRepository.startRecording(sensorsToRecord, new RepositoryCallback<>() {
                @Override
                public void onSuccess(Boolean result) {
                    recordingState.setIsRecording(result);
                }

                @Override
                public void onFailure(Throwable error) {
                    recordingState.setHasAccountError(true);
                    recordingState.setIsRecording(false);
                }
            });
        } else {
            recordingState.setHasAccountError(true);
            recordingState.setIsRecording(false);
        }
    }

    public void stopRecording() {
        sensorRepository.stopRecording();
        recordingState.setIsRecording(false);
        sensorCollectionStateManagerRepository.setTaskId("");
        toggleAllSensors(false);
    }

    public void toggleAllSensors(boolean toggle) {
        if (Boolean.TRUE.equals(recordingState.getIsRecording().getValue())) {
            return;
        }

        List<SensorType> updatedSensorTypes = new ArrayList<>();
        for (SensorType type : SensorType.values()) {
            if (toggle) {
                updatedSensorTypes.add(type);
            }
        }
        recordingState.setSelectedSensors(updatedSensorTypes);
        recordingState.setAllToggledOn(toggle);
        saveSelectedSensors(updatedSensorTypes);
    }

    public void clearManagers(String userId) {
        sensorCollectionStateManagerRepository.clearManager(userId);
    }

    private void saveSelectedSensors(List<SensorType> sensors) {
        sensorCollectionStateManagerRepository.setSelectedSensors(sensors);
    }

    private void loadSelectedSensors() {
        sensorCollectionStateManagerRepository.getSelectedSensors(new RepositoryCallback<>() {
            @Override
            public void onSuccess(List<SensorType> loadedSelectedSensors) {
                recordingState.setSelectedSensors(loadedSelectedSensors);
                recordingState.setAllToggledOn(loadedSelectedSensors.size() == SensorType.values().length);
            }

            @Override
            public void onFailure(Throwable error) {
                recordingState.setSelectedSensors(new ArrayList<>());
                recordingState.setAllToggledOn(false);
            }
        });
    }
}
