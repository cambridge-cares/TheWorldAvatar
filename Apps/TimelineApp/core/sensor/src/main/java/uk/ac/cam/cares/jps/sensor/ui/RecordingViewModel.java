package uk.ac.cam.cares.jps.sensor.ui;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.Logger;

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

    protected SensorRepository sensorRepository;
    protected SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository;

    protected RecordingState recordingState;
    protected Logger LOGGER = Logger.getLogger(RecordingViewModel.class);

    public RecordingViewModel() {}

    @Inject
    public RecordingViewModel(
            SensorRepository sensorRepository,
            SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository,
            RecordingState recordingState
    ) {
        this.sensorRepository = sensorRepository;
        this.sensorCollectionStateManagerRepository = sensorCollectionStateManagerRepository;
        this.recordingState = recordingState;

        checkRecordingStatusAndUpdateUI();
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
        LOGGER.info("Attempting to start recording. Sensors to record: " + sensorsToRecord);

        if (sensorsToRecord != null && !sensorsToRecord.isEmpty()) {
            sensorRepository.startRecording(sensorsToRecord, new RepositoryCallback<>() {
                @Override
                public void onSuccess(Boolean result) {
                    recordingState.postIsRecording(result);
                    recordingState.postHasAccountError(false);
                    LOGGER.info("Recording successfully started.");
                }

                @Override
                public void onFailure(Throwable error) {
                    recordingState.postHasAccountError(true);
                    recordingState.postIsRecording(false);
                    LOGGER.error("Recording failed to start: " + error.getMessage());
                }
            });
        } else {
            LOGGER.warn("startRecording() called but no sensors are selected. Aborting.");
            recordingState.setIsRecording(false);
        }
    }

    public void stopRecording() {
        sensorRepository.stopRecording();
        recordingState.setIsRecording(false);
        sensorCollectionStateManagerRepository.setTaskId("");
    }

    public void toggleAllSensors(boolean toggle) {
        if (Boolean.TRUE.equals(recordingState.getIsRecording().getValue())) {
            LOGGER.warn("Recording is active.");
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

    protected void saveSelectedSensors(List<SensorType> sensors) {
        sensorCollectionStateManagerRepository.setSelectedSensors(sensors);
    }

    protected void loadSelectedSensors() {
        sensorCollectionStateManagerRepository.getSelectedSensors(new RepositoryCallback<>() {
            @Override
            public void onSuccess(List<SensorType> loadedSelectedSensors) {
                recordingState.postSelectedSensors(loadedSelectedSensors);
                recordingState.setAllToggledOn(loadedSelectedSensors.size() == SensorType.values().length);
            }

            @Override
            public void onFailure(Throwable error) {
                recordingState.postSelectedSensors(new ArrayList<>());
                recordingState.setAllToggledOn(false);
            }
        });
    }

    public void checkRecordingStatusAndUpdateUI() {
        sensorCollectionStateManagerRepository.getTaskId(new RepositoryCallback<>() {
            @Override
            public void onSuccess(String taskId) {
                boolean isRecording = sensorRepository.isTaskRunning(taskId);
                recordingState.postIsRecording(isRecording);
                sensorCollectionStateManagerRepository.setRecordingState(isRecording);
            }

            @Override
            public void onFailure(Throwable error) {
                recordingState.postIsRecording(false);
            }
        });
    }
}
