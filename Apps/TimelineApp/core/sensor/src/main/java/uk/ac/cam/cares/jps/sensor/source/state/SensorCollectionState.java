package uk.ac.cam.cares.jps.sensor.source.state;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;

/**
 * A data class for sensor collection state
 * It contains the user information for sensor data upload to server and the current recording state.
 */
public class SensorCollectionState {
    private String userId;
    private String deviceId;
    private Boolean recordingState;
    private String taskId;
    private List<SensorType> selectedSensors;

    public SensorCollectionState(String userId, String deviceId, Boolean recordingState, String taskId) {
        this.userId = userId;
        this.deviceId = deviceId;
        this.recordingState = recordingState;
        this.taskId = taskId;
        this.selectedSensors = new ArrayList<>();
    }

    public String getUserId() {
        return userId;
    }

    public String getDeviceId() {
        return deviceId;
    }

    public Boolean getRecordingState() {
        return recordingState;
    }

    public void setRecordingState(Boolean isRecording) {
        this.recordingState = isRecording;
    }

    public String getTaskId() {
        return taskId;
    }

    public void setTaskId(String taskId) {
        this.taskId = taskId;
    }

    public List<SensorType> getSelectedSensors() {
        return selectedSensors;
    }

    public void setSelectedSensors(List<SensorType> selectedSensors) {
        this.selectedSensors = selectedSensors;
    }
}
