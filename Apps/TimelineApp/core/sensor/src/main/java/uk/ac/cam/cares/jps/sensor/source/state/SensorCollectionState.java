package uk.ac.cam.cares.jps.sensor.source.state;

/**
 * A data class for sensor collection state
 * It contains the user information for sensor data upload to server and the current recording state.
 */
public class SensorCollectionState {
    private String userId;
    private String deviceId;
    private Boolean recordingState;
    private String taskId;

    public SensorCollectionState(String userId, String deviceId, Boolean recordingState, String taskId) {
        this.userId = userId;
        this.deviceId = deviceId;
        this.recordingState = recordingState;
        this.taskId = taskId;
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
}
