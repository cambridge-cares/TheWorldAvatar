package uk.ac.cam.cares.jps.sensor.source.state;

/**
 * A data class for sensor collection state
 * It contains the user information for sensor data upload to server and the current recording state.
 */
public class SensorCollectionState {
    private String userId;
    private String deviceId;
    private Boolean recordingState;

    public SensorCollectionState(String userId, String deviceId, Boolean recordingState) {
        this.userId = userId;
        this.deviceId = deviceId;
        this.recordingState = recordingState;
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
}
