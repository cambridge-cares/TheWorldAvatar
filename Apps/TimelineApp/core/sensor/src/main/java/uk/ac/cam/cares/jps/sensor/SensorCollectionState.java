package uk.ac.cam.cares.jps.sensor;

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
