package uk.ac.cam.cares.jps.sensor;

import uk.ac.cam.cares.jps.login.AccountException;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;
import uk.ac.cam.cares.jps.login.User;

public class SensorCollectionStateManagerRepository {
    LoginRepository loginRepository;
    SensorCollectionStateManager sensorCollectionStateManager;

    public SensorCollectionStateManagerRepository(
            SensorCollectionStateManager sensorCollectionStateManager,
            LoginRepository loginRepository) {
        this.loginRepository = loginRepository;
        this.sensorCollectionStateManager = sensorCollectionStateManager;
    }

    private void checkOrInitSensorCollectionStateManagerWithLoginInfo() {
        if (sensorCollectionStateManager.getSensorCollectionState() != null) {
            return;
        }

        loginRepository.getUserInfo(new RepositoryCallback<>() {
            @Override
            public void onSuccess(User result) {
                sensorCollectionStateManager.initSensorCollectionState(result.getId());
            }

            @Override
            public void onFailure(Throwable error) {
                // todo
            }
        });

    }

    public void getUserId(RepositoryCallback<String> callback) {
        try {
            checkOrInitSensorCollectionStateManagerWithLoginInfo();
            callback.onSuccess(sensorCollectionStateManager.getUserId());
        } catch (AccountException e) {
            callback.onFailure(e);
        }
    }

    public void getDeviceId(RepositoryCallback<String> callback) {
        try {
            checkOrInitSensorCollectionStateManagerWithLoginInfo();
            callback.onSuccess(sensorCollectionStateManager.getDeviceId());
        } catch (AccountException e) {
            callback.onFailure(e);
        }
    }

    public void getRecordingStatus(RepositoryCallback<Boolean> callback) {
        try {
            checkOrInitSensorCollectionStateManagerWithLoginInfo();
            callback.onSuccess(sensorCollectionStateManager.getRecordingState());
        } catch (AccountException e) {
            callback.onFailure(e);
        }
    }

    public void setRecordingState(boolean isRecording) {
        sensorCollectionStateManager.setRecordingState(isRecording);
    }

    public void clearManager(String userId) {
        sensorCollectionStateManager.clearState(userId);
    }
}
