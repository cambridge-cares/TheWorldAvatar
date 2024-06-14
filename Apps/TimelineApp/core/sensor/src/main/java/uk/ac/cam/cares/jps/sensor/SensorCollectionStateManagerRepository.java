package uk.ac.cam.cares.jps.sensor;

import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.User;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

public class SensorCollectionStateManagerRepository {
    LoginRepository loginRepository;
    SensorCollectionStateManager sensorCollectionStateManager;

    private interface FunctionRunWithSensorCollectionState<E> {
        E get() throws SensorCollectionStateException;
    }

    public SensorCollectionStateManagerRepository(
            SensorCollectionStateManager sensorCollectionStateManager,
            LoginRepository loginRepository) {
        this.loginRepository = loginRepository;
        this.sensorCollectionStateManager = sensorCollectionStateManager;
    }

    private <T> void checkOrInitSensorCollectionStateManagerWithLoginInfo(RepositoryCallback<T> callback, FunctionRunWithSensorCollectionState<T> function) {
        if (sensorCollectionStateManager.getSensorCollectionState() != null) {
            return;
        }

        loginRepository.getUserInfo(new RepositoryCallback<>() {
            @Override
            public void onSuccess(User result) {
                sensorCollectionStateManager.initSensorCollectionState(result.getId());
                try {
                    callback.onSuccess(function.get());
                } catch (SensorCollectionStateException e) {
                    callback.onFailure(e);
                }
            }

            @Override
            public void onFailure(Throwable error) {
                callback.onFailure(error);
            }
        });

    }

    public void getUserId(RepositoryCallback<String> callback) {
        checkOrInitSensorCollectionStateManagerWithLoginInfo(callback, (FunctionRunWithSensorCollectionState<String>) () -> sensorCollectionStateManager.getUserId());
    }

    public void getDeviceId(RepositoryCallback<String> callback) {
        checkOrInitSensorCollectionStateManagerWithLoginInfo(callback, (FunctionRunWithSensorCollectionState<String>) () -> sensorCollectionStateManager.getDeviceId());
    }

    public void getRecordingStatus(RepositoryCallback<Boolean> callback) {
        checkOrInitSensorCollectionStateManagerWithLoginInfo(callback, (FunctionRunWithSensorCollectionState<Boolean>) () -> sensorCollectionStateManager.getRecordingState());
    }

    public void setRecordingState(boolean isRecording) {
        sensorCollectionStateManager.setRecordingState(isRecording);
    }

    public void clearManager(String userId) {
        sensorCollectionStateManager.clearState(userId);
    }
}
