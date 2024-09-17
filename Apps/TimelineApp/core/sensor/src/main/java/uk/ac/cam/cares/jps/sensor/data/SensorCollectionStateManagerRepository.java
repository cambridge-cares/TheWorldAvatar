package uk.ac.cam.cares.jps.sensor.data;

import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.User;
import uk.ac.cam.cares.jps.sensor.source.state.SensorCollectionStateException;
import uk.ac.cam.cares.jps.sensor.source.state.SensorCollectionStateManager;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

/**
 * A repository level component which provide SensorCollectionStateManager functions to higher level components or other repositories.
 * Functionalities:
 * 1. Run provided function (as Callback) with requested SensorCollectionState information (eg. userId, deviceId, isRecording)
 * 2. Clean SensorCollectionStateManager
 */
public class SensorCollectionStateManagerRepository {
    LoginRepository loginRepository;
    SensorCollectionStateManager sensorCollectionStateManager;

    private interface FunctionRunWithSensorCollectionState<E> {
        E get() throws SensorCollectionStateException;
    }

    /**
     * Constructor of the class. The instantiation is handled by dependency injection.
     * @param sensorCollectionStateManager SensorCollectionStateManager object
     * @param loginRepository LoginRepository object
     */
    public SensorCollectionStateManagerRepository(
            SensorCollectionStateManager sensorCollectionStateManager,
            LoginRepository loginRepository) {
        this.loginRepository = loginRepository;
        this.sensorCollectionStateManager = sensorCollectionStateManager;
    }

    /**
     * Get or init SensorCollectionState if the in memory object doesn't exists and run functions (in callback) with the requested SensorCollectionState information.
     * During init of the SensorCollectionState, this function pulls userId directly from LoginRepository, which will handle the query and management of userId.
     * This function is created to handle case when SensorCollectionState isn't available in memory.
     * @param callback Callback which encapsulate the intended operation with a specific SensorCollectionState information
     * @param function Function to get the specified SensorCollectionState information
     * @param <T>
     */
    private <T> void checkOrInitSensorCollectionStateManagerWithLoginInfo(RepositoryCallback<T> callback, FunctionRunWithSensorCollectionState<T> function) {
        if (sensorCollectionStateManager.getSensorCollectionState() != null) {
            try {
                callback.onSuccess(function.get());
            } catch (SensorCollectionStateException e) {
                callback.onFailure(e);
            }
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

    /**
     * Run provided functions (in Callback) with userId
     * @param callback
     */
    public void getUserId(RepositoryCallback<String> callback) {
        checkOrInitSensorCollectionStateManagerWithLoginInfo(callback, (FunctionRunWithSensorCollectionState<String>) () -> sensorCollectionStateManager.getUserId());
    }
    /**
     * Run provided functions (in Callback) with deviceId
     * @param callback
     */
    public void getDeviceId(RepositoryCallback<String> callback) {
        checkOrInitSensorCollectionStateManagerWithLoginInfo(callback, (FunctionRunWithSensorCollectionState<String>) () -> sensorCollectionStateManager.getDeviceId());
    }

    /**
     * Run provided functions (in Callback) with isRecording
     * @param callback
     */
    public void getRecordingStatus(RepositoryCallback<Boolean> callback) {
        checkOrInitSensorCollectionStateManagerWithLoginInfo(callback, (FunctionRunWithSensorCollectionState<Boolean>) () -> sensorCollectionStateManager.getRecordingState());
    }

    /**
     * Set recording state
     * @param isRecording
     */
    public void setRecordingState(boolean isRecording) {
        sensorCollectionStateManager.setRecordingState(isRecording);
    }

    /**
     * Clear manager and the in memory SensorCollectionState
     * @param userId
     */
    public void clearManager(String userId) {
        sensorCollectionStateManager.clearState(userId);
    }
}
