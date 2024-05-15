package uk.ac.cam.cares.jps.sensor;

import android.content.Context;
import android.content.Intent;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.login.AccountException;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.RepositoryCallback;
import uk.ac.cam.cares.jps.login.User;

public class SensorRepository {
    SensorCollectionStateManager sensorCollectionStateManager;
    UserPhoneNetworkSource userPhoneNetworkSource;
    LoginRepository loginRepository;
    Logger LOGGER = Logger.getLogger(SensorRepository.class);
    Intent serviceIntent;
    Context context;

    public SensorRepository(Context applicationContext,
                            SensorCollectionStateManager sensorCollectionStateManager,
                            UserPhoneNetworkSource userPhoneNetworkSource,
                            LoginRepository loginRepository) {
        this.context = applicationContext;
        this.sensorCollectionStateManager = sensorCollectionStateManager;
        this.userPhoneNetworkSource = userPhoneNetworkSource;
        this.loginRepository = loginRepository;

        serviceIntent = new Intent(context, SensorService.class);
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

    public void registerAppToUser() throws AccountException {
        checkOrInitSensorCollectionStateManagerWithLoginInfo();
        userPhoneNetworkSource.registerAppToUser(sensorCollectionStateManager.getUserId(), sensorCollectionStateManager.getDeviceId(),
                volleyError -> {
                    // todo
                });
    }

    public void startRecording() throws AccountException {
        LOGGER.info("start recording");
        checkOrInitSensorCollectionStateManagerWithLoginInfo();
        serviceIntent.putExtra("deviceId", sensorCollectionStateManager.getDeviceId());
        context.startService(serviceIntent);
        sensorCollectionStateManager.setRecordingState(true);
    }

    public void stopRecording() {
        LOGGER.info("stop recording");
        context.stopService(serviceIntent);
        sensorCollectionStateManager.setRecordingState(false);
    }

    public void clearManagers(String userId) {
        stopRecording();
        sensorCollectionStateManager.clearState(userId);
    }

    public boolean getRecordingState() throws AccountException {
        checkOrInitSensorCollectionStateManagerWithLoginInfo();
        return sensorCollectionStateManager.getRecordingState();
    }
}
