package uk.ac.cam.cares.jps.sensor.data;

import com.android.volley.Response;

import uk.ac.cam.cares.jps.sensor.source.network.UserPhoneNetworkSource;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

/**
 * A repository level component that provide UserPhoneNetworkSource access to UI level.
 * Functionalities:
 * 1. register device id to user id
 */
public class UserPhoneRepository {
    UserPhoneNetworkSource userPhoneNetworkSource;
    SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository;

    public UserPhoneRepository(
            UserPhoneNetworkSource userPhoneNetworkSource,
            SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository) {

            this.userPhoneNetworkSource = userPhoneNetworkSource;
            this.sensorCollectionStateManagerRepository = sensorCollectionStateManagerRepository;
    }

    public void registerAppToUser(RepositoryCallback<Boolean> callback) {
        sensorCollectionStateManagerRepository.getUserId(new RepositoryCallback<>() {
            @Override
            public void onSuccess(String userId) {
                sensorCollectionStateManagerRepository.getDeviceId(new RepositoryCallback<>() {
                    @Override
                    public void onSuccess(String deviceId) {
                        userPhoneNetworkSource.registerAppToUser(userId,
                                deviceId,
                                (Response.Listener<Boolean>) aBoolean -> callback.onSuccess(true),
                                callback::onFailure); // onFailure is not triggered
                    }

                    @Override
                    public void onFailure(Throwable error) {
                        callback.onFailure(error);
                    }
                });
            }

            @Override
            public void onFailure(Throwable error) {
                callback.onFailure(error);
            }
        });
    }

}
