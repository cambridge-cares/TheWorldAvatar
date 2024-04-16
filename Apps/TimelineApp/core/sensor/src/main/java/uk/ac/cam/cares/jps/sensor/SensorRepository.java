package uk.ac.cam.cares.jps.sensor;

import com.android.volley.Response;
import com.android.volley.VolleyError;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.RepositoryCallback;
import uk.ac.cam.cares.jps.login.User;

public class SensorRepository {
    SensorNetworkSource sensorNetworkSource;
    LoginRepository loginRepository;
    Logger LOGGER = Logger.getLogger(SensorRepository.class);

    public SensorRepository(SensorNetworkSource sensorNetworkSource, LoginRepository loginRepository) {
        this.sensorNetworkSource = sensorNetworkSource;
        this.loginRepository = loginRepository;
    }

    public void registerAppToUser() {
        loginRepository.getUserInfo(new RepositoryCallback<>() {
            @Override
            public void onSuccess(User result) {
                sensorNetworkSource.registerAppToUser(result.getId(), volleyError -> {
                    // todo
                });
            }

            @Override
            public void onFailure(Throwable error) {
                // todo
            }
        });
    }

    public void startRecording() {
        LOGGER.info("start recording");
        sensorNetworkSource.startDataCollection();
    }

    public void stopRecording() {
        LOGGER.info("stop recording");
        sensorNetworkSource.stopDataCollection();
    }
}
