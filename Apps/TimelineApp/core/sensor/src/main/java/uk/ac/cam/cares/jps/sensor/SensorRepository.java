package uk.ac.cam.cares.jps.sensor;

import android.content.Context;
import android.content.Intent;

import com.android.volley.Response;
import com.android.volley.VolleyError;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.RepositoryCallback;
import uk.ac.cam.cares.jps.login.User;

public class SensorRepository {
    SensorNetworkSource sensorNetworkSource;
    SensorService sensorService;
    LoginRepository loginRepository;
    Logger LOGGER = Logger.getLogger(SensorRepository.class);
    Intent serviceIntent;
    Context context;

    public SensorRepository(Context applicationContext,
                            SensorNetworkSource sensorNetworkSource,
                            SensorService sensorService,
                            LoginRepository loginRepository) {
        this.context = applicationContext;
        this.sensorNetworkSource = sensorNetworkSource;
        this.sensorService = sensorService;
        this.loginRepository = loginRepository;

        serviceIntent = new Intent(context, sensorService.getClass());
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
        context.startService(serviceIntent);
    }

    public void stopRecording() {
        LOGGER.info("stop recording");
        sensorService.stopService();
    }

    public void clearManagers() {
        sensorNetworkSource.clearManagers();
    }
}
