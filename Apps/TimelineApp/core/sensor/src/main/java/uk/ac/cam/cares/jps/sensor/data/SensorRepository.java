package uk.ac.cam.cares.jps.sensor.data;

import android.content.Context;
import android.content.Intent;

import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.sensor.SensorService;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.sensor.source.network.UserPhoneNetworkSource;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

/**
 * A repository level component that provides control of sensor collection to UI level component
 * Functionalities:
 * 1. Start/stop recording in service
 * @see SensorService SensorService
 */
public class SensorRepository {
    SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository;
    Logger LOGGER = Logger.getLogger(SensorRepository.class);
    Intent serviceIntent;
    Context context;

    /**
     * Constructor of the class. The instantiation is handled by dependency injection.
     * @param sensorCollectionStateManagerRepository SensorCollectionStateManagerRepository object
     */
    public SensorRepository(Context applicationContext,
                            SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository) {
        this.context = applicationContext;
        this.sensorCollectionStateManagerRepository = sensorCollectionStateManagerRepository;

        serviceIntent = new Intent(context, SensorService.class);
    }

    /**
     * Start foreground task to record sensor data
     * @param callback callback
     */
    public void startRecording(List<SensorType> selectedSensorTypes, RepositoryCallback<Boolean> callback) {
        LOGGER.info("start recording with selected sensors");
        sensorCollectionStateManagerRepository.getDeviceId(new RepositoryCallback<>() {
            @Override
            public void onSuccess(String result) {
                serviceIntent.putExtra("deviceId", result);
                serviceIntent.putParcelableArrayListExtra("selectedSensors", new ArrayList<>(selectedSensorTypes));
                context.startService(serviceIntent);
                sensorCollectionStateManagerRepository.setRecordingState(true);
                callback.onSuccess(true);
            }

            @Override
            public void onFailure(Throwable error) {
                callback.onFailure(error);
            }
        });
    }

    /**
     * Stop the data collection foreground task
     */
    public void stopRecording() {
        LOGGER.info("stop recording");
        context.stopService(serviceIntent);
        sensorCollectionStateManagerRepository.setRecordingState(false);
    }
}
