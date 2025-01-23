package uk.ac.cam.cares.jps.sensor.data;

import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;

import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.sensor.SensorService;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

/**
 * A repository level component that provides control of sensor collection to UI level component
 * Functionalities:
 * 1. Start/stop recording in service
 * @see SensorService SensorService
 */
public class SensorRepository {
    SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository;
    SensorLocalSource sensorLocalSource;
    Logger LOGGER = Logger.getLogger(SensorRepository.class);
    Intent serviceIntent;
    Context context;

    /**
     * Constructor of the class. The instantiation is handled by dependency injection.
     * @param sensorCollectionStateManagerRepository SensorCollectionStateManagerRepository object
     */
    public SensorRepository(Context applicationContext,
                            SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository,
                            SensorLocalSource sensorLocalSource) {
        this.context = applicationContext;
        this.sensorCollectionStateManagerRepository = sensorCollectionStateManagerRepository;
        this.sensorLocalSource = sensorLocalSource;

        serviceIntent = new Intent(context, SensorService.class);
    }

    /**
     * Start foreground task to record sensor data
     * @param callback callback
     */
    public void startRecording(List<SensorType> selectedSensorTypes, RepositoryCallback<Boolean> callback) {
        LOGGER.info("start recording with selected sensors");
        prepareAndStartCollection(selectedSensorTypes, callback);
    }

    private void prepareAndStartCollection(List<SensorType> selectedSensorTypes, RepositoryCallback<Boolean> callback) {
        sensorCollectionStateManagerRepository.getHashedFileName(new RepositoryCallback<>() {
            @Override
            public void onSuccess(String hashedFileName) {
                sensorLocalSource.initAppDataBase(hashedFileName);
                LOGGER.info("successfully init local database");

                startSensorCollection(selectedSensorTypes, callback);
            }

            @Override
            public void onFailure(Throwable error) {
                callback.onFailure(error);
            }
        });
    }

    private void startSensorCollection(List<SensorType> selectedSensorTypes, RepositoryCallback<Boolean> callback) {
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

    /**
     * Checks if a task with the given task ID is currently running.
     *
     * @param taskId  The unique identifier for the task that needs to be checked.
     * @return `true` if the task with the given ID is running, `false` otherwise.
     *
     * This method first checks if the task ID is valid (i.e., not null or empty). If so, the method proceeds
     * to check if the service associated with the task is currently running by calling {@link #isServiceRunning(Class)}.
     */
    public boolean isTaskRunning(String taskId) {
        if (taskId == null || taskId.isEmpty()) {
            return false;
        }
        return isServiceRunning(SensorService.class);
    }


    /**
     * Checks if a specified service is currently running in the background.
     *
     * @param serviceClass The class of the service that needs to be checked.
     * @return `true` if the specified service is currently running, `false` otherwise.
     *
     * This method uses the {@link ActivityManager} to query the system's running services. It iterates through the list of
     * running services and checks if the specified service class is among them. If the service is found, the method returns `true`.
     * Otherwise, it returns `false`.
     */
    public boolean isServiceRunning(Class<?> serviceClass) {
        ActivityManager manager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
        for (ActivityManager.RunningServiceInfo service : manager.getRunningServices(Integer.MAX_VALUE)) {
            if (SensorService.class.getName().equals(service.service.getClassName())) {
                return true;
            }
        }
        return false;
    }
}
