package uk.ac.cam.cares.jps.user.viewmodel;

import android.app.ActivityManager;
import android.content.Context;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.sensor.SensorService;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorHandler;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.user.SensorItem;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;
import uk.ac.cam.cares.jps.sensor.data.SensorCollectionStateManagerRepository;
import uk.ac.cam.cares.jps.sensor.data.SensorRepository;
import uk.ac.cam.cares.jps.sensor.data.UserPhoneRepository;

/**
 * ViewModel that manages sensor recording related states and functions
 */
@HiltViewModel
public class SensorViewModel extends ViewModel {
    private static final Logger LOGGER = LogManager.getLogger(SensorViewModel.class);
    private final SensorRepository sensorRepository;
    private final SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository;
    private final UserPhoneRepository userPhoneRepository;
    private final MutableLiveData<List<SensorType>> selectedSensors = new MutableLiveData<>();
    private final MutableLiveData<Boolean> allToggledOn = new MutableLiveData<>(false);
    private final MutableLiveData<List<SensorItem>> sensorItems = new MutableLiveData<>();


    private final MutableLiveData<Boolean> _isRecording = new MutableLiveData<>();
    public final LiveData<Boolean> isRecording = _isRecording;
    private final MutableLiveData<Boolean> _hasAccountError = new MutableLiveData<>();
    private final LiveData<Boolean> hasAccountError = _hasAccountError;

    /**
     * Constructor of the class. Instantiation is done with ViewProvider and dependency injection
     * @param repository SensorRepository object
     * @param sensorCollectionStateManagerRepository SensorCollectionStateManagerRepository object
     * @param userPhoneRepository
     */
    @Inject
    SensorViewModel(
            SensorRepository repository,
            SensorCollectionStateManagerRepository sensorCollectionStateManagerRepository,
            UserPhoneRepository userPhoneRepository

    ) {
        BasicConfigurator.configure();
        this.sensorRepository = repository;
        selectedSensors.setValue(new ArrayList<>());
        this.sensorCollectionStateManagerRepository = sensorCollectionStateManagerRepository;
        this.userPhoneRepository = userPhoneRepository;

        sensorCollectionStateManagerRepository.getRecordingStatus(new RepositoryCallback<>() {
            @Override
            public void onSuccess(Boolean result) {
                _isRecording.setValue(result);
            }

            @Override
            public void onFailure(Throwable error) {
                _hasAccountError.setValue(true);
            }
        });
        loadSensorItems();
    }

    public void loadSensorItems() {
        // initialize sensor items and post value to LiveData
        List<SensorItem> items = new ArrayList<>();
        items.add(new SensorItem("Accelerometer", "Measures acceleration.", SensorType.ACCELEROMETER));
        items.add(new SensorItem("Gyroscope", "Tracks rotation rate.", SensorType.GYROSCOPE));
        items.add(new SensorItem("Magnetometer", "Detects magnetic fields.", SensorType.MAGNETOMETER));
        items.add(new SensorItem("Light", "Senses light levels.", SensorType.LIGHT));
        items.add(new SensorItem("Humidity", "Monitors air moisture.", SensorType.HUMIDITY));
        items.add(new SensorItem("Pressure", "Gauges atmospheric pressure.", SensorType.PRESSURE));
        items.add(new SensorItem("Gravity", "Detects gravity vector.", SensorType.GRAVITY));
        items.add(new SensorItem("Location", "Tracks GPS position.", SensorType.LOCATION));
        items.add(new SensorItem("Microphone", "Captures sound levels.", SensorType.SOUND));

        sensorItems.setValue(items);
    }


    /**
     * Toggles the selected sensors for recording.
     * @param sensorItem the sensor selected to be recorded
     */
    public void toggleSensor(SensorType sensorItem) {
        List<SensorType> currentSelectedSensors = new ArrayList<>(selectedSensors.getValue());
        if (currentSelectedSensors.contains(sensorItem)) {
            currentSelectedSensors.remove(sensorItem);  // Remove sensor if already toggled
        } else {
            currentSelectedSensors.add(sensorItem);  // Add sensor if toggled on
        }
        selectedSensors.setValue(currentSelectedSensors);
    }


    public LiveData<List<SensorType>> getSelectedSensors() {
        return selectedSensors;
    }


    public LiveData<Boolean> isRecording() {
        return isRecording;
    }

    public LiveData<Boolean> isAllToggledOn() {
        return allToggledOn;
    }



    /**
     * Start recording
     */
    public void startRecording() {
        List<SensorType> sensorsToRecord = selectedSensors.getValue();
        if (sensorsToRecord != null && !sensorsToRecord.isEmpty()) {
        sensorRepository.startRecording(sensorsToRecord, new RepositoryCallback<Boolean>() {
            @Override
            public void onSuccess(Boolean result) {
                _isRecording.setValue(result);
            }

            @Override
            public void onFailure(Throwable error) {
                _hasAccountError.setValue(true);
                _isRecording.setValue(false);
            }
        });
    }
    }


    /**
     * Stop recording
     */
    public void stopRecording() {
        sensorRepository.stopRecording();
        _isRecording.setValue(false);
        sensorCollectionStateManagerRepository.setTaskId(null);
    }


    /**
     * Checks the current recording status by verifying if a task with a given ID is running and updates the UI accordingly.
     *
     * @param context The context in which this method is called. It is used to check the status of the service
     *                associated with the recording task.
     *
     * This method retrieves the task ID from the sensor collection state manager repository. It then checks whether
     * the task is currently running by calling {@link #isTaskRunning(String, Context)}. Based on the result, it updates
     * the `_isRecording` LiveData, which in turn triggers the UI to update the recording status.
     *
     * If the task ID retrieval fails, the `_isRecording` LiveData is set to `false`, ensuring that the UI reflects that no
     * recording is in progress.
     */
    public void checkRecordingStatusAndUpdateUI(Context context) {
        sensorCollectionStateManagerRepository.getTaskId(new RepositoryCallback<String>() {
            @Override
            public void onSuccess(String taskId) {
                if (isTaskRunning(taskId, context)) {
                    _isRecording.setValue(true);
                } else {
                    _isRecording.setValue(false);
                }
            }

            @Override
            public void onFailure(Throwable error) {
                _isRecording.setValue(false);
            }
        });
    }

    /**
     * Checks if a task with the given task ID is currently running.
     *
     * @param taskId  The unique identifier for the task that needs to be checked.
     * @param context The context in which this method is called. It is used to determine the status of the service
     *                associated with the task.
     * @return `true` if the task with the given ID is running, `false` otherwise.
     *
     * This method first checks if the task ID is valid (i.e., not null or empty). If so, the method proceeds
     * to check if the service associated with the task is currently running by calling {@link #isServiceRunning(Class, Context)}.
     */
    private boolean isTaskRunning(String taskId, Context context) {
        if (taskId == null || taskId.isEmpty()) {
            return false;
        }
        return isServiceRunning(SensorService.class, context);
    }

    /**
     * Checks if a specified service is currently running in the background.
     *
     * @param serviceClass The class of the service that needs to be checked.
     * @param context      The context in which this method is called. It is used to access the system's activity manager
     *                     to query the running services.
     * @return `true` if the specified service is currently running, `false` otherwise.
     *
     * This method uses the {@link ActivityManager} to query the system's running services. It iterates through the list of
     * running services and checks if the specified service class is among them. If the service is found, the method returns `true`.
     * Otherwise, it returns `false`.
     */
    private boolean isServiceRunning(Class<?> serviceClass, Context context) {
        ActivityManager manager = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
        for (ActivityManager.RunningServiceInfo service : manager.getRunningServices(Integer.MAX_VALUE)) {
            if (serviceClass.getName().equals(service.service.getClassName())) {
                return true;
            }
        }
        return false;
    }


    public LiveData<Boolean> getIsRecording() {
        return isRecording;
    }

    public LiveData<Boolean> getHasAccountError() {
        return hasAccountError;
    }

    public void clearManagers(String userId) {
        sensorCollectionStateManagerRepository.clearManager(userId);
    }

    /**
     * Register phone to user
     */
    public void registerPhoneToUser() {
        userPhoneRepository.registerAppToUser(new RepositoryCallback<Boolean>() {
            @Override
            public void onSuccess(Boolean result) {
                // do nothing
            }

            @Override
            public void onFailure(Throwable error) {
                _hasAccountError.setValue(true);
            }
        });
    }

    public LiveData<Boolean> getAllToggledOn() {
        return allToggledOn;
    }

    /**
     * Toggles all the sensors when the user selects to do so.
     * @param toggle boolean t/f value which denotes if a sensor has or has not been toggled
     */
    public void toggleAllSensors(boolean toggle) {
        List<SensorType> updatedSensorTypes = new ArrayList<>();
        for (SensorItem item : sensorItems.getValue()) {
            item.setToggled(toggle);
            if (toggle) {
                updatedSensorTypes.add(item.getSensorType());
            }
        }
        selectedSensors.setValue(updatedSensorTypes);
        allToggledOn.setValue(toggle);
    }

    public LiveData<List<SensorItem>> getSensorItems() {
        return sensorItems;
    }


}
