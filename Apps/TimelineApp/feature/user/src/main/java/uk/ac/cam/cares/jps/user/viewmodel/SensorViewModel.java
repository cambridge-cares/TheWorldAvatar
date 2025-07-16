package uk.ac.cam.cares.jps.user.viewmodel;

import android.content.Context;


import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.user.R;
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
    private final List<SensorItem> sensorItems;


    private final MutableLiveData<Boolean> _isRecording = new MutableLiveData<>();
    public final LiveData<Boolean> isRecording = _isRecording;
    private final MutableLiveData<Boolean> _hasAccountError = new MutableLiveData<>();
    private final LiveData<Boolean> hasAccountError = _hasAccountError;

    /**
     * Constructor of the class. Instantiation is done with ViewProvider and dependency injection
     *
     * @param repository                             SensorRepository object
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
        this.selectedSensors.setValue(new ArrayList<>());
        this.sensorCollectionStateManagerRepository = sensorCollectionStateManagerRepository;
        this.userPhoneRepository = userPhoneRepository;
        this.sensorItems = new ArrayList<>();

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
        loadSelectedSensors();
    }

    public void loadSensorItems() {
        // initialize sensor items and post value to LiveData
        List<SensorItem> items = new ArrayList<>();
        items.add(new SensorItem("Accelerometer", R.string.sensor_description_accelerometer, SensorType.ACCELEROMETER));
        items.add(new SensorItem("Gyroscope", R.string.sensor_description_gyroscope, SensorType.GYROSCOPE));
        items.add(new SensorItem("Magnetometer", R.string.sensor_description_magnetometer, SensorType.MAGNETOMETER));
        items.add(new SensorItem("Light", R.string.sensor_description_light, SensorType.LIGHT));
        items.add(new SensorItem("Humidity", R.string.sensor_description_humidity, SensorType.HUMIDITY));
        items.add(new SensorItem("Pressure", R.string.sensor_description_pressure, SensorType.PRESSURE));
        items.add(new SensorItem("Gravity", R.string.sensor_description_gravity, SensorType.GRAVITY));
        items.add(new SensorItem("Location", R.string.sensor_description_location, SensorType.LOCATION));
        items.add(new SensorItem("Microphone", R.string.sensor_description_microphone, SensorType.SOUND));
        items.add(new SensorItem("Activity", R.string.sensor_description_activity, SensorType.ACTIVITY));


        sensorItems.addAll(items);
    }


    /**
     * Toggles the selected sensors for recording.
     *
     * @param sensorItem the sensor selected to be recorded
     */
    public void toggleSensor(SensorType sensorItem) {
        if (Boolean.TRUE.equals(_isRecording.getValue())) {
            LOGGER.warn("Recording is in progress.");
            return;
        }

        List<SensorType> currentSelectedSensors = new ArrayList<>(Objects.requireNonNull(selectedSensors.getValue()));
        if (currentSelectedSensors.contains(sensorItem)) {
            currentSelectedSensors.remove(sensorItem);
        } else {
            currentSelectedSensors.add(sensorItem);
        }

        selectedSensors.setValue(currentSelectedSensors);
    }

    public LiveData<List<SensorType>> getSelectedSensors() {
        return selectedSensors;
    }

    /**
     * Start recording
     */
    public void startRecording() {
        List<SensorType> sensorsToRecord = selectedSensors.getValue();
        LOGGER.info("Attempting to start recording. Sensors to record: " + sensorsToRecord);

        if (sensorsToRecord != null && !sensorsToRecord.isEmpty()) {
            sensorRepository.startRecording(sensorsToRecord, new RepositoryCallback<>() {
                @Override
                public void onSuccess(Boolean result) {
                    _isRecording.setValue(result);
                    LOGGER.info("Recording successfully started.");
                }

                @Override
                public void onFailure(Throwable error) {
                    _hasAccountError.setValue(true);
                    _isRecording.setValue(false);
                    LOGGER.error("Recording failed to start: " + error.getMessage());
                }
            });
        } else {
            LOGGER.warn("startRecording() called but no sensors are selected. Aborting.");
            _hasAccountError.setValue(true);
            _isRecording.setValue(false);
        }
    }

    /**
     * Stop recording
     */
    public void stopRecording() {
        sensorRepository.stopRecording();
        _isRecording.setValue(false);
        toggleAllSensors(false);
    }

    public void toggleRecording() {
        if (_isRecording.getValue() != null && _isRecording.getValue()) {
            stopRecording();
        } else {
            startRecording();
        }
    }


    /**
     * Checks the current recording status by verifying if a task with a given ID is running and updates the UI accordingly.
     *
     * @param context The context in which this method is called. It is used to check the status of the service
     *                associated with the recording task.
     *                This method retrieves the task ID from the sensor collection state manager repository. It then checks whether
     *                the task is currently running by calling isTaskRunning. Based on the result, it updates
     *                the `_isRecording` LiveData, which in turn triggers the UI to update the recording status.
     *                If the task ID retrieval fails, the `_isRecording` LiveData is set to `false`, ensuring that the UI reflects that no
     *                recording is in progress.
     */
    public void checkRecordingStatusAndUpdateUI(Context context) {
        sensorCollectionStateManagerRepository.getTaskId(new RepositoryCallback<>() {
            @Override
            public void onSuccess(String taskId) {
                if (sensorRepository.isTaskRunning(taskId)) {
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
        userPhoneRepository.registerAppToUser(new RepositoryCallback<>() {
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
     *
     * @param toggle boolean t/f value which denotes if a sensor has or has not been toggled
     */
    public void toggleAllSensors(boolean toggle) {
        if (Boolean.TRUE.equals(_isRecording.getValue())) {
            LOGGER.warn("Recording is active.");
            return;
        }

        List<SensorType> updatedSensorTypes = new ArrayList<>();
        for (SensorItem item : sensorItems) {
            item.setToggled(toggle);
            if (toggle) {
                updatedSensorTypes.add(item.getSensorType());
            }
        }
        selectedSensors.setValue(updatedSensorTypes);
        allToggledOn.setValue(toggle);
    }


    public List<SensorItem> getSensorItems() {
        return sensorItems;
    }

    private void loadSelectedSensors() {
        sensorCollectionStateManagerRepository.getSelectedSensors(new RepositoryCallback<>() {
            @Override
            public void onSuccess(List<SensorType> loadedSelectedSensors) {
                selectedSensors.setValue(loadedSelectedSensors);

                for (SensorType sensorType : loadedSelectedSensors) {
                    for (SensorItem item : sensorItems) {
                        if (item.getSensorType() == sensorType) {
                            item.setToggled(true);
                            break;
                        }
                    }
                }

                allToggledOn.setValue(loadedSelectedSensors.size() == sensorItems.size());
            }

            @Override
            public void onFailure(Throwable error) {
                selectedSensors.setValue(new ArrayList<>());
                allToggledOn.setValue(false);
                LOGGER.error("Failed to load selected sensors: " + error.getMessage());
            }
        });
    }


}
 