package uk.ac.cam.cares.jps.user.viewmodel;


import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
import uk.ac.cam.cares.jps.sensor.ui.RecordingState;
import uk.ac.cam.cares.jps.sensor.ui.RecordingViewModel;
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
public class SensorViewModel extends RecordingViewModel {
    private final UserPhoneRepository userPhoneRepository;
    private final MutableLiveData<Boolean> allToggledOn = new MutableLiveData<>(false);
    private final List<SensorItem> sensorItems;

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
            UserPhoneRepository userPhoneRepository,
            RecordingState recordingState
    ) {
        super();
        BasicConfigurator.configure();

        this.sensorRepository = repository;
        this.sensorCollectionStateManagerRepository = sensorCollectionStateManagerRepository;
        this.recordingState = recordingState;
        this.LOGGER = LogManager.getLogger(SensorViewModel.class);

        this.userPhoneRepository = userPhoneRepository;
        this.sensorItems = new ArrayList<>();

        sensorCollectionStateManagerRepository.getIsRecording(new RepositoryCallback<>() {
            @Override
            public void onSuccess(Boolean result) {
                recordingState.setIsRecording(result);
            }

            @Override
            public void onFailure(Throwable error) {
                recordingState.setHasAccountError(true);
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
        if (Boolean.TRUE.equals(recordingState.getIsRecording().getValue())) {
            LOGGER.warn("Recording is in progress.");
            return;
        }

        List<SensorType> currentSelectedSensors = new ArrayList<>(Objects.requireNonNull(
                recordingState.getSelectedSensors().getValue()));
        if (currentSelectedSensors.contains(sensorItem)) {
            currentSelectedSensors.remove(sensorItem);
        } else {
            currentSelectedSensors.add(sensorItem);
        }

        recordingState.setSelectedSensors(currentSelectedSensors);
    }

    public void toggleRecording() {
        if (recordingState.getIsRecording().getValue() != null && recordingState.getIsRecording().getValue()) {
            stopRecording();
        } else {
            startRecording();
        }
    }


    /**
     * Checks the current recording status by verifying if a task with a given ID is running and updates the UI accordingly.
     */
    public void checkRecordingStatusAndUpdateUI() {
        sensorCollectionStateManagerRepository.getTaskId(new RepositoryCallback<>() {
            @Override
            public void onSuccess(String taskId) {
                recordingState.setIsRecording(sensorRepository.isTaskRunning(taskId));
            }

            @Override
            public void onFailure(Throwable error) {
                recordingState.setIsRecording(false);
            }
        });
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
                recordingState.setHasAccountError(true);
            }
        });
    }

    public LiveData<Boolean> getAllToggledOn() {
        return allToggledOn;
    }

    public List<SensorItem> getSensorItems() {
        return sensorItems;
    }

    @Override
    protected void loadSelectedSensors() {
        sensorCollectionStateManagerRepository.getSelectedSensors(new RepositoryCallback<>() {
            @Override
            public void onSuccess(List<SensorType> loadedSelectedSensors) {
                recordingState.setSelectedSensors(loadedSelectedSensors);

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
                recordingState.setSelectedSensors(new ArrayList<>());
                allToggledOn.setValue(false);
                LOGGER.error("Failed to load selected sensors: " + error.getMessage());
            }
        });
    }


}
 