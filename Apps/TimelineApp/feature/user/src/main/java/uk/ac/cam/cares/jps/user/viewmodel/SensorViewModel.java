package uk.ac.cam.cares.jps.user.viewmodel;

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
    private final LiveData<Boolean> isRecording = _isRecording;
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
        List<SensorItem> updatedItems = new ArrayList<>();
        for (SensorItem item : sensorItems.getValue()) {
            item.setToggled(toggle);
            updatedItems.add(item);
        }
        sensorItems.setValue(updatedItems);
        allToggledOn.setValue(toggle);
    }

    public LiveData<List<SensorItem>> getSensorItems() {
        return sensorItems;
    }


}
