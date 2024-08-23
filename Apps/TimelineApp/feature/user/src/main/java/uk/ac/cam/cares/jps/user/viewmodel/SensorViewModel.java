package uk.ac.cam.cares.jps.user.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorHandler;
import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;
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
    }

    /**
     * Start recording
     */
    public void startRecording(List<SensorType> selectedSensors) {
        sensorRepository.startRecording(selectedSensors, new RepositoryCallback<Boolean>() {
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
}
