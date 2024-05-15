package uk.ac.cam.cares.jps.user.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.login.AccountException;
import uk.ac.cam.cares.jps.sensor.SensorRepository;

@HiltViewModel
public class SensorViewModel extends ViewModel {
    private static final Logger LOGGER = LogManager.getLogger(SensorViewModel.class);
    private final SensorRepository sensorRepository;

    private final MutableLiveData<Boolean> _isRecording = new MutableLiveData<>();
    private final LiveData<Boolean> isRecording = _isRecording;
    private final MutableLiveData<Boolean> _hasAccountError = new MutableLiveData<>();
    private final LiveData<Boolean> hasAccountError = _hasAccountError;

    @Inject
    SensorViewModel(SensorRepository repository) {
        BasicConfigurator.configure();
        this.sensorRepository = repository;
        try {
            _isRecording.setValue(sensorRepository.getRecordingState());
        } catch (AccountException e) {
            _hasAccountError.setValue(true);
        }
    }

    public void startRecording() {
        try {
            sensorRepository.startRecording();
        } catch (AccountException e) {
            _hasAccountError.setValue(true);
        }
    }

    public void stopRecording() {
        sensorRepository.stopRecording();
    }

    public LiveData<Boolean> getIsRecording() {
        return isRecording;
    }

    public void setIsRecording(Boolean isRecording) {
        this._isRecording.setValue(isRecording);
    }

    public LiveData<Boolean> getHasAccountError() {
        return hasAccountError;
    }

    public void clearManagers(String userId) {
        sensorRepository.clearManagers(userId);
    }

    public void registerPhoneToUser() {
        try {
            sensorRepository.registerAppToUser();
        } catch (uk.ac.cam.cares.jps.login.AccountException e) {
            _hasAccountError.setValue(true);
        }
    }
}
