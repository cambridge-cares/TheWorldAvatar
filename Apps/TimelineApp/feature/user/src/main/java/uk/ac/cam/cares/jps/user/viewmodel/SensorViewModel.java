package uk.ac.cam.cares.jps.user.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.sensor.SensorRepository;

@HiltViewModel
public class SensorViewModel extends ViewModel {
    private static final Logger LOGGER = LogManager.getLogger(SensorViewModel.class);
    private SensorRepository sensorRepository;

    private MutableLiveData<Boolean> _isRecording = new MutableLiveData<>(false);
    private LiveData<Boolean> isRecording = _isRecording;

    @Inject
    SensorViewModel(SensorRepository repository) {
        BasicConfigurator.configure();
        this.sensorRepository = repository;
    }

    public void startRecording() {
        sensorRepository.startRecording();
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
}
