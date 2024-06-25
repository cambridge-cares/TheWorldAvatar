package uk.ac.cam.cares.jps.camera;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

public class CameraViewModel extends ViewModel {
    private final MutableLiveData<Boolean> isFlashOn = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> getIsFlashOn() {
        return isFlashOn;
    }
    public void toggleFlashState() {
        this.isFlashOn.setValue(!this.isFlashOn.getValue());
    }
}
