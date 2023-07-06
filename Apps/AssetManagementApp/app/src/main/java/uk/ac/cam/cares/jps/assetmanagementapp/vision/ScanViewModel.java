package uk.ac.cam.cares.jps.assetmanagementapp.vision;

import android.graphics.Rect;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

public class ScanViewModel extends ViewModel {

    private final MutableLiveData<Rect> bBox = new MutableLiveData<>();
    private final MutableLiveData<Boolean> isFlashOn = new MutableLiveData<>(false);

    public void setBBox(Rect rect) {
        this.bBox.setValue(rect);
    }

    public MutableLiveData<Rect> getBBox() {
        return bBox;
    }


    public MutableLiveData<Boolean> getIsFlashOn() {
        return isFlashOn;
    }

    public void toggleFlashState() {
        this.isFlashOn.setValue(!this.isFlashOn.getValue());
    }
}
