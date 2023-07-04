package uk.ac.cam.cares.jps.assetmanagementapp.vision;

import android.graphics.Rect;
import android.graphics.RectF;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

public class QRCodeViewModel extends ViewModel {

    private final MutableLiveData<Rect> bBox = new MutableLiveData<>();

    public void setBBox(Rect rect) {
        this.bBox.setValue(rect);
    }

    public MutableLiveData<Rect> getBBox() {
        return bBox;
    }
}
