package uk.ac.cam.cares.jps.addasset.model;

import android.net.Uri;

import androidx.lifecycle.MutableLiveData;

public class DataFileDataModel extends AssetPropertyDataModel{
    // todo: this should be attached to the button only...
    MutableLiveData<Uri> filePath = new MutableLiveData<>(Uri.parse(""));

    DataFileDataModel(String fieldName) {
        super(fieldName);
    }

    public MutableLiveData<Uri> getMutableFilePath() {
        return filePath;
    }

    public Uri getFilePath() {
        return filePath.getValue();
    }

    public void setFilePath(Uri filePath) {
        this.filePath.setValue(filePath);
    }
}
