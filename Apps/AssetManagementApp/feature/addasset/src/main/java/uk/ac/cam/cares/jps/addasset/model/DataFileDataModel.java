package uk.ac.cam.cares.jps.addasset.model;

import android.content.Context;
import android.net.Uri;

import androidx.lifecycle.MutableLiveData;

import uk.ac.cam.cares.jps.utils.FileUtils;

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

    public String getFieldValue(Context context) {
        if (filePath.getValue().toString().isEmpty()) {
            return "";
        }
        return FileUtils.getFileNameFromUri(filePath.getValue(), context);
    }
}
