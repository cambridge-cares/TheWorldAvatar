package com.example.notsensorlogger2;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;
import java.util.ArrayList;
import java.util.List;
import androidx.lifecycle.LiveData;


public class SensorViewModel extends ViewModel {
    private MutableLiveData<Boolean> isRecording = new MutableLiveData<>(false);
    private MutableLiveData<List<SensorItem>> sensorItems = new MutableLiveData<>(new ArrayList<>());

    // LiveData getters
    public LiveData<Boolean> getIsRecording() {
        return isRecording;
    }

    public LiveData<List<SensorItem>> getSensorItems() {
        return sensorItems;
    }

    // Business logic methods
    public void startRecording() {
        isRecording.setValue(true);
        // Add logic to start recording from the sensors here
    }

    public void stopRecording() {
        isRecording.setValue(false);
        // Add logic to stop recording from the sensors here
    }

    // Methods to manage sensor items
    public void setSensorItems(List<SensorItem> items) {
        sensorItems.setValue(items);
    }

    // Add methods to start and stop individual sensors as needed
}

