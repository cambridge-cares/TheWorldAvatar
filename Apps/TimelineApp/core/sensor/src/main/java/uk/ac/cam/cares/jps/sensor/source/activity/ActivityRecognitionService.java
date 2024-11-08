package uk.ac.cam.cares.jps.sensor.source.activity;

import android.app.IntentService;
import android.content.Intent;

import androidx.annotation.Nullable;

import com.google.android.gms.location.ActivityRecognitionResult;


import javax.inject.Inject;

import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;

public class ActivityRecognitionService extends IntentService {
    @Inject
    SensorLocalSource sensorLocalSource;

    /**
     * @deprecated
     */
    public ActivityRecognitionService() {
        super("ActivityRecognitionService");
    }


    @Override
    public void onCreate() {
        super.onCreate();
    }


    @Override
    protected void onHandleIntent(@Nullable Intent intent) {
        if (ActivityRecognitionResult.hasResult(intent)) {
            ActivityRecognitionResult result = ActivityRecognitionResult.extractResult(intent);
            assert result != null;
        }
    }


}
