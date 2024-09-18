package uk.ac.cam.cares.jps.sensor.source.database.model.activity;

import android.app.IntentService;
import android.content.Intent;

import androidx.annotation.Nullable;

import com.google.android.gms.location.ActivityRecognitionResult;
import com.google.android.gms.location.DetectedActivity;

public class ActivityRecognitionService extends IntentService {
    /**
     * @deprecated
     */
    public ActivityRecognitionService() {
        super("ActivityRecognitionService");
    }

    @Override
    protected void onHandleIntent(@Nullable Intent intent) {
        if (ActivityRecognitionResult.hasResult(intent)) {
            ActivityRecognitionResult result = ActivityRecognitionResult.extractResult(intent);
            assert result != null;
            DetectedActivity detectedActivity = result.getMostProbableActivity();
            handleUserActivity(detectedActivity);
        }
    }

    private String handleUserActivity(DetectedActivity detectedActivity) {
        int activityType = detectedActivity.getType();
        int confidence = detectedActivity.getConfidence();

        if (confidence > 75) {
            return switch (activityType) {
                case DetectedActivity.IN_VEHICLE -> "vehicle";
                case DetectedActivity.ON_BICYCLE -> "bike";
                case DetectedActivity.ON_FOOT, DetectedActivity.WALKING -> "on foot";
                case DetectedActivity.STILL -> "still";
                default -> "unknown";
            };
        }
        return "Low confidence";
    }
}
