package uk.ac.cam.cares.jps.sensor.source.database.model.activity;

import android.app.IntentService;
import android.content.Intent;

import androidx.annotation.Nullable;

import com.google.android.gms.location.ActivityRecognitionResult;
import com.google.android.gms.location.DetectedActivity;

import java.util.List;

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
        sensorLocalSource = new SensorLocalSource(getApplicationContext());
    }


    @Override
    protected void onHandleIntent(@Nullable Intent intent) {
        if (ActivityRecognitionResult.hasResult(intent)) {
            ActivityRecognitionResult result = ActivityRecognitionResult.extractResult(intent);
            assert result != null;
            List<DetectedActivity> probableActivities = result.getProbableActivities();
            DetectedActivity mostConfidentActivity = getMostConfidentActivity(probableActivities);
            handleUserActivity(mostConfidentActivity);
        }
    }

    private void notifyActivityChange(String activityType) {
        Intent broadcastIntent = new Intent("uk.ac.cam.cares.jps.ACTIVITY_UPDATE");
        broadcastIntent.putExtra("activityType", activityType);
        sendBroadcast(broadcastIntent);
    }

    /**
     * Returns the activity with the highest confidence from the list of probable activities.
     *
     * @param probableActivities List of detected activities.
     * @return The activity with the highest confidence.
     */
    private DetectedActivity getMostConfidentActivity(List<DetectedActivity> probableActivities) {
        DetectedActivity mostConfidentActivity = null;

        for (DetectedActivity activity : probableActivities) {
            if (mostConfidentActivity == null || activity.getConfidence() > mostConfidentActivity.getConfidence()) {
                mostConfidentActivity = activity;
            }
        }

        return mostConfidentActivity;
    }


    private String handleUserActivity(DetectedActivity detectedActivity) {
        int activityType = detectedActivity.getType();
        int confidence = detectedActivity.getConfidence();
        long timestamp = System.currentTimeMillis();

        if (confidence > 75) {
            String activity = switch (activityType) {
                case DetectedActivity.IN_VEHICLE -> "vehicle";
                case DetectedActivity.ON_BICYCLE -> "bike";
                case DetectedActivity.ON_FOOT, DetectedActivity.WALKING -> "walking";
                case DetectedActivity.STILL -> "still";
                default -> "unknown";
            };
            notifyActivityChange(activity); // Notify about the detected activity
            sensorLocalSource.saveActivityData(activity, confidence, timestamp); // upload detected activity
            return activity;
        }
        return "Low confidence";
    }

}
