package uk.ac.cam.cares.jps.sensor.source.activity;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.google.android.gms.location.ActivityRecognitionResult;
import com.google.android.gms.location.DetectedActivity;

import org.apache.log4j.Logger;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import dagger.hilt.android.EntryPointAccessors;
import uk.ac.cam.cares.jps.sensor.source.database.SensorLocalSource;
import androidx.localbroadcastmanager.content.LocalBroadcastManager;



public class ActivityRecognitionReceiver extends BroadcastReceiver {

    SensorLocalSource sensorLocalSource;
    private final Logger LOGGER = Logger.getLogger(ActivityRecognitionReceiver.class);
    private final ExecutorService executorService = Executors.newSingleThreadExecutor();

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent == null) {
            LOGGER.warn("Received null intent");
            return;
        }

        SensorLocalSourceEntryPoint entryPoint = EntryPointAccessors.fromApplication(
                context.getApplicationContext(),
                SensorLocalSourceEntryPoint.class);
        sensorLocalSource = entryPoint.getSensorLocalSource();
        if (sensorLocalSource == null) {
            LOGGER.warn("Received null sensorLocalSource");
            return;
        }


        if (ActivityRecognitionResult.hasResult(intent)) {
            LOGGER.info("Received activity recognition result.");
            ActivityRecognitionResult result = ActivityRecognitionResult.extractResult(intent);
            List<DetectedActivity> probableActivities = result.getProbableActivities();
            DetectedActivity mostConfidentActivity = getMostConfidentActivity(probableActivities);
            handleUserActivity(context, mostConfidentActivity);
        } else {
            LOGGER.warn("No activity recognition result found in intent.");
        }
    }

    private DetectedActivity getMostConfidentActivity(List<DetectedActivity> probableActivities) {
        DetectedActivity mostConfidentActivity = null;

        for (DetectedActivity activity : probableActivities) {
            if (mostConfidentActivity == null || activity.getConfidence() > mostConfidentActivity.getConfidence()) {
                mostConfidentActivity = activity;
            }
        }

        return mostConfidentActivity;
    }

    private void handleUserActivity(Context context, DetectedActivity detectedActivity) {
        int activityType = detectedActivity.getType();
        int confidence = detectedActivity.getConfidence();
        long timestamp = System.currentTimeMillis();

        LOGGER.info("Detected activity: " + activityType + ", confidence: " + confidence);

        if (confidence > 75) {
            String activity = mapActivityType(activityType);
            notifyActivityChange(context, activity);
            executorService.execute(() -> {
                sensorLocalSource.saveActivityData(activity, confidence, timestamp);
            });
        } else {
            LOGGER.info("Activity confidence too low to record.");
        }
    }

    private String mapActivityType(int activityType) {
        switch (activityType) {
            case DetectedActivity.IN_VEHICLE:
                return "vehicle";
            case DetectedActivity.ON_BICYCLE:
                return "bike";
            case DetectedActivity.ON_FOOT:
            case DetectedActivity.WALKING:
                return "walking";
            case DetectedActivity.STILL:
                return "still";
            default:
                return "unknown";
        }
    }

    private void notifyActivityChange(Context context, String activityType) {
        Intent broadcastIntent = new Intent("uk.ac.cam.cares.jps.ACTIVITY_UPDATE");
        broadcastIntent.putExtra("activityType", activityType);
        LocalBroadcastManager.getInstance(context).sendBroadcast(broadcastIntent);
    }

}
