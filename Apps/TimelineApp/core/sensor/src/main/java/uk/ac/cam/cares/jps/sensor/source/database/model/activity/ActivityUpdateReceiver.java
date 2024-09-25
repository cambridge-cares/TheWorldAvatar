package uk.ac.cam.cares.jps.sensor.source.database.model.activity;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class ActivityUpdateReceiver extends BroadcastReceiver {

    public interface ActivityUpdateListener {
        void onActivityUpdate(String activityType);
    }

    private ActivityUpdateListener listener;


    public ActivityUpdateReceiver(ActivityUpdateListener listener) {
        this.listener = listener;
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent != null && intent.hasExtra("activityType")) {
            String activityType = intent.getStringExtra("activityType");
            if (listener != null && activityType != null) {
                listener.onActivityUpdate(activityType);
            }
        }
    }
}
