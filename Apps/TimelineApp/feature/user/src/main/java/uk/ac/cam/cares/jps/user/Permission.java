package uk.ac.cam.cares.jps.user;

import android.Manifest;
import android.os.Build;

import androidx.activity.result.ActivityResultLauncher;

/**
 * Permission class
 */
public class Permission {
    public enum PermissionType {
        LOCATION_FINE,
        AUDIO,
        NOTIFICATION
    }

    String permissionString = "";
    int explanation = 0;
    PermissionType type;
    boolean isGranted = false;
    ActivityResultLauncher<String> launcher;

    public Permission(PermissionType type, ActivityResultLauncher<String> launcher) {
        this.type = type;
        this.launcher = launcher;

        switch (type) {
            case LOCATION_FINE -> {
                permissionString = Manifest.permission.ACCESS_FINE_LOCATION;
                explanation = R.string.location_permission_explanation;
            }
            case AUDIO -> {
                permissionString = Manifest.permission.RECORD_AUDIO;
                explanation = R.string.audio_permission_explanation;
            }
            case NOTIFICATION -> {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
                    permissionString = Manifest.permission.POST_NOTIFICATIONS;
                    explanation = R.string.notification_permission_explanation;
                }
            }
        }
    }
}
