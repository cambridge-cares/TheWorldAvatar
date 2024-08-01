package uk.ac.cam.cares.sensorlogger;


import android.content.Context;
import android.content.SharedPreferences;
import java.util.UUID;

/**
 * Manages the device identifier storage and retrieval. This class ensures that a unique device ID is generated,
 * stored, and consistently retrieved across different sessions and app restarts.
 *
 * The device ID is stored in the application's shared preferences, allowing it to persist across app launches.
 */
public class DeviceIdManager {
    private static final String PREFS_FILE = "device_id_prefs";
    private static final String DEVICE_ID_KEY = "device_id";

    /**
     * Retrieves the unique device identifier from shared preferences or generates a new one if it doesn't exist.
     * The ID is then stored in SharedPreferences for future access.
     *
     * @param context The application context used to access SharedPreferences.
     * @return The unique device identifier.
     */
    public static String getDeviceId(Context context) {
        SharedPreferences prefs = context.getSharedPreferences(PREFS_FILE, Context.MODE_PRIVATE);
        String deviceId = prefs.getString(DEVICE_ID_KEY, null);
        if (deviceId == null) {
            deviceId = UUID.randomUUID().toString();
            prefs.edit().putString(DEVICE_ID_KEY, deviceId).apply();
        }
        return deviceId;
    }
}

