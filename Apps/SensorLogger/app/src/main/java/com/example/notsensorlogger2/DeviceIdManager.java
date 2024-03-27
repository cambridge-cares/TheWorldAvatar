package com.example.notsensorlogger2;

import android.content.Context;
import android.content.SharedPreferences;
import java.util.UUID;

public class DeviceIdManager {
    private static final String PREFS_FILE = "device_id_prefs";
    private static final String DEVICE_ID_KEY = "device_id";

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

