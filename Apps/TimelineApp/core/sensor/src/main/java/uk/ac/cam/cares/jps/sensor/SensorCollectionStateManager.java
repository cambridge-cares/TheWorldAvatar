package uk.ac.cam.cares.jps.sensor;

import android.content.Context;
import android.content.SharedPreferences;

import androidx.security.crypto.EncryptedSharedPreferences;
import androidx.security.crypto.MasterKey;

import org.apache.log4j.Logger;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

import uk.ac.cam.cares.jps.login.AccountException;

public class SensorCollectionStateManager {
    private static final String DEVICE_ID_KEY = "device_id";
    private static final String USER_ID_KEY = "user_id";
    private static final String RECORDING_STATE_KEY = "recording_state";
    private AtomicReference<SensorCollectionState> sensorCollectionState = new AtomicReference<>();
    private Context context;
    private Logger LOGGER = Logger.getLogger(SensorCollectionStateManager.class);

    public SensorCollectionStateManager(Context context) {
        this.context = context;
    }

    public void initSensorCollectionState(String userId) {
        String sharedPrefFileName = getHashedPrefsFileName(userId);
        File sharedPrefsFile = new File(context.getApplicationInfo().dataDir + "/shared_prefs/" + sharedPrefFileName + ".xml");
        if (sharedPrefsFile.exists()) {
            SharedPreferences sharedPreferences = getSharedPreferences(sharedPrefFileName);
            String deviceId = sharedPreferences.getString(DEVICE_ID_KEY, "");
            Boolean recordingState = sharedPreferences.getBoolean(RECORDING_STATE_KEY, false);

            sensorCollectionState.set(new SensorCollectionState(userId, deviceId, recordingState));
            return;
        }

//        String deviceId = UUID.randomUUID().toString();
        String deviceId = "c2583bd2-456c-4812-9752-658c9dff74b0";
        boolean recordingState = false;
        // todo: check this sync block
        sensorCollectionState.set(new SensorCollectionState(userId, deviceId, recordingState));
        SharedPreferences sharedPreferences = getSharedPreferences(sharedPrefFileName);
        sharedPreferences.edit()
                .putString(USER_ID_KEY, userId)
                .putString(DEVICE_ID_KEY, deviceId)
                .putBoolean(RECORDING_STATE_KEY, recordingState)
                .apply();
    }

    public SensorCollectionState getSensorCollectionState() {
        if (sensorCollectionState.get() != null) {
            return sensorCollectionState.get();
        }

        LOGGER.info("sensor collection state is null");
        return null;
    }

    public void setRecordingState(Boolean isRecording) {
        if (sensorCollectionState.get() == null) {
            return;
        }

        synchronized (sensorCollectionState.get()) {
            sensorCollectionState.get().setRecordingState(isRecording);

            SharedPreferences sharedPreferences = getSharedPreferences(getHashedPrefsFileName(sensorCollectionState.get().getUserId()));
            sharedPreferences.edit()
                    .putBoolean(RECORDING_STATE_KEY, isRecording)
                    .apply();
        }
    }

    public boolean getRecordingState() throws AccountException {
        if (sensorCollectionState.get() == null) {
            throw new AccountException("SensorCollectionState is null. Need to reinitialize with userId.");
        }

        return sensorCollectionState.get().getRecordingState();
    }

    public void clearState(String userId) {
        sensorCollectionState.set(null);
        LOGGER.info("sensor collection state is set to null");

        SharedPreferences sharedPreferences = getSharedPreferences(getHashedPrefsFileName(userId));
        sharedPreferences.edit()
                .putBoolean(RECORDING_STATE_KEY, false)
                .apply();
    }

    public String getDeviceId() throws AccountException {
        if (sensorCollectionState.get() == null) {
            throw new AccountException("SensorCollectionState is null. Need to reinitialize with userId.");
        }

        return sensorCollectionState.get().getDeviceId();
    }

    public String getUserId() throws AccountException {
        if (sensorCollectionState.get() == null) {
            throw new AccountException("SensorCollectionState is null. Need to reinitialize with userId.");
        }

        return sensorCollectionState.get().getUserId();
    }

    private SharedPreferences getSharedPreferences(String sharedPrefFileName) {
        try {
            MasterKey masterKey = new MasterKey.Builder(context)
                    .setKeyScheme(MasterKey.KeyScheme.AES256_GCM)
                    .build();

            return EncryptedSharedPreferences.create(
                    context,
                    sharedPrefFileName,
                    masterKey,
                    EncryptedSharedPreferences.PrefKeyEncryptionScheme.AES256_SIV,
                    EncryptedSharedPreferences.PrefValueEncryptionScheme.AES256_GCM
            );
        } catch (GeneralSecurityException | IOException e) {
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    private String getHashedPrefsFileName(String userId) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hashBytes = digest.digest(userId.getBytes(StandardCharsets.UTF_8));
            StringBuilder hexString = new StringBuilder();
            for (byte b : hashBytes) {
                String hex = Integer.toHexString(0xff & b);
                if (hex.length() == 1) hexString.append('0');
                hexString.append(hex);
            }
            return hexString.toString();
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
            return null;
        }
    }

}

