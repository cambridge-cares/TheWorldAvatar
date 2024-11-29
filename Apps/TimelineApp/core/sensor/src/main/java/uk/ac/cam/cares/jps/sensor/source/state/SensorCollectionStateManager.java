package uk.ac.cam.cares.jps.sensor.source.state;

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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

import uk.ac.cam.cares.jps.sensor.source.handler.SensorType;

/**
 * A class that manages the sensor collection states. It is considered as a data source level component.
 * Functions:
 * 1. Init SensorCollectionState by reading from local storage (an encrypted SharedPreference file) or generate
 * 2. Store SensorCollectionState to local storage (SharedPreference)
 * 3. Clear the in memory SensorCollectionState if user logs out so the user's information will not be used for data upload. SharedPreference for the user still persists
 * 4. Get/set in memory SensorCollectionState
 * @see <a href="https://developer.android.com/training/data-storage/shared-preferences">SharedPreferences</a>
 * @see SensorCollectionState SensorCollectionState
 */
public class SensorCollectionStateManager {
    private static final String DEVICE_ID_KEY = "device_id";
    private static final String USER_ID_KEY = "user_id";
    private static final String RECORDING_STATE_KEY = "recording_state";
    private static final String TASK_ID_KEY = "task_id";
    private AtomicReference<SensorCollectionState> sensorCollectionState = new AtomicReference<>();
    private Context context;
    private Logger LOGGER = Logger.getLogger(SensorCollectionStateManager.class);
    private static final String SELECTED_SENSORS_KEY = "selected_sensors";


    public SensorCollectionStateManager(Context context) {
        this.context = context;
    }

    /**
     * Receive a userId and init the SensorCollectionState object with it
     * 1. Try to retrieve the SensorCollectionState from local SharedPreference, where the SharedPreference file name is hash of the userId
     *     a. If file exists, initialize the SensorCollectionState object with information stored in the file
     *     b. If file not exists, generate the deviceId and store store the SensorCollectionState object to SharedPreference file
     * @param userId User id of the current logged in user
     */
    public void initSensorCollectionState(String userId) {
        String sharedPrefFileName = getHashedFileName(userId);
        File sharedPrefsFile = new File(context.getApplicationInfo().dataDir + "/shared_prefs/" + sharedPrefFileName + ".xml");

        SharedPreferences sharedPreferences = getSharedPreferences(sharedPrefFileName);
        String deviceId = "";
        Boolean recordingState = false;
        String taskId = "";

        if (sharedPrefsFile.exists()) {
            deviceId = sharedPreferences.getString(DEVICE_ID_KEY, "");
            recordingState = sharedPreferences.getBoolean(RECORDING_STATE_KEY, false);
            taskId = sharedPreferences.getString(TASK_ID_KEY, "");
        }

        if (deviceId.isEmpty() || taskId.isEmpty()) {
            deviceId = deviceId.isEmpty() ? UUID.randomUUID().toString() : deviceId;
            recordingState = false;
            taskId = taskId.isEmpty() ? UUID.randomUUID().toString() : taskId;

            sharedPreferences.edit()
                    .putString(USER_ID_KEY, userId)
                    .putString(DEVICE_ID_KEY, deviceId)
                    .putBoolean(RECORDING_STATE_KEY, recordingState)
                    .putString(TASK_ID_KEY, taskId)
                    .apply();
        }

        // Load selectedSensors
        Set<String> selectedSensorNames = sharedPreferences.getStringSet(SELECTED_SENSORS_KEY, new HashSet<>());
        List<SensorType> loadedSelectedSensors = new ArrayList<>();
        for (String sensorName : selectedSensorNames) {
            try {
                SensorType sensorType = SensorType.valueOf(sensorName);
                loadedSelectedSensors.add(sensorType);
            } catch (IllegalArgumentException e) {
                // Handle invalid sensor name
            }
        }

        // Initialize sensorCollectionState
        SensorCollectionState state = new SensorCollectionState(userId, deviceId, recordingState, taskId);
        state.setSelectedSensors(loadedSelectedSensors);
        sensorCollectionState.set(state);
    }

    /**
     * Sets the task ID for the current sensor collection state and stores it in the encrypted SharedPreferences.
     *
     * @param taskId The unique identifier for the current recording task. This value will be stored
     *               both in memory and in the encrypted SharedPreferences for persistence.
     *
     * If the sensor collection state has not been initialized (i.e., it is null), the method will return immediately
     * without performing any operations.
     *
     */
    public void setTaskId(String taskId) {
        if (sensorCollectionState.get() == null) {
            return;
        }

        synchronized (sensorCollectionState.get()) {
            sensorCollectionState.get().setTaskId(taskId);

            SharedPreferences sharedPreferences = getSharedPreferences(getHashedFileName(sensorCollectionState.get().getUserId()));
            sharedPreferences.edit()
                    .putString("task_id", taskId)
                    .apply();
        }
    }

    /**
     * Retrieves the current task ID from the sensor collection state.
     *
     * @return The unique task ID associated with the current recording session.
     *
     * @throws SensorCollectionStateException if the sensor collection state has not been initialized.
     *                                        This exception indicates that the task ID cannot be retrieved
     *                                        until the sensor collection state is properly initialized.
     *
     *Task ID is accessible only if the sensor collection state is initialized and available.
     */
    public String getTaskId() throws SensorCollectionStateException {
        if (sensorCollectionState.get() == null) {
            throw new SensorCollectionStateException("SensorCollectionState is null. Need to reinitialize with userId.");
        }

        return sensorCollectionState.get().getTaskId();
    }


    /**
     * Get the current SensorCollectionState object
     * @return SensorCollectionState object
     */
    public SensorCollectionState getSensorCollectionState() {
        if (sensorCollectionState.get() != null) {
            return sensorCollectionState.get();
        }

        LOGGER.info("sensor collection state is null");
        return null;
    }

    /**
     * Update the current in memory SensorCollectionState object and SharedPreference stored
     * @param isRecording Whether the phone is recording sensor data
     */
    public void setRecordingState(Boolean isRecording) {
        if (sensorCollectionState.get() == null) {
            return;
        }

        synchronized (sensorCollectionState.get()) {
            sensorCollectionState.get().setRecordingState(isRecording);

            SharedPreferences sharedPreferences = getSharedPreferences(getHashedFileName(sensorCollectionState.get().getUserId()));
            sharedPreferences.edit()
                    .putBoolean(RECORDING_STATE_KEY, isRecording)
                    .apply();
        }
    }

    /**
     * Get recording state
     * @return Whether the phone is recording data now
     * @throws SensorCollectionStateException the in memory SensorCollectionState doesn't exist due to user logout or other reason
     */
    public boolean getRecordingState() throws SensorCollectionStateException {
        if (sensorCollectionState.get() == null) {
            throw new SensorCollectionStateException("SensorCollectionState is null. Need to reinitialize with userId.");
        }

        return sensorCollectionState.get().getRecordingState();
    }

    /**
     * Remove the current in memory state and set the SharedPreference recording state to false
     * @param userId
     */
    public void clearState(String userId) {
        sensorCollectionState.set(null);
        LOGGER.info("sensor collection state is set to null");

        SharedPreferences sharedPreferences = getSharedPreferences(getHashedFileName(userId));
        sharedPreferences.edit()
                .putBoolean(RECORDING_STATE_KEY, false)
                .apply();
    }

    /**
     * Get current deviceId
     * @return deviceId
     * @throws SensorCollectionStateException
     */
    public String getDeviceId() throws SensorCollectionStateException {
        if (sensorCollectionState.get() == null) {
            throw new SensorCollectionStateException("SensorCollectionState is null. Need to reinitialize with userId.");
        }

        return sensorCollectionState.get().getDeviceId();
    }

    /**
     * Get current userId
     * @return user id
     * @throws SensorCollectionStateException
     */
    public String getUserId() throws SensorCollectionStateException {
        if (sensorCollectionState.get() == null) {
            throw new SensorCollectionStateException("SensorCollectionState is null. Need to reinitialize with userId.");
        }

        return sensorCollectionState.get().getUserId();
    }

    /**
     * Get the encrypted SharedPreference object with the provided file name
     * @param sharedPrefFileName
     * @return
     */
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

    /**
     * Get the hashed file name with the provided user Id
     * @param userId user id
     * @return hashed file name
     */
    private String getHashedFileName(String userId) {
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

    /**
     * Get the hashed file name with the current status's user id
     * @return hashed file name
     */
    public String getHashedFileName() throws SensorCollectionStateException {
        if (sensorCollectionState.get() == null) {
            throw new SensorCollectionStateException("SensorCollectionState is null. Need to reinitialize with userId.");
        }

        return getHashedFileName(sensorCollectionState.get().getUserId());
    }

    public List<SensorType> getSelectedSensors() throws SensorCollectionStateException {
        if (sensorCollectionState.get() == null) {
            throw new SensorCollectionStateException("SensorCollectionState is null. Need to reinitialize with userId.");
        }
        return sensorCollectionState.get().getSelectedSensors();
    }

    public void setSelectedSensors(List<SensorType> sensors) {
        if (sensorCollectionState.get() == null) {
            return;
        }
        synchronized (sensorCollectionState.get()) {
            sensorCollectionState.get().setSelectedSensors(sensors);

            // Save to SharedPreferences
            SharedPreferences sharedPreferences = getSharedPreferences(getHashedFileName(sensorCollectionState.get().getUserId()));
            Set<String> sensorNames = new HashSet<>();
            for (SensorType sensor : sensors) {
                sensorNames.add(sensor.name());
            }
            sharedPreferences.edit()
                    .putStringSet(SELECTED_SENSORS_KEY, sensorNames)
                    .apply();
        }
    }

}

