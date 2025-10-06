package uk.ac.cam.cares.jps.data;

import static android.content.Context.MODE_PRIVATE;

import android.content.Context;
import android.content.SharedPreferences;

import org.apache.log4j.Logger;

import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.User;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;
import uk.ac.cam.cares.jps.utils.Utils;

public class AppPreferenceRepository {
    private Context context;
    private LoginRepository loginRepository;
    private final Logger LOGGER = Logger.getLogger(AppPreferenceRepository.class);

    private final String KEY_UPLOAD_DURATION = "upload_duration";
    private final String KEY_AUTO_START_ENABLED = "autostart_enabled";
    private final String KEY_LOCATION_PERMISSION_PROMPTED = "location_permission_prompted";
    private final String KEY_TOOLTIPS_SKIP = "tooltip_skip";

    public AppPreferenceRepository(LoginRepository loginRepository, Context applicationContext) {
        this.context = applicationContext;
        this.loginRepository = loginRepository;
    }

    private <T> void setFieldInSharedPref(String fieldKey, T val) {
        loginRepository.getUserInfo(new RepositoryCallback<>() {
            @Override
            public void onSuccess(User result) {
                SharedPreferences preferences = context.getSharedPreferences(
                        Utils.getHashedFileName(result.getName()),
                        MODE_PRIVATE);
                if (val instanceof Boolean) {
                    preferences.edit().putBoolean(fieldKey, (Boolean) val).apply();
                } else if (val instanceof String) {
                    preferences.edit().putString(fieldKey, (String) val).apply();
                } else {
                    LOGGER.error("No corresponding handler for the provided type of app preference. Preference not saved. Please edit the source code!");
                }

            }

            @Override
            public void onFailure(Throwable error) {
                LOGGER.error("Cannot fetch the user info, no preference is saved.");
            }
        });
    }

    private <T> void getFieldFromSharedPref(RepositoryCallback<T> callback, String fieldKey, Class<T> type) {
        loginRepository.getUserInfo(new RepositoryCallback<>() {
            @Override
            public void onSuccess(User result) {
                SharedPreferences preferences = context.getSharedPreferences(
                        Utils.getHashedFileName(result.getName()),
                        MODE_PRIVATE);
                if (type == Boolean.class || type == boolean.class) {
                    Boolean val = preferences.getBoolean(fieldKey, false);
                    callback.onSuccess(type.cast(val));
                } else if (type == String.class) {
                    String val = preferences.getString(fieldKey, "");
                    callback.onSuccess(type.cast(val));
                } else {
                    LOGGER.error("No corresponding handler for the provided type of app preference. Preference not saved. Please edit the source code!");
                }

            }

            @Override
            public void onFailure(Throwable error) {
                LOGGER.error("Unable to fetch user info, hence failed to fetch shared pref.");
                callback.onFailure(error);
            }
        });
    }

    public void getUploadDuration(RepositoryCallback<String> callback) {
        getFieldFromSharedPref(callback, KEY_UPLOAD_DURATION, String.class);
    }

    public void setUploadDuration(String duration) {
        setFieldInSharedPref(KEY_UPLOAD_DURATION, duration);
    }

    public void getAutoStart(RepositoryCallback<Boolean> callback) {
        getFieldFromSharedPref(callback, KEY_AUTO_START_ENABLED, Boolean.class);
    }

    public void setAutoStart(Boolean autoStart) {
        setFieldInSharedPref(KEY_AUTO_START_ENABLED, autoStart);
    }

    public void getLocationPermissionPrompted(RepositoryCallback<Boolean> callback) {
        getFieldFromSharedPref(callback, KEY_LOCATION_PERMISSION_PROMPTED, Boolean.class);
    }

    public void setLocationPermissionPrompted(Boolean locationPermissionPrompted) {
        setFieldInSharedPref(KEY_LOCATION_PERMISSION_PROMPTED, locationPermissionPrompted);
    }

    public void getTooltipSkipped(RepositoryCallback<Boolean> callback) {
        getFieldFromSharedPref(callback, KEY_TOOLTIPS_SKIP, Boolean.class);
    }

    public void setTooltipSkipped(Boolean tooltipSkipped) {
        setFieldInSharedPref(KEY_TOOLTIPS_SKIP, tooltipSkipped);
    }
}
