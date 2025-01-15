package uk.ac.cam.cares.jps.setting;

import static uk.ac.cam.cares.jps.login.LoginErrorMessage.NO_UER_INFO_RETRIEVED;
import static uk.ac.cam.cares.jps.login.LoginErrorMessage.SESSION_EXPIRED;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import android.content.Intent;

import androidx.activity.result.ActivityResultLauncher;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.setting.SettingRepository;
import uk.ac.cam.cares.jps.login.LoginRepository;

@HiltViewModel
public class SettingViewModel extends ViewModel {
    private Map<String, MutableLiveData<Integer>> settings = new HashMap<>();

    private MutableLiveData<String> name = new MutableLiveData<>("");
    private MutableLiveData<String> email = new MutableLiveData<>("");
    private MutableLiveData<Boolean> shouldShowSessionExpired = new MutableLiveData<>(false);

    private final SettingRepository repository;
    private final LoginRepository loginRepository;
    private final Logger LOGGER = Logger.getLogger(SettingViewModel.class);



    @Inject
    public SettingViewModel(SettingRepository repository, LoginRepository loginRepository) {
        this.repository = repository;
        this.loginRepository = loginRepository;
        initSettings();
    }

    public MutableLiveData<Integer> getSettingByKey(String key) {
        return settings.computeIfAbsent(key, k -> new MutableLiveData<>(1));
    }

    public void initSettings() {
        repository.getSettings(new RepositoryCallback<Map<String, Integer>>() {
            @Override
            public void onSuccess(Map<String, Integer> result) {
                for (Map.Entry<String, Integer> entry : result.entrySet()) {
                    getSettingByKey(entry.getKey()).postValue(entry.getValue());
                }
            }

            @Override
            public void onFailure(Throwable error) {
                LOGGER.error("not able to load settings from local storage, use default settings where all are checked");
            }
        });
    }

    public void submitChanges() {
        repository.updateSettings(getSettingsInInteger());
    }

    private Map<String, Integer> getSettingsInInteger() {
        Map<String, Integer> results = new HashMap<>();
        settings.forEach((key, value) -> {
            if (!Arrays.asList(BASIC_SECTION_TITLE, LOCATION_SECTION_TITLE, SUPPLIER_SECTION_TITLE, PURCHASE_SECTION_TITLE, ITEM_SECTION_TITLE, MAINTENANCE_SECTION_TITLE, OTHERS_SECTION_TITLE).contains(key)) {
                results.put(key, value.getValue());
            }
        });
        return results;
    }

    public void getUserInfo() {
        loginRepository.getUserInfo(new uk.ac.cam.cares.jps.login.RepositoryCallback<JSONObject>() {
            @Override
            public void onSuccess(JSONObject result) {
                try {
                    if (result.has("email")) {
                        email.postValue(result.getString("email"));
                    }

                    String nameStr = "";
                    if (result.has("given_name")) {
                        nameStr  = nameStr + result.getString("given_name") + " ";
                    }
                    if (result.has("family_name")) {
                        nameStr  = nameStr + result.getString("family_name");
                    }
                    name.postValue(nameStr);

                }
                catch (JSONException e) {
                    throw new RuntimeException(e);
                }
            }

            @Override
            public void onFailure(Throwable error) {
                if (error.getMessage().equals(NO_UER_INFO_RETRIEVED) || error.getMessage().equals(SESSION_EXPIRED)) {
                    shouldShowSessionExpired.postValue(true);
                }
            }
        });
    }

    public MaterialAlertDialogBuilder getSessionExpiredDialog(Fragment fragment) {
        return loginRepository.getSessionExpiredDialog(fragment);
    }

    public ActivityResultLauncher<Intent> getLogoutLauncher(Fragment fragment) {
        return loginRepository.getLogoutLauncher(fragment);
    }

    public void logout(ActivityResultLauncher<Intent> logoutLauncher) {
        logoutLauncher.launch(loginRepository.getLogOutIntent());
    }

    public MutableLiveData<String> getName() {
        return name;
    }

    public MutableLiveData<String> getEmail() {
        return email;
    }

    public MutableLiveData<Boolean> getShouldShowSessionExpired() {
        return shouldShowSessionExpired;
    }
}
