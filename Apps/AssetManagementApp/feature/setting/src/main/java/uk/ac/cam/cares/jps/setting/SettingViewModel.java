package uk.ac.cam.cares.jps.setting;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.*;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.Logger;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.SettingRepository;

@HiltViewModel
public class SettingViewModel extends ViewModel {
    private Map<String, MutableLiveData<Integer>> settings = new HashMap<>();
    private final SettingRepository repository;
    private final Logger LOGGER = Logger.getLogger(SettingViewModel.class);

    @Inject
    public SettingViewModel(SettingRepository repository) {
        this.repository = repository;
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
            if (!Arrays.asList(BASIC_SECTION_TITLE, LOCATION_SECTION_TITLE, SUPPLIER_SECTION_TITLE, PURCHASE_SECTION_TITLE, ITEM_SECTION_TITLE, OTHERS_SECTION_TITLE).contains(key)) {
                results.put(key, value.getValue());
            }
        });
        return results;
    }
}
