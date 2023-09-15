package uk.ac.cam.cares.jps.data;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.MANUAL_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.SPEC_SHEET_SECTION_TITLE;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.basicInfoOrder;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.docLineInfoOrder;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.itemInfoOrder;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.locationInfoOrder;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.maintenanceInfoOrder;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.supplierInfoOrder;

import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import io.reactivex.disposables.Disposable;
import uk.ac.cam.cares.jps.datastore.SettingLocalSource;

public class SettingRepository {
    Map<String, Integer> settings = new HashMap<>();

    private Logger LOGGER =  Logger.getLogger(SettingRepository.class);
    private SettingLocalSource settingLocalSource;

    public SettingRepository(SettingLocalSource settingLocalSource) {
        this.settingLocalSource = settingLocalSource;
    }

    public void getSettings(RepositoryCallback callback) {
        if (!settings.isEmpty()) {
            callback.onSuccess(settings);
            return;
        }

        Disposable typesDisposable = settingLocalSource.getSettings().subscribe(
                stringIntegerMap -> {
                    if (stringIntegerMap.isEmpty()) {
                        getDefaultSettings();
                        updateSettings(settings);
                    } else {
                        settings = stringIntegerMap;
                    }

                  callback.onSuccess(settings);
                },
                error -> {
                    LOGGER.error(error.getMessage());
                    callback.onFailure(error);
                },
                () -> {
                    LOGGER.info("local datastore completed");
                }
        );
    }

    public void updateSettings(Map<String, Integer> newSettings) {
        settings = newSettings;
        settingLocalSource.updateSettings(newSettings);
    }

    public Map<String, Integer> getDefaultSettings() {
        List<String> allKeys = new ArrayList<>();
        allKeys.addAll(basicInfoOrder);
        allKeys.addAll(locationInfoOrder);
        allKeys.addAll(supplierInfoOrder);
        allKeys.addAll(docLineInfoOrder);
        allKeys.addAll(itemInfoOrder);
        allKeys.addAll(maintenanceInfoOrder);
        allKeys.add(SPEC_SHEET_SECTION_TITLE);
        allKeys.add(MANUAL_SECTION_TITLE);

        for (String key : allKeys) {
            // MaterialCheckbox.STATE_CHECKED = 1
            settings.put(key, 1);
        }
        return settings;
    }
}
