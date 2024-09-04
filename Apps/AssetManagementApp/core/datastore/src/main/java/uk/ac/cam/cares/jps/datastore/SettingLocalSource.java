package uk.ac.cam.cares.jps.datastore;

import android.content.Context;

import androidx.datastore.preferences.core.MutablePreferences;
import androidx.datastore.preferences.core.Preferences;
import androidx.datastore.preferences.rxjava2.RxPreferenceDataStoreBuilder;
import androidx.datastore.rxjava2.RxDataStore;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.Map;

import dagger.hilt.android.qualifiers.ApplicationContext;
import io.reactivex.Flowable;
import io.reactivex.Single;

public class SettingLocalSource {
    private static final Logger LOGGER = Logger.getLogger(SettingLocalSource.class);
    private RxDataStore<Preferences> dataStore;
    private Flowable<Map<String, Integer>> flowable;

    public SettingLocalSource(@ApplicationContext Context applicationContext) {
        BasicConfigurator.configure();
        dataStore = new RxPreferenceDataStoreBuilder(applicationContext, "Settings").build();
        flowable = dataStore.data().map(preferences -> CommonFunctions.convertPreferencesToIntegerMap(preferences));
    }

    public Flowable<Map<String, Integer>> getSettings() {
        return flowable;
    }

    public void updateSettings(Map<String, Integer> inputs) {
        dataStore.updateDataAsync(prefsIn -> {
            MutablePreferences mutablePreferences = prefsIn.toMutablePreferences();
            mutablePreferences.clear();

            for (Map.Entry<String, Integer> input : inputs.entrySet()) {
                mutablePreferences.set(new Preferences.Key<>(input.getKey()), input.getValue());
            }
            return Single.just(mutablePreferences);
        });
    }
}
