package uk.ac.cam.cares.jps.datastore;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.otherInfoFromAssetAgentKeys;

import android.content.Context;

import androidx.datastore.preferences.core.MutablePreferences;
import androidx.datastore.preferences.core.Preferences;
import androidx.datastore.preferences.rxjava2.RxPreferenceDataStoreBuilder;
import androidx.datastore.rxjava2.RxDataStore;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import dagger.hilt.android.qualifiers.ApplicationContext;
import io.reactivex.Flowable;
import io.reactivex.Single;

public class OtherInfoLocalSource {
    private static final Logger LOGGER = Logger.getLogger(OtherInfoLocalSource.class);
    private final Context applicationContext;
    private Flowable<Map<String, String>> typesFlow;

    private RxDataStore<Preferences> dataStore;
    private final Map<String, RxDataStore<Preferences>> dataStores = new HashMap<>();
    private final Map<String, Flowable<Map<String, String>>> flowables = new HashMap<>();

    public OtherInfoLocalSource(@ApplicationContext Context applicationContext) {
        BasicConfigurator.configure();
        this.applicationContext = applicationContext;

        for (String key : otherInfoFromAssetAgentKeys) {
            RxDataStore<Preferences> temp = new RxPreferenceDataStoreBuilder(applicationContext, key).build();
            dataStores.put(key, temp);
            flowables.put(key, temp.data().map(preferences -> convertPreferencesToMap(preferences)));
        }

    }

    public Map<String, Flowable<Map<String, String>>> getOtherInfo() {
        return flowables;
    }

    private Map<String, String> convertPreferencesToMap(Preferences preferences) {
        Map<String, String> results = new HashMap<>();
        Map<Preferences.Key<?>, Object> preferencesMap =  preferences.asMap();
        for (Preferences.Key key : preferencesMap.keySet()) {
            results.put(key.toString(), preferencesMap.get(key).toString());
        }
        return results;
    }

    public void saveToLocalStore(String key, List<Map.Entry<String, String>> inputs) {
        dataStores.get(key).updateDataAsync(prefsIn -> {
            MutablePreferences mutablePreferences = prefsIn.toMutablePreferences();
            mutablePreferences.clear();
            for (Map.Entry<String, String> input : inputs) {
                mutablePreferences.set(new Preferences.Key<>(input.getKey()), input.getValue());
            }
            return Single.just(mutablePreferences);
        });
    }

}
