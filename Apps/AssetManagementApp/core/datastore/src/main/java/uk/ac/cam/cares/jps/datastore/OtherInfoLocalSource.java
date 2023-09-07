package uk.ac.cam.cares.jps.datastore;
import android.content.Context;
import android.util.Pair;

import androidx.datastore.preferences.core.Preferences;
import androidx.datastore.preferences.core.PreferencesKeys;
import androidx.datastore.preferences.rxjava2.RxPreferenceDataStoreBuilder;
import androidx.datastore.rxjava2.RxDataStore;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import dagger.hilt.android.qualifiers.ApplicationContext;
import io.reactivex.Flowable;

public class OtherInfoLocalSource {
    private static final Logger LOGGER = Logger.getLogger(OtherInfoLocalSource.class);
    private final Context applicationContext;
    private Flowable<Map<String, String>> typesFlow;

    public OtherInfoLocalSource(@ApplicationContext Context applicationContext) {
        BasicConfigurator.configure();
        this.applicationContext = applicationContext;

        RxDataStore<Preferences>  dataStore = new RxPreferenceDataStoreBuilder(applicationContext, /*name=*/ "types").build();
        typesFlow = dataStore.data().map(preferences -> convertPreferencesToMap(preferences));

    }

    public Flowable<Map<String, String>> getOtherInfo() {
        return typesFlow;
    }

    private List<Pair<String, String>> convertPreferencesToList(Preferences preferences) {
        List<Pair<String, String>> results = new ArrayList<>();
        for (Preferences.Key key : preferences.asMap().keySet()) {
            results.add(new Pair<>(key.toString(), preferences.get(key).toString()));
        }
        return results;
    }

    private Map<String, String> convertPreferencesToMap(Preferences preferences) {
        Map<String, String> results = new HashMap<>();
        Map<Preferences.Key<?>, Object> preferencesMap =  preferences.asMap();
        for (Preferences.Key key : preferencesMap.keySet()) {
            results.put(key.toString(), preferencesMap.get(key).toString());
        }
        return results;
    }

}
