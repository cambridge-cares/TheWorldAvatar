package uk.ac.cam.cares.jps.data;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.otherInfoFromAssetAgentKeys;

import com.android.volley.Response;

import org.apache.log4j.Logger;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.inject.Inject;

import io.reactivex.disposables.Disposable;
import uk.ac.cam.cares.jps.datastore.OtherInfoLocalSource;
import uk.ac.cam.cares.jps.network.otherinfo.OtherInfoNetworkSource;

public class OtherInfoRepository {
    private Logger LOGGER =  Logger.getLogger(OtherInfoRepository.class);
    private OtherInfoLocalSource otherInfoLocalSource;
    private OtherInfoNetworkSource otherInfoNetworkSource;
    private Map<String, Map<String, String>> otherInfoByKey = new HashMap<>();

    @Inject
    public OtherInfoRepository(OtherInfoLocalSource otherInfoLocalSource, OtherInfoNetworkSource otherInfoNetworkSource) {
        this.otherInfoLocalSource = otherInfoLocalSource;
        this.otherInfoNetworkSource = otherInfoNetworkSource;

        for (String key : otherInfoFromAssetAgentKeys) {
            otherInfoByKey.put(key, new HashMap<>());
        }
    }

    public void getAllOtherInfo(Map<String, RepositoryCallback<Map<String, String>>> callbacks) {
        Response.Listener<Map<String, HashMap<String, String>>> onSuccess = response -> {
            for (String key : response.keySet()) {
                // compare whether network is different from cache
                synchronized (otherInfoByKey.get(key)) {
                    if (!response.get(key).equals(otherInfoByKey.get(key))) {
                        // write to local datastore
                        List<Map.Entry<String, String>> difference = response.get(key).entrySet().stream()
                                .filter(entry -> !otherInfoByKey.get(key).containsKey(entry.getKey()) || !otherInfoByKey.get(key).containsValue(entry.getValue()))
                                .collect(Collectors.toList());
                        otherInfoLocalSource.saveToLocalStore(key, difference);
                    }

                    otherInfoByKey.get(key).putAll(response.get(key));
                }

                callbacks.get(key).onSuccess(convertToLabelToIriMap(response.get(key)));
            }
        };
        Response.ErrorListener onFailure = error -> {
            callbacks.values().forEach(callback -> callback.onFailure(error));
            LOGGER.error(error.getMessage());
        };
        otherInfoNetworkSource.getOtherInfoFromAssetAgent(onSuccess, onFailure);


        for (String key : otherInfoByKey.keySet()) {
            if (!otherInfoByKey.get(key).isEmpty()) {
                synchronized (otherInfoByKey.get(key)) {
                    callbacks.get(key).onSuccess(convertToLabelToIriMap(otherInfoByKey.get(key)));
                }
                return;
            }

            // fetch from local source is cache is empty
            Disposable typesDisposable = otherInfoLocalSource.getOtherInfo().get(key).subscribe(
                    stringMap -> {
                        // need to check otherinfobykey again?
                        synchronized (otherInfoByKey.get(key)) {
                            otherInfoByKey.get(key).putAll(stringMap);
                        }
                        callbacks.get(key).onSuccess(convertToLabelToIriMap(stringMap));
                    },
                    error -> {
                        LOGGER.error(error.getMessage());
                        callbacks.get(key).onFailure(error);
                    },
                    () -> {
                        LOGGER.info("local datastore completed");
                    }
            );
        }


    }

    private Map<String, String> convertToLabelToIriMap(Map<String, String> map) {
        Map<String, String> results = new HashMap<>();
        for (String key : map.keySet()) {
            results.put(map.get(key), key);
        }
        return results;
    }
}
