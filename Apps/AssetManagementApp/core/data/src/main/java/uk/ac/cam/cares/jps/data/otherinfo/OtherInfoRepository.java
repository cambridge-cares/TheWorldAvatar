package uk.ac.cam.cares.jps.data.otherinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.BUILDING;
import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.otherInfoFromAssetAgentKeys;

import com.android.volley.Response;
import com.android.volley.VolleyError;

import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.inject.Inject;

import io.reactivex.Completable;
import io.reactivex.disposables.Disposable;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.datastore.OtherInfoLocalSource;
import uk.ac.cam.cares.jps.model.AssetInfo;
import uk.ac.cam.cares.jps.model.building.Building;
import uk.ac.cam.cares.jps.model.building.Instance;
import uk.ac.cam.cares.jps.model.building.Room;
import uk.ac.cam.cares.jps.model.building.Workspace;
import uk.ac.cam.cares.jps.network.otherinfo.BMSNetworkSource;
import uk.ac.cam.cares.jps.network.otherinfo.OtherInfoNetworkSource;
import uk.ac.cam.cares.jps.network.otherinfo.OtherInfoResponse;

public class OtherInfoRepository {
    private Logger LOGGER =  Logger.getLogger(OtherInfoRepository.class);
    private OtherInfoLocalSource otherInfoLocalSource;
    private OtherInfoNetworkSource otherInfoNetworkSource;
    private BMSNetworkSource bmsNetworkSource;
    private Map<String, Map<String, String>> otherInfoByKey = new HashMap<>();

    List<Workspace> workspacesWithElements = new ArrayList<>();
    List<Workspace> roomsWithWorkspaces = new ArrayList<>();
    List<Instance> buildings = new ArrayList<>();

    @Inject
    public OtherInfoRepository(OtherInfoLocalSource otherInfoLocalSource, OtherInfoNetworkSource otherInfoNetworkSource, BMSNetworkSource bmsNetworkSource) {
        this.otherInfoLocalSource = otherInfoLocalSource;
        this.otherInfoNetworkSource = otherInfoNetworkSource;
        this.bmsNetworkSource = bmsNetworkSource;

        for (String key : otherInfoFromAssetAgentKeys) {
            otherInfoByKey.put(key, new HashMap<>());
        }
    }

    public void getAllOtherInfo(Map<String, RepositoryCallback<Map<String, String>>> callbacks, RepositoryCallback<List<Instance>> locationCallback) {
        retrieveFromNetworkSource(callbacks, locationCallback);

        for (String key : otherInfoByKey.keySet()) {
            // try retrieve from cache
            if (!otherInfoByKey.get(key).isEmpty()) {
                synchronized (otherInfoByKey.get(key)) {
                    callbacks.get(key).onSuccess(convertToLabelToIriMap(otherInfoByKey.get(key)));
                }
                return;
            }

            retrieveFromLocalSource(callbacks, key);
        }

    }

    private void otherInfoOnSuccess(OtherInfoResponse response, Map<String, RepositoryCallback<Map<String, String>>> callbacks) {
        workspacesWithElements = response.getWorkspaces();

        Map<String, HashMap<String, String>> otherInfo = response.getOtherInfo();
        for (String key : otherInfo.keySet()) {
            // compare whether network is different from cache
            synchronized (otherInfoByKey.get(key)) {
                if (!otherInfo.get(key).equals(otherInfoByKey.get(key))) {
                    // write to local datastore
                    List<Map.Entry<String, String>> difference = otherInfo.get(key).entrySet().stream()
                            .filter(entry -> !otherInfoByKey.get(key).containsKey(entry.getKey()) || !otherInfoByKey.get(key).containsValue(entry.getValue()))
                            .collect(Collectors.toList());
                    otherInfoLocalSource.saveToLocalStore(key, difference);
                }

                otherInfoByKey.get(key).putAll(otherInfo.get(key));
            }

            callbacks.get(key).onSuccess(convertToLabelToIriMap(otherInfo.get(key)));
        }
    }

    private void retrieveFromNetworkSource(Map<String, RepositoryCallback<Map<String, String>>> callbacks, RepositoryCallback<List<Instance>> locationCallback) {

        Completable otherInfoNetworkCall = Completable.create(emitter -> otherInfoNetworkSource.getOtherInfo(response -> {
            otherInfoOnSuccess(response, callbacks);
            emitter.onComplete();
        }, error -> {
            callbacks.values().forEach(callback -> callback.onFailure(error));
            LOGGER.error(error.getMessage());
            emitter.onError(error);
        }));

        Completable bmsNetworkCall = Completable.create(emitter -> bmsNetworkSource.getBuildingInfo(response -> {
            buildings = response;
            emitter.onComplete();
        }, error -> {
            locationCallback.onFailure(error);
            LOGGER.error(error.getMessage());
            emitter.onError(error);
        }));

        Completable combinedCompletable = Completable.mergeArray(otherInfoNetworkCall, bmsNetworkCall);
        Disposable disposable = combinedCompletable.subscribe(
                () -> {
                    // merge location info from both request
                    // todo: link roomsWithWorkspaces, workspacesWithElements and buildings
//                    for (Workspace workspace : workspacesWithElements) {
//
//                    }

                    // todo: store in local storage

                    // todo: callback to UI
                    locationCallback.onSuccess(buildings);

                },
                error -> {
                    LOGGER.error(error.getMessage());
                    locationCallback.onFailure(error);
                }
        );
    }

    private void retrieveFromLocalSource(Map<String, RepositoryCallback<Map<String, String>>> callbacks, String key) {
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

    private Map<String, String> convertToLabelToIriMap(Map<String, String> map) {
        Map<String, String> results = new HashMap<>();
        for (String key : map.keySet()) {
            results.put(map.get(key), key);
        }
        return results;
    }
}
