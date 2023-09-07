package uk.ac.cam.cares.jps.data;

import android.util.Pair;

import androidx.datastore.preferences.core.Preferences;

import org.apache.log4j.Logger;
import org.reactivestreams.Subscriber;
import org.reactivestreams.Subscription;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.inject.Inject;

import io.reactivex.Flowable;
import io.reactivex.disposables.Disposable;
import io.reactivex.functions.Consumer;
import uk.ac.cam.cares.jps.datastore.OtherInfoLocalSource;

public class OtherInfoRepository {
    private Logger LOGGER =  Logger.getLogger(OtherInfoRepository.class);
    private OtherInfoLocalSource otherInfoLocalSource;
    private Map<String, String> types = new HashMap<>();

    @Inject
    public OtherInfoRepository(OtherInfoLocalSource otherInfoLocalSource) {
        this.otherInfoLocalSource = otherInfoLocalSource;

    }

    // todo: should merge data from network source and local source here and write update to local source
    public void getTypes(RepositoryCallback callback) {
        Disposable typesDisposable = otherInfoLocalSource.getOtherInfo().subscribe(
                stringMap -> {
                    types.putAll(stringMap);
//                    for (Map.Entry<String, String> entry : types.entrySet()) {
//                        LOGGER.info(entry.getKey() + " " + entry.getValue());
//                    }
                    callback.onSuccess(convertToModelList(types));
                },
                error -> {
                    LOGGER.error(error.getMessage());
                    callback.onFailure(error);
                },
                () -> {
                    LOGGER.info("types completed");
                }
        );
    }

    private List<OtherInfoModel> convertToModelList(Map<String, String> map) {
        List<OtherInfoModel> results = new ArrayList<>();
        for (String key : map.keySet()) {
            results.add(new OtherInfoModel(key, map.get(key)));
        }
        return results.stream().sorted().collect(Collectors.toList());
    }
}
