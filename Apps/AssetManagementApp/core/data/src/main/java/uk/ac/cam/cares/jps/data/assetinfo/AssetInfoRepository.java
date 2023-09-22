package uk.ac.cam.cares.jps.data.assetinfo;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.HAS_TIME_SERIES;

import android.os.Handler;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import javax.inject.Inject;

import io.reactivex.Completable;
import io.reactivex.disposables.Disposable;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.setting.SettingRepository;
import uk.ac.cam.cares.jps.network.assetinfo.AssetNetworkSource;

public class AssetInfoRepository {
    private final Logger LOGGER = Logger.getLogger(AssetInfoRepository.class);

    AssetNetworkSource assetInfoNetworkSource;
    SettingRepository settingRepository;
    List<String> visibleProperties = new ArrayList<>();
    AssetInfo assetInfo;
    Random random = new Random();

    @Inject
    public AssetInfoRepository(AssetNetworkSource assetInfoNetworkSource, SettingRepository settingRepository) {
        this.assetInfoNetworkSource = assetInfoNetworkSource;
        this.settingRepository = settingRepository;
    }

    public void getAssetInfoByIri(String iri, RepositoryCallback<AssetInfo> callback) {
        Completable assetNetworkCall = Completable.create(emitter -> assetInfoNetworkSource.getAssetInfoByIri(iri, asset -> {
            assetInfo = new AssetInfo(asset);
            emitter.onComplete();
        }, emitter::onError));

        Completable settingLocalCall = Completable.create(emitter -> settingRepository.getSettings(new RepositoryCallback<Map<String, Integer>>() {
            @Override
            public void onSuccess(Map<String, Integer> result) {
                visibleProperties.clear();
                for (Map.Entry<String, Integer> entry : result.entrySet()) {
                    if (entry.getValue() == 1) {
                        visibleProperties.add(entry.getKey());
                    }
                }
                emitter.onComplete();
            }

            @Override
            public void onFailure(Throwable error) {
                LOGGER.error(error.getMessage());
                // use default settings
                visibleProperties = new ArrayList<>(settingRepository.getDefaultSettings().keySet());
                emitter.onComplete();
            }
        }));

        Completable combinedCompletable = Completable.mergeArray(assetNetworkCall, settingLocalCall);
        Disposable disposable = combinedCompletable.subscribe(
                () -> {
                    LOGGER.info("Both asset and setting async call are return");
                    AssetInfo newAssetInfo = getAssetInfoWithVisibleProperties();
                    callback.onSuccess(newAssetInfo);
                },
                error -> {
                    LOGGER.error(error.getMessage());
                    callback.onFailure(error);
                }
        );
    }

    private AssetInfo getAssetInfoWithVisibleProperties() {
        AssetInfo result = new AssetInfo();
        for (String visibleProperties : visibleProperties) {
            if (assetInfo.getProperty(visibleProperties) != null) {
                result.addProperties(visibleProperties, assetInfo.getProperty(visibleProperties));
            }
        }
        result.addProperties(HAS_TIME_SERIES, assetInfo.getProperty(HAS_TIME_SERIES));
        return  result;
    }

    public void createNewAsset(AssetInfo assetInfo, RepositoryCallback<JSONObject> callback) {
        //        callback.onFailure(new Exception());

        // test data for qr code printing
        Handler handler = new Handler();
        handler.postDelayed((Runnable) () -> {
            JSONObject successMessage = new JSONObject();
            try {
                successMessage.put("iri", "iri");
                successMessage.put("inventoryID", random.nextInt());
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }

            callback.onSuccess(successMessage);
        }, 2000);
    }
}