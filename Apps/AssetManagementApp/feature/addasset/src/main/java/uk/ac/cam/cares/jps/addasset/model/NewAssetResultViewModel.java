package uk.ac.cam.cares.jps.addasset.model;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.REFERENCE_LABEL;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.json.JSONException;
import org.json.JSONObject;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.model.AssetInfo;
import uk.ac.cam.cares.jps.data.assetinfo.AssetInfoRepository;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.qrprint.QRPrintRepository;
import uk.ac.cam.cares.jps.model.PrintItem;

@HiltViewModel
public class NewAssetResultViewModel extends ViewModel {
    AssetInfoRepository assetInfoRepository;
    QRPrintRepository qrPrintRepository;
    private MutableLiveData<Boolean> isSuccess = new MutableLiveData<>();

    @Inject
    public NewAssetResultViewModel(AssetInfoRepository assetInfoRepository, QRPrintRepository qrPrintRepository) {
        this.assetInfoRepository = assetInfoRepository;
        this.qrPrintRepository = qrPrintRepository;
    }

    public void addNewAsset() {
        AssetInfo assetInfo = assetInfoRepository.getAssetInfo();
        assetInfoRepository.createNewAsset(assetInfo, new RepositoryCallback<JSONObject>() {
            @Override
            public void onSuccess(JSONObject result) {
                isSuccess.postValue(true);
                try {
                    qrPrintRepository.updatePrintList(new PrintItem(
                            result.getString("ID"),
                            assetInfo.getProperty(REFERENCE_LABEL),
                            result.getString("deviceIRI")));
                } catch (JSONException e) {
                    throw new RuntimeException(e);
                }
            }

            @Override
            public void onFailure(Throwable error) {
                isSuccess.postValue(false);
            }
        });
    }

    public MutableLiveData<Boolean> getIsSuccess() {
        return isSuccess;
    }
}
