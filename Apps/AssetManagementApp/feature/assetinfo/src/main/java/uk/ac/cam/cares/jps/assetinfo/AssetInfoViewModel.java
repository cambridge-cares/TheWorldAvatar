package uk.ac.cam.cares.jps.assetinfo;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import com.android.volley.Response;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.AssetInfo;
import uk.ac.cam.cares.jps.data.AssetInfoRepository;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.SettingRepository;

@HiltViewModel
public class AssetInfoViewModel extends ViewModel {

    static final Logger LOGGER = Logger.getLogger(AssetInfoViewModel.class);

    AssetInfoRepository assetInfoRepository;

    MutableLiveData<AssetInfo> assetInfo = new MutableLiveData<>();
    MutableLiveData<Integer> errorMessage = new MutableLiveData<>();

    @Inject
    AssetInfoViewModel(AssetInfoRepository assetInfoRepository) {
        BasicConfigurator.configure();
        this.assetInfoRepository = assetInfoRepository;
    }

    void getAssetInfoByIri(String assetUri) {
        RepositoryCallback<AssetInfo> callback = new RepositoryCallback<AssetInfo>() {
            @Override
            public void onSuccess(AssetInfo response) {
                assetInfo.postValue(response);
            }

            @Override
            public void onFailure(Throwable error) {
                LOGGER.error(error.getMessage());
                errorMessage.postValue(R.string.network_error);
            }
        };
        assetInfoRepository.getAssetInfoByIri(assetUri, callback);
    }

    MutableLiveData<AssetInfo> getAssetInfo() {
        return assetInfo;
    }

    public MutableLiveData<Integer> getErrorMessage() {
        return errorMessage;
    }
}
