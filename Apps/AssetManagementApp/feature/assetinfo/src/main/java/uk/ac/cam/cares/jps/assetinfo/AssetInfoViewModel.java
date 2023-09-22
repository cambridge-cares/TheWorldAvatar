package uk.ac.cam.cares.jps.assetinfo;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.assetinfo.AssetInfo;
import uk.ac.cam.cares.jps.data.assetinfo.AssetInfoRepository;
import uk.ac.cam.cares.jps.data.RepositoryCallback;

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
                // todo: should separate the type of error and show the corresponding error message
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
