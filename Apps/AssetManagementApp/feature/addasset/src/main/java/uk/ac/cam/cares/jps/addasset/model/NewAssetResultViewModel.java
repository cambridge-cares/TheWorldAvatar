package uk.ac.cam.cares.jps.addasset.model;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.assetinfo.AssetInfo;
import uk.ac.cam.cares.jps.data.assetinfo.AssetInfoRepository;
import uk.ac.cam.cares.jps.data.RepositoryCallback;

@HiltViewModel
public class NewAssetResultViewModel extends ViewModel {
    AssetInfoRepository assetInfoRepository;
    private MutableLiveData<Boolean> isSuccess = new MutableLiveData<>();

    @Inject
    public NewAssetResultViewModel(AssetInfoRepository assetInfoRepository) {
        this.assetInfoRepository = assetInfoRepository;
    }

    public void addNewAsset(AssetInfo assetInfo) {
        assetInfoRepository.createNewAsset(assetInfo, new RepositoryCallback<Boolean>() {
            @Override
            public void onSuccess(Boolean result) {
                isSuccess.postValue(result);
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
