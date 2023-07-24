package uk.ac.cam.cares.jps.assetinfo;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import com.android.volley.Response;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.AssetInfo;
import uk.ac.cam.cares.jps.data.AssetInfoRepository;

@HiltViewModel
public class AssetInfoViewModel extends ViewModel {

    static final Logger LOGGER = Logger.getLogger(AssetInfoViewModel.class);

    AssetInfoRepository repository;

    MutableLiveData<AssetInfo> assetInfo = new MutableLiveData<>();
    MutableLiveData<String> error = new MutableLiveData<>();

    @Inject
    AssetInfoViewModel(AssetInfoRepository repository) {
        BasicConfigurator.configure();
        this.repository = repository;
    }

    void getAssetInfoByIri(String assetUri) {
        Response.Listener<AssetInfo> onSuccess = response -> assetInfo.postValue(response);

        Response.ErrorListener onError = error -> {
            LOGGER.error(error.getMessage());
            this.error.postValue(String.valueOf(R.string.netowrk_error));
        };
        repository.getAssetInfoByIri(assetUri, onSuccess, onError);
    }

    MutableLiveData<AssetInfo> getAssetInfo() {
        return assetInfo;
    }
}
