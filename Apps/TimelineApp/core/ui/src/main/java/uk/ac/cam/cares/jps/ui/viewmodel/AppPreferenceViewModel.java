package uk.ac.cam.cares.jps.ui.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.AppPreferenceRepository;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

@HiltViewModel
public class AppPreferenceViewModel extends ViewModel {
    private final AppPreferenceRepository appPreferenceRepository;
    private MutableLiveData<Boolean> _autoStart = new MutableLiveData<>(null);
    public LiveData<Boolean> autoStart = _autoStart;

    private MutableLiveData<String> _uploadDuration = new MutableLiveData<>("15 minutes");
    public LiveData<String> uploadDuration = _uploadDuration;

    private MutableLiveData<Boolean> _skipTooltips = new MutableLiveData<>(null);
    public LiveData<Boolean> skipTooltips = _skipTooltips;

    private MutableLiveData<Boolean> _locationPermissionPrompted = new MutableLiveData<>(null);
    public LiveData<Boolean> locationPermissionPrompted = _locationPermissionPrompted;

    private MutableLiveData<String> _accountError = new MutableLiveData<>("");
    public LiveData<String> accountError = _accountError;

    @Inject
    public AppPreferenceViewModel(AppPreferenceRepository appPreferenceRepository) {
        this.appPreferenceRepository = appPreferenceRepository;
    }

    public void loadAllPreferences() {
        loadAutoStart();
        loadSkipTooltips();
        loadUploadDuration();
    }

    private void loadAutoStart() {
        RepositoryCallback<Boolean> callback = new RepositoryCallback<Boolean>() {
            @Override
            public void onSuccess(Boolean result) {
                _autoStart.postValue(result);
            }

            @Override
            public void onFailure(Throwable error) {
                _accountError.postValue("Account error.");
            }
        };
        appPreferenceRepository.getAutoStart(callback);
    }

    public void setAutoStart(Boolean val) {
        _autoStart.setValue(val);
        appPreferenceRepository.setAutoStart(_autoStart.getValue());
    }

    private void loadUploadDuration() {
        RepositoryCallback<String> callback = new RepositoryCallback<>() {
            @Override
            public void onSuccess(String result) {
                _uploadDuration.postValue(result);
            }

            @Override
            public void onFailure(Throwable error) {
                _accountError.postValue("Account error.");
            }
        };
        appPreferenceRepository.getUploadDuration(callback);
    }

    public void setUploadDuration(String duration) {
        _uploadDuration.setValue(duration);
        appPreferenceRepository.setUploadDuration(duration);
    }

    private void loadSkipTooltips() {
        RepositoryCallback<Boolean> callback = new RepositoryCallback<>() {
            @Override
            public void onSuccess(Boolean result) {
                _skipTooltips.postValue(result);
            }

            @Override
            public void onFailure(Throwable error) {
                _accountError.postValue("Account error.");
            }
        };
        appPreferenceRepository.getTooltipSkipped(callback);
    }

    public void setSkipTooltips(Boolean skip) {
        _skipTooltips.setValue(skip);
        appPreferenceRepository.setTooltipSkipped(skip);
    }

    private void loadLocationPermissionPrompted() {
        RepositoryCallback<Boolean> callback = new RepositoryCallback<>() {
            @Override
            public void onSuccess(Boolean result) {
                _locationPermissionPrompted.postValue(true);
            }

            @Override
            public void onFailure(Throwable error) {
                _accountError.postValue("Account error.");
            }
        };
        appPreferenceRepository.getLocationPermissionPrompted(callback);
    }

    public void setLocationPermissionPrompted() {
        _locationPermissionPrompted.setValue(true);
        appPreferenceRepository.setLocationPermissionPrompted(true);
    }

}
