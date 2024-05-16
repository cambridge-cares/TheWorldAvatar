package uk.ac.cam.cares.jps.timeline.viewmodel;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;
import uk.ac.cam.cares.jps.sensor.UserPhoneRepository;

@HiltViewModel
public class UserPhoneViewModel extends ViewModel {
    UserPhoneRepository userPhoneRepository;
    private final MutableLiveData<Boolean> _hasAccountError = new MutableLiveData<>();
    private final LiveData<Boolean> hasAccountError = _hasAccountError;

    @Inject
    public UserPhoneViewModel(UserPhoneRepository userPhoneRepository) {
        this.userPhoneRepository = userPhoneRepository;
    }

    public void registerPhoneToUser() {
        userPhoneRepository.registerAppToUser(new RepositoryCallback<>() {
            @Override
            public void onSuccess(Boolean result) {
                // do nothing
            }

            @Override
            public void onFailure(Throwable error) {
                _hasAccountError.setValue(true);
            }
        });
    }
}
