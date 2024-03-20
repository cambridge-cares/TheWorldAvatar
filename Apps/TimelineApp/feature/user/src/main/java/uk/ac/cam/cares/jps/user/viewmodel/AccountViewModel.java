package uk.ac.cam.cares.jps.user.viewmodel;

import static uk.ac.cam.cares.jps.login.LoginErrorMessage.NO_UER_INFO_RETRIEVED;
import static uk.ac.cam.cares.jps.login.LoginErrorMessage.SESSION_EXPIRED;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.User;

@HiltViewModel
public class AccountViewModel extends ViewModel {
    private final LoginRepository loginRepository;
    private final Logger LOGGER = Logger.getLogger(AccountViewModel.class);

    private final MutableLiveData<String> _name = new MutableLiveData<>("");
    private final MutableLiveData<String> _email = new MutableLiveData<>("");
    private final MutableLiveData<Boolean> _shouldShowSessionExpired = new MutableLiveData<>(false);

    public LiveData<String> name = _name;
    public LiveData<String> email = _email;
    public LiveData<Boolean> shouldShowSessionExpired = _shouldShowSessionExpired;

    @Inject
    public AccountViewModel(LoginRepository loginRepository) {
        this.loginRepository = loginRepository;
    }

    public void getUserInfo() {
        loginRepository.getUserInfo(new uk.ac.cam.cares.jps.login.RepositoryCallback<User>() {
            @Override
            public void onSuccess(User user) {
                _email.postValue(user.getEmail());
                _name.postValue(user.getName());
            }

            @Override
            public void onFailure(Throwable error) {
                if (error.getMessage().equals(NO_UER_INFO_RETRIEVED) || error.getMessage().equals(SESSION_EXPIRED)) {
                    _shouldShowSessionExpired.postValue(true);
                }
            }
        });
    }

}
