package uk.ac.cam.cares.jps.ui.viewmodel;

import android.content.Context;
import android.content.Intent;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import kotlin.Pair;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.User;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

/**
 * ViewModel to expose user account information (name and email) for UI,
 * and handle logout status and session expired flag.
 * Minimal, suitable for reuse across modules.
 */
@HiltViewModel
public class UserAccountViewModel extends ViewModel {

    private final LoginRepository loginRepository;
    private ActivityResultLauncher<Intent> logoutLauncher;

    private final MutableLiveData<String> _name = new MutableLiveData<>("");
    private final MutableLiveData<String> _email = new MutableLiveData<>("");
    private final MutableLiveData<Boolean> _shouldShowSessionExpired = new MutableLiveData<>(false);
    private final MutableLiveData<Pair<Boolean, String>> _logoutStatus = new MutableLiveData<>();

    public LiveData<String> name = _name;
    public LiveData<String> email = _email;
    public LiveData<Boolean> shouldShowSessionExpired = _shouldShowSessionExpired;
    public LiveData<Pair<Boolean, String>> logoutStatus = _logoutStatus;

    @Inject
    public UserAccountViewModel(LoginRepository loginRepository) {
        this.loginRepository = loginRepository;
    }

    public void fetchAndSetUserInfo() {
        loginRepository.getUserInfo(new RepositoryCallback<User>() {
            @Override
            public void onSuccess(User user) {
                _name.postValue(user.getName());
                _email.postValue(user.getEmail());
            }

            @Override
            public void onFailure(Throwable error) {
                _shouldShowSessionExpired.postValue(true);
            }
        });
    }

    public void registerForLogoutResult(Fragment fragment) {
        logoutLauncher = loginRepository.getLogoutLauncher(fragment, new RepositoryCallback<Pair<Boolean, String>>() {
            @Override
            public void onSuccess(Pair<Boolean, String> result) {
                _logoutStatus.postValue(result);
            }

            @Override
            public void onFailure(Throwable error) {
                _logoutStatus.postValue(new Pair<>(false, ""));
            }
        });
    }

    public void logout() {
        if (logoutLauncher != null) {
            Intent intent = loginRepository.getLogOutIntent();
            if (intent != null) {
                logoutLauncher.launch(intent);
            } else {
                _logoutStatus.postValue(new Pair<>(false, ""));
            }
        } else {
            _logoutStatus.postValue(new Pair<>(false, ""));
        }
    }

    public void clearLogoutStatus() {
        _logoutStatus.setValue(null);
    }

}
