package uk.ac.cam.cares.jps.user.viewmodel;

import static uk.ac.cam.cares.jps.login.AccountException.NO_UER_INFO_RETRIEVED;
import static uk.ac.cam.cares.jps.login.AccountException.SESSION_EXPIRED;

import android.content.Intent;

import androidx.activity.result.ActivityResultLauncher;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import org.apache.log4j.Logger;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import kotlin.Pair;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.User;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

/**
 * ViewModel that manages user account information for UI
 */
@HiltViewModel
public class AccountViewModel extends ViewModel {
    private final LoginRepository loginRepository;
    private final Logger LOGGER = Logger.getLogger(AccountViewModel.class);

    private final MutableLiveData<String> _name = new MutableLiveData<>("");
    private final MutableLiveData<String> _email = new MutableLiveData<>("");
    private final MutableLiveData<Boolean> _shouldShowSessionExpired = new MutableLiveData<>(false);
    private final MutableLiveData<Pair<Boolean, String>> _logoutStatus = new MutableLiveData<>();

    public LiveData<String> name = _name;
    public LiveData<String> email = _email;
    public LiveData<Boolean> shouldShowSessionExpired = _shouldShowSessionExpired;
    public LiveData<Pair<Boolean, String>> logoutStatus = _logoutStatus;

    private ActivityResultLauncher<Intent> logoutLauncher;

    @Inject
    public AccountViewModel(LoginRepository loginRepository) {
        this.loginRepository = loginRepository;
    }

    /**
     * Get user info from server by calling repository
     */
    public void getUserInfo() {
        loginRepository.getUserInfo(new RepositoryCallback<>() {
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

    /**
     * Register the fragment for logout result and set up the logout launcher object
     * @param fragment Host fragment
     */
    public void registerForLogoutResult(Fragment fragment) {
        logoutLauncher = loginRepository.getLogoutLauncher(fragment, new RepositoryCallback<>() {
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

    /**
     * Logout the current user
     */
    public void logout() {
        logoutLauncher.launch(loginRepository.getLogOutIntent());
    }

    /**
     * get session expired dialog
     * @param fragment Host fragment
     * @return session expired dialog
     */
    public MaterialAlertDialogBuilder getSessionExpiredDialog(Fragment fragment) {
        return loginRepository.getSessionExpiredDialog(fragment);
    }

}
