package uk.ac.cam.cares.jps.user.viewmodel;

import static uk.ac.cam.cares.jps.login.AccountException.CONNECTION_ERROR;
import static uk.ac.cam.cares.jps.login.AccountException.LOGIN_FAILURE;
import static uk.ac.cam.cares.jps.login.AccountException.SKEW_SYSTEM_CLOCK;

import android.content.Intent;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;
import uk.ac.cam.cares.jps.user.R;

/**
 * ViewModel that manages login related state
 */
@HiltViewModel
public class LoginViewModel extends ViewModel {
    private static final Logger LOGGER = LogManager.getLogger(LoginViewModel.class);

    private MutableLiveData<Boolean> hasLogin = new MutableLiveData<>(false);
    private MutableLiveData<Integer> toastErrorMessage = new MutableLiveData<>(-1);
    private MutableLiveData<Intent> loginIntent = new MutableLiveData<>(null);

    private LoginRepository repository;

    @Inject
    LoginViewModel(LoginRepository repository) {
        BasicConfigurator.configure();
        this.repository = repository;
    }

    /**
     * Prepare for authentication
     */
    public void initAuth() {
        repository.initAuth(new RepositoryCallback<>() {
            @Override
            public void onSuccess(Boolean result) {
                hasLogin.postValue(true);
            }

            @Override
            public void onFailure(Throwable error) {
                setToastErrorMessageValue(error);
            }
        });
    }

    /**
     * Perform authentication
     */
    public void doAuth() {
        repository.doAuth(new RepositoryCallback<>() {
            @Override
            public void onSuccess(Intent result) {
                loginIntent.postValue(result);
            }

            @Override
            public void onFailure(Throwable error) {
                setToastErrorMessageValue(error);
            }
        });
    }

    /**
     * Process authorization response
     * @param data
     */
    public void processAuthorizationResponse(Intent data) {
        repository.processAuthorizationResponse(data, new RepositoryCallback<>() {
            @Override
            public void onSuccess(Boolean result) {
                hasLogin.postValue(true);
            }

            @Override
            public void onFailure(Throwable error) {
                setToastErrorMessageValue(error);
            }
        });
    }

    private void setToastErrorMessageValue(Throwable error) {
        if (error.getMessage().equals(CONNECTION_ERROR)) {
            toastErrorMessage.postValue(R.string.connection_error);
        } else if (error.getMessage().equals(SKEW_SYSTEM_CLOCK)) {
            // todo: show dialog for skew clock?
            toastErrorMessage.postValue(R.string.login_failure_due_to_check_system_clock);
        } else if (error.getMessage().equals(LOGIN_FAILURE)) {
            toastErrorMessage.postValue(R.string.fail_to_login);
        }
    }

    public MutableLiveData<Boolean> getHasLogin() {
        return hasLogin;
    }

    public MutableLiveData<Integer> getToastErrorMessage() {
        return toastErrorMessage;
    }

    public MutableLiveData<Intent> getLoginIntent() {
        return loginIntent;
    }

}
