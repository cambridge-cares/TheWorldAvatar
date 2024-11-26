package uk.ac.cam.cares.jps.login;

import static android.app.Activity.RESULT_CANCELED;

import android.content.Intent;
import android.net.Uri;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.fragment.app.Fragment;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import net.openid.appauth.AuthorizationServiceConfiguration;
import net.openid.appauth.EndSessionRequest;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import javax.inject.Inject;

import kotlin.Pair;
import uk.ac.cam.cares.jps.loginmodule.R;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

public class LoginRepositoryImpl implements LoginRepository{
    private static final Logger LOGGER = LogManager.getLogger(LoginRepositoryImpl.class);
    private User user;

    LoginSource loginSource;

    @Inject
    public LoginRepositoryImpl(LoginSource loginSource) {
        this.loginSource = loginSource;
    }

    public void initAuth(RepositoryCallback<Boolean> callback) {
        loginSource.initLoginSource(callback);
    }

    public void doAuth(RepositoryCallback<Intent> callback) {
        loginSource.doAuth(callback);
    }

    public void processAuthorizationResponse(Intent data, RepositoryCallback<Boolean> repositoryCallback) {
        loginSource.processAuthorizationResponse(data, repositoryCallback);
    }

    public void getUserInfo(RepositoryCallback<User> repositoryCallback) {
        LOGGER.info("retrieving user info");
        if (!loginSource.hasConfigurationChanged() && user != null) {
            LOGGER.info("use preloaded user info");
            repositoryCallback.onSuccess(user);
            return;
        }

        RepositoryCallback<User> callback = new RepositoryCallback<User>() {
            @Override
            public void onSuccess(User result) {
                user = result;
                repositoryCallback.onSuccess(result);
            }

            @Override
            public void onFailure(Throwable error) {
                repositoryCallback.onFailure(error);
            }
        };
        loginSource.getUserInfo(callback);
    }

    public Intent getLogOutIntent() {
        AuthorizationServiceConfiguration config = loginSource.authStateManager.getCurrent().getAuthorizationServiceConfiguration();
        if (config.endSessionEndpoint != null) {
            Intent endSessionIntent = loginSource.authService.getEndSessionRequestIntent(
                    new EndSessionRequest.Builder(config)
                            .setIdTokenHint(loginSource.authStateManager.getCurrent().getIdToken())
                            .setPostLogoutRedirectUri(loginSource.configuration.getEndSessionRedirectUri())
                            .build());
            return endSessionIntent;
        }
        return null;
    }

    public ActivityResultLauncher<Intent> getLogoutLauncher(Fragment fragment, RepositoryCallback<Pair<Boolean, String>> callback) {
        return fragment.registerForActivityResult(
                new ActivityResultContracts.StartActivityForResult(),
                result -> {
                    if (result.getResultCode() == RESULT_CANCELED) {
                        callback.onFailure(new Throwable("cancelled"));
                    } else {
                        loginSource.authStateManager.clearLoginState();
                        String userId = user.getId();
                        user = null;
                        callback.onSuccess(new Pair<>(true, userId));
                    }
                }
        );

    }

    public MaterialAlertDialogBuilder getSessionExpiredDialog(Fragment fragment) {
        if (fragment == null || fragment.getContext() == null) {
            return null;
        }

        return new MaterialAlertDialogBuilder(fragment.getContext())
                .setTitle(R.string.session_expired_title)
                .setMessage(R.string.session_expired)
                .setPositiveButton(R.string.ok, (dialogInterface, i) -> {
                    loginSource.authStateManager.clearLoginState();
                    NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                            .fromUri(Uri.parse(fragment.getString(uk.ac.cam.cares.jps.utils.R.string.login_fragment_link)))
                            .build();
                    NavHostFragment.findNavController(fragment).navigate(request);
                });
    }
}
