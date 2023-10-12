package uk.ac.cam.cares.jps.login;

import static android.app.Activity.RESULT_CANCELED;

import android.content.Intent;
import android.net.Uri;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.fragment.app.Fragment;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import net.openid.appauth.AuthState;
import net.openid.appauth.AuthorizationServiceConfiguration;
import net.openid.appauth.EndSessionRequest;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.loginmodule.R;

public class LoginRepository {
    private static final Logger LOGGER = LogManager.getLogger(LoginRepository.class);

    LoginSource loginSource;

    @Inject
    public LoginRepository(LoginSource loginSource) {
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

    public void performActionWithFreshTokens(AuthState.AuthStateAction action) {
        loginSource.authStateManager.getCurrent().performActionWithFreshTokens(loginSource.authService, action);
    }

    public String getUserInfoEndPoint() {
        try {
            return loginSource.authStateManager.getCurrent().getAuthorizationServiceConfiguration().discoveryDoc.getUserinfoEndpoint().toString();
        } catch (NullPointerException e) {
            LOGGER.info("no user endpoint found from the current logged in AuthServer");
            return "";
        }
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

    public ActivityResultLauncher<Intent> getLogoutLauncher(Fragment fragment) {
        return fragment.registerForActivityResult(
                new ActivityResultContracts.StartActivityForResult(),
                result -> {
                    if (result.getResultCode() == RESULT_CANCELED) {
                        Toast.makeText(fragment.requireActivity(), R.string.cancel_logout, Toast.LENGTH_SHORT).show();
                    } else {
                        loginSource.authStateManager.clearSharedPref();
                        NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                                .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/login"))
                                .build();
                        NavHostFragment.findNavController(fragment).navigate(request);

//                        Intent loginIntent = new Intent(fragment.requireActivity(), LoginActivity.class);
//                        loginIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
//                        fragment.startActivity(loginIntent);
                    }
                }
        );

    }

    public void showSessionExpiredDialog(Fragment fragment) {
        new MaterialAlertDialogBuilder(fragment.getContext())
                .setTitle(R.string.session_expired_title)
                .setMessage(R.string.session_expired)
                .setPositiveButton(R.string.ok, (dialogInterface, i) -> {
                    //todo: fix the activity
                    NavDeepLinkRequest request = NavDeepLinkRequest.Builder
                            .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/login"))
                            .build();
                    NavHostFragment.findNavController(fragment).navigate(request);

//                    activity.startActivity(new Intent(activity, LoginActivity.class));
//                    activity.finish();
                })
                .show();
    }
}
