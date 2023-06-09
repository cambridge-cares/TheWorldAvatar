package uk.ac.cam.cares.jps.bmsqueryapp.authorization;

import static android.app.Activity.RESULT_CANCELED;

import android.content.Context;
import android.content.Intent;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import net.openid.appauth.AppAuthConfiguration;
import net.openid.appauth.AuthState;
import net.openid.appauth.AuthorizationService;
import net.openid.appauth.AuthorizationServiceConfiguration;
import net.openid.appauth.EndSessionRequest;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import uk.ac.cam.cares.jps.bmsqueryapp.LoginActivity;
import uk.ac.cam.cares.jps.bmsqueryapp.R;

public class AuthorizationHelper {
    private AuthStateManager authStateManager;
    private AuthorizationService authService;
    private Executor executor;

    private final Context context;
    private static AuthorizationHelper instance;

    private AuthServerConfiguration configuration;

    private static final Logger LOGGER = LogManager.getLogger(AuthorizationHelper.class);

    private AuthorizationHelper(Context context) {
        this.context = context.getApplicationContext();

        authStateManager = AuthStateManager.getInstance(this.context);
        configuration = AuthServerConfiguration.getInstance(this.context);

        executor = Executors.newSingleThreadExecutor();

        AuthServerConfiguration config = AuthServerConfiguration.getInstance(context);
        authService = new AuthorizationService(this.context,
                new AppAuthConfiguration.Builder()
                .setConnectionBuilder(config.getConnectionBuilder())
                .setSkipIssuerHttpsCheck(true)
                .build());
    }

    public static synchronized AuthorizationHelper getInstance(Context context) {
        if (instance == null) {
            instance = new AuthorizationHelper(context);
        }
        return instance;
    }

    public void performActionWithFreshTokens(AuthState.AuthStateAction action) {
        instance.authStateManager.getCurrent().performActionWithFreshTokens(instance.authService, action);
    }

    public String getUserInfoEndPoint() {
        try {
            return authStateManager.getCurrent().getAuthorizationServiceConfiguration().discoveryDoc.getUserinfoEndpoint().toString();
        } catch (NullPointerException e) {
            // todo: should call initializeAppAuth, need refactoring
            return "";
        }
    }

    public Intent getLogOutIntent() {
        AuthorizationServiceConfiguration config = authStateManager.getCurrent().getAuthorizationServiceConfiguration();
        if (config.endSessionEndpoint != null) {
            Intent endSessionIntent = authService.getEndSessionRequestIntent(
                    new EndSessionRequest.Builder(config)
                            .setIdTokenHint(authStateManager.getCurrent().getIdToken())
                            .setPostLogoutRedirectUri(configuration.getEndSessionRedirectUri())
                            .build());
            return endSessionIntent;
        }
        return null;
    }

    // todo: should be moved to AuthStateManager
    private void clearSharedPref() {
        // discard the authorization and token state, but retain the configuration and
        // dynamic client registration (if applicable), to save from retrieving them again.
        AuthState currentState = authStateManager.getCurrent();
        AuthState clearedState =
                new AuthState(currentState.getAuthorizationServiceConfiguration());
        if (currentState.getLastRegistrationResponse() != null) {
            clearedState.update(currentState.getLastRegistrationResponse());
        }
        authStateManager.replace(clearedState);
    }

    public ActivityResultLauncher<Intent> getLogoutLauncher(FragmentActivity activity) {
        return activity.registerForActivityResult(
                new ActivityResultContracts.StartActivityForResult(),
                result -> {
                    if (result.getResultCode() == RESULT_CANCELED) {
                        Toast.makeText(activity, R.string.cancel_logout, Toast.LENGTH_SHORT).show();
                    } else {
                        clearSharedPref();
                        Intent loginIntent = new Intent(activity, LoginActivity.class);
                        loginIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                        activity.startActivity(loginIntent);
                        activity.finish();
                    }
                }
        );

    }

    public ActivityResultLauncher<Intent> getLogoutLauncher(Fragment fragment) {
        return fragment.registerForActivityResult(
                new ActivityResultContracts.StartActivityForResult(),
                result -> {
                    if (result.getResultCode() == RESULT_CANCELED) {
                        Toast.makeText(fragment.requireActivity(), R.string.cancel_logout, Toast.LENGTH_SHORT).show();
                    } else {
                        clearSharedPref();
                        Intent loginIntent = new Intent(fragment.requireActivity(), LoginActivity.class);
                        loginIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                        fragment.startActivity(loginIntent);
                    }
                }
        );

    }

    public void showSessionExpiredDialog(FragmentActivity activity) {
        new MaterialAlertDialogBuilder(activity)
                .setTitle(R.string.session_expired_title)
                .setMessage(R.string.session_expired)
                .setPositiveButton(R.string.ok, (dialogInterface, i) -> {
                    activity.startActivity(new Intent(activity, LoginActivity.class));
                    activity.finish();
                })
                .show();
    }

}
