package uk.ac.cam.cares.jps.login;

import static android.app.Activity.RESULT_CANCELED;

import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import androidx.navigation.NavDeepLinkRequest;
import androidx.navigation.fragment.NavHostFragment;

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

import javax.inject.Inject;

import uk.ac.cam.cares.jps.loginmodule.R;

public class AuthorizationHelper {
    private AuthStateManager authStateManager;
    private AuthorizationService authService;
//    private static AuthorizationHelper instance;

    private AuthServerConfiguration configuration;

    private static final Logger LOGGER = LogManager.getLogger(AuthorizationHelper.class);

    public AuthorizationHelper(AuthStateManager authStateManager, AuthServerConfiguration configuration) {

        this.authStateManager = authStateManager;
        this.configuration = configuration;

//        executor = Executors.newSingleThreadExecutor();
//
//        AuthServerConfiguration config = AuthServerConfiguration.getInstance(context);
//        authService = new AuthorizationService(this.context,
//                new AppAuthConfiguration.Builder()
//                .setConnectionBuilder(configuration.getConnectionBuilder())
//                .setSkipIssuerHttpsCheck(true)
//                .build());
    }

//    public static synchronized AuthorizationHelper getInstance(Context context) {
//        if (instance == null) {
//            instance = new AuthorizationHelper(context);
//        }
//        return instance;
//    }

    public void performActionWithFreshTokens(AuthState.AuthStateAction action) {
        this.authStateManager.getCurrent().performActionWithFreshTokens(this.authService, action);
    }

    public String getUserInfoEndPoint() {
        try {
            return authStateManager.getCurrent().getAuthorizationServiceConfiguration().discoveryDoc.getUserinfoEndpoint().toString();
        } catch (NullPointerException e) {
            // no user endpoint found from the current logged in AuthServer
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

//    public ActivityResultLauncher<Intent> getLogoutLauncher(FragmentActivity activity) {
//        return activity.registerForActivityResult(
//                new ActivityResultContracts.StartActivityForResult(),
//                result -> {
//                    if (result.getResultCode() == RESULT_CANCELED) {
//                        Toast.makeText(activity, R.string.cancel_logout, Toast.LENGTH_SHORT).show();
//                    } else {
//                        authStateManager.clearSharedPref();
//                        //todo: fix the activity
//                        NavDeepLinkRequest request = NavDeepLinkRequest.Builder
//                                .fromUri(Uri.parse("android-app://uk.ac.cam.cares.jps.app/new_asset_result?assetinfo=" + serializeObjectToString(assetInfo) + "&operation=" + operation))
//                                .build();
//                        NavHostFragment.findNavController(activity).navigate(request);
//
//                        Intent loginIntent = new Intent(activity, LoginActivity.class);
//                        loginIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
//                        activity.startActivity(loginIntent);
//                        activity.finish();
//                    }
//                }
//        );
//
//    }

    public ActivityResultLauncher<Intent> getLogoutLauncher(Fragment fragment) {
        return fragment.registerForActivityResult(
                new ActivityResultContracts.StartActivityForResult(),
                result -> {
                    if (result.getResultCode() == RESULT_CANCELED) {
                        Toast.makeText(fragment.requireActivity(), R.string.cancel_logout, Toast.LENGTH_SHORT).show();
                    } else {
                        authStateManager.clearSharedPref();
                        //todo: fix the activity
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
