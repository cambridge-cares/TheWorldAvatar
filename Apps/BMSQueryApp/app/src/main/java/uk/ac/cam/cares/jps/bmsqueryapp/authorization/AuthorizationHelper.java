package uk.ac.cam.cares.jps.bmsqueryapp.authorization;

import android.content.Context;
import android.content.Intent;

import net.openid.appauth.AppAuthConfiguration;
import net.openid.appauth.AuthState;
import net.openid.appauth.AuthorizationService;
import net.openid.appauth.AuthorizationServiceConfiguration;
import net.openid.appauth.EndSessionRequest;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

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

    public void clearSharedPref() {
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

}
