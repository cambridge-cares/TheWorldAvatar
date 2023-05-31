package uk.ac.cam.cares.jps.bmsqueryapp.authorization;

import android.content.Context;

import net.openid.appauth.AppAuthConfiguration;
import net.openid.appauth.AuthState;
import net.openid.appauth.AuthorizationService;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

public class AuthorizationHelper {
    private AuthStateManager authStateManager;
    private AuthorizationService authService;
    private Executor executor;

    private final Context context;
    private static AuthorizationHelper instance;

    private AuthorizationHelper(Context context) {
        this.context = context.getApplicationContext();

        authStateManager = AuthStateManager.getInstance(this.context);

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



}
