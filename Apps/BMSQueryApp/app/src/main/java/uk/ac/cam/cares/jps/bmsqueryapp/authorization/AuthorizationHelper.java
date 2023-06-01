package uk.ac.cam.cares.jps.bmsqueryapp.authorization;

import android.content.Context;
import android.net.Uri;

import com.android.volley.Request;
import com.android.volley.toolbox.StringRequest;

import net.openid.appauth.AppAuthConfiguration;
import net.openid.appauth.AuthState;
import net.openid.appauth.AuthorizationService;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicReference;

import uk.ac.cam.cares.jps.bmsqueryapp.utils.SingletonConnection;

public class AuthorizationHelper {
    private AuthStateManager authStateManager;
    private AuthorizationService authService;
    private Executor executor;

    private final Context context;
    private static AuthorizationHelper instance;

    private static final Logger LOGGER = LogManager.getLogger(AuthorizationHelper.class);

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

    public String getUserInfoEndPoint() {
        try {
            return authStateManager.getCurrent().getAuthorizationServiceConfiguration().discoveryDoc.getUserinfoEndpoint().toString();
        } catch (NullPointerException e) {
            // todo: should call initializeAppAuth, need refactoring
            return "";
        }
    }

}
