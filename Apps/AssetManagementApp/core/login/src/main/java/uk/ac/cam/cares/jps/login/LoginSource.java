package uk.ac.cam.cares.jps.login;

import static uk.ac.cam.cares.jps.login.LoginErrorMessage.CONNECTION_ERROR;
import static uk.ac.cam.cares.jps.login.LoginErrorMessage.LOGIN_FAILURE;
import static uk.ac.cam.cares.jps.login.LoginErrorMessage.SKEW_SYSTEM_CLOCK;
import static uk.ac.cam.cares.jps.login.LoginErrorMessage.SESSION_EXPIRED;

import android.content.Context;
import android.content.Intent;

import androidx.annotation.Nullable;
import androidx.browser.customtabs.CustomTabsIntent;

import com.android.volley.Request;
import com.android.volley.toolbox.StringRequest;

import net.openid.appauth.AppAuthConfiguration;
import net.openid.appauth.AuthState;
import net.openid.appauth.AuthorizationException;
import net.openid.appauth.AuthorizationRequest;
import net.openid.appauth.AuthorizationResponse;
import net.openid.appauth.AuthorizationService;
import net.openid.appauth.AuthorizationServiceConfiguration;
import net.openid.appauth.ClientAuthentication;
import net.openid.appauth.ResponseTypeValues;
import net.openid.appauth.TokenRequest;
import net.openid.appauth.TokenResponse;
import net.openid.appauth.browser.AnyBrowserMatcher;
import net.openid.appauth.browser.BrowserMatcher;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicReference;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.network.Connection;

public class LoginSource {
    private static final Logger LOGGER = LogManager.getLogger(LoginSource.class);

    Context context;

    AuthorizationService authService;
    AuthStateManager authStateManager;
    AuthServerConfiguration configuration;

    private ExecutorService executor;
    private BrowserMatcher browserMatcher = AnyBrowserMatcher.INSTANCE;
    private final AtomicReference<String> clientId = new AtomicReference<>();
    private final AtomicReference<AuthorizationRequest> authRequest = new AtomicReference<>();
    private CountDownLatch authIntentLatch = new CountDownLatch(1);
    private final AtomicReference<CustomTabsIntent> authIntent = new AtomicReference<>();
    private Connection connection;

    private class  AuthConfigurationCallback implements AuthorizationServiceConfiguration.RetrieveConfigurationCallback {
        RepositoryCallback<Boolean> callback;
        AuthConfigurationCallback(RepositoryCallback<Boolean> callback) {
            this.callback = callback;
        }
        @Override
        public void onFetchConfigurationCompleted(@Nullable AuthorizationServiceConfiguration config, @Nullable AuthorizationException ex) {
            if (config == null) {
                LOGGER.error("Failed to retrieve discovery document", ex);
                callback.onFailure(new Throwable(CONNECTION_ERROR));
                return;
            }

            LOGGER.info("Discovery document retrieved");
            authStateManager.replace(new AuthState(config));
            executor.submit(LoginSource.this::initializeClient);
        }
    }

    @Inject
    public LoginSource(Context context, AuthStateManager authStateManager, AuthServerConfiguration configuration, Connection connection) {
        this.authStateManager = authStateManager;
        this.configuration = configuration;
        this.context = context;
        this.connection = connection;
        executor = Executors.newSingleThreadExecutor();

        if (this.configuration.hasConfigurationChanged()) {
            this.configuration.acceptConfiguration();
        }

    }

    public void initLoginSource(RepositoryCallback<Boolean> callback) {
        if (authStateManager.getCurrent().isAuthorized()
                && !configuration.hasConfigurationChanged()) {
            LOGGER.info("User is already authenticated, proceeding to main activity");
            callback.onSuccess(true);
        }

        executor.submit(() -> initializeAppAuth(callback));
    }

    private void initializeAppAuth(RepositoryCallback<Boolean> callback) {
        LOGGER.info("Initializing AppAuth");
        authService = createAuthorizationService();

        if (authStateManager.getCurrent().getAuthorizationServiceConfiguration() != null) {
            LOGGER.info("auth config already established");
            initializeClient();
            return;
        }

        LOGGER.info("Retrieving OpenID discovery doc");
        LOGGER.info("Discovery Uri: " + configuration.getDiscoveryUri());
        AuthorizationServiceConfiguration.fetchFromUrl(
                configuration.getDiscoveryUri(),
                new AuthConfigurationCallback(callback),
                configuration.getConnectionBuilder());
    }

    private AuthorizationService createAuthorizationService() {
        LOGGER.info("Creating authorization service");
        AppAuthConfiguration.Builder builder = new AppAuthConfiguration.Builder();
        builder.setBrowserMatcher(browserMatcher);
        builder.setConnectionBuilder(configuration.getConnectionBuilder());
        builder.setSkipIssuerHttpsCheck(true);

        return new AuthorizationService(context, builder.build());
    }

    private void initializeClient() {
        LOGGER.info("Using static client ID: " + configuration.getClientId());
        clientId.set(configuration.getClientId());
        initializeAuthRequest();
    }

    private void initializeAuthRequest() {
        createAuthRequest();
        warmUpBrowser();
    }

    private void createAuthRequest() {
        AuthorizationRequest.Builder authRequestBuilder = new AuthorizationRequest.Builder(
                authStateManager.getCurrent().getAuthorizationServiceConfiguration(),
                clientId.get(),
                ResponseTypeValues.CODE,
                configuration.getRedirectUri())
                .setScope(configuration.getScope());

        authRequest.set(authRequestBuilder.build());
    }

    private void warmUpBrowser() {
        authIntentLatch = new CountDownLatch(1);
        executor.execute(() -> {
            LOGGER.info("Warming up browser instance for auth request");
            CustomTabsIntent.Builder intentBuilder =
                    authService.createCustomTabsIntentBuilder(authRequest.get().toUri());
            authIntent.set(intentBuilder.build());
            authIntentLatch.countDown();
        });
    }

    public void doAuth(RepositoryCallback<Intent> callback) {
        LOGGER.info("doAuth");

        try {
            authIntentLatch.await();
        } catch (InterruptedException ex) {
            LOGGER.warn("Interrupted while waiting for auth intent");
        }

        Intent intent = authService.getAuthorizationRequestIntent(
            authRequest.get(),
            authIntent.get());
        callback.onSuccess(intent);
    }

    public void processAuthorizationResponse(Intent data, RepositoryCallback<Boolean> repositoryCallback) {
        AuthorizationResponse response = AuthorizationResponse.fromIntent(data);
        AuthorizationException ex = AuthorizationException.fromIntent(data);

        if (response != null || ex != null) {
            authStateManager.updateAfterAuthorization(response, ex);
        }

        if (response != null && response.authorizationCode != null) {
            performTokenRequest(
                    response.createTokenExchangeRequest(),
                    new TokenResponseCallback(repositoryCallback),
                    repositoryCallback);
        }
    }

    private class TokenResponseCallback implements AuthorizationService.TokenResponseCallback {

        RepositoryCallback<Boolean> repositoryCallback;

        TokenResponseCallback(RepositoryCallback<Boolean> repositoryCallback) {
            this.repositoryCallback = repositoryCallback;
        }

        @Override
        public void onTokenRequestCompleted(@Nullable TokenResponse tokenResponse, @Nullable AuthorizationException authException) {
            authStateManager.updateAfterTokenResponse(tokenResponse, authException);
            if (!authStateManager.getCurrent().isAuthorized()) {
                if (authException.getCause() != null &&
                        authException.getCause().getMessage().matches("Issued at time is more than \\d+ minutes before or after the current time") &&
                        authException.code == 9) {
                    // ID Token expired
                    repositoryCallback.onFailure(new Throwable(SKEW_SYSTEM_CLOCK));
                    return;
                }

                // other reasons
                repositoryCallback.onFailure(new Throwable(LOGIN_FAILURE));
            } else {
                repositoryCallback.onSuccess(true);
            }
        }
    }

    private void performTokenRequest(
            TokenRequest request,
            AuthorizationService.TokenResponseCallback tokenCallback,
            RepositoryCallback<Boolean> repositoryCallback) {
        ClientAuthentication clientAuthentication;
        try {
            clientAuthentication = authStateManager.getCurrent().getClientAuthentication();
        } catch (ClientAuthentication.UnsupportedAuthenticationMethod ex) {
            LOGGER.error("Token request cannot be made, client authentication for the token "
                    + "endpoint could not be constructed (%s)", ex);
            repositoryCallback.onFailure(new Throwable(LOGIN_FAILURE));
            return;
        }

        authService.performTokenRequest(
                request,
                clientAuthentication,
                tokenCallback);
    }

    public void performActionWithFreshTokens(AuthState.AuthStateAction action) {
        authStateManager.getCurrent().performActionWithFreshTokens(authService, action);
    }

    public void getUserInfo(RepositoryCallback<JSONObject> callback) {
        AuthState.AuthStateAction getUserInfoAction = (accessToken, idToken, ex) -> {
            if (ex != null) {
                LOGGER.warn("Failed to refresh access token. Reauthorization is needed.");
                callback.onFailure(new Throwable(SESSION_EXPIRED));
            }

            String userInfoEndpoint = getUserInfoEndPoint();
            StringRequest request = new StringRequest(Request.Method.GET, userInfoEndpoint,
                    response -> {
                        try {
                            JSONObject jsonResponse = new JSONObject(response);
                            callback.onSuccess(jsonResponse);
                        } catch (JSONException e) {
                            throw new RuntimeException(e);
                        }
                    },
                    error -> {
                        LOGGER.warn("No user info retrieved");
                        callback.onFailure(error);
                    }) {
                public Map<String, String> getHeaders() {
                    Map<String, String> params = new HashMap<>();
                    params.put("Content-Type", "application/json");
                    params.put("Authorization", "Bearer " + accessToken);
                    return params;
                }
            };
            connection.addToRequestQueue(request);
        };
        performActionWithFreshTokens(getUserInfoAction);
    }

    private String getUserInfoEndPoint() {
        try {
            return authStateManager.getCurrent().getAuthorizationServiceConfiguration().discoveryDoc.getUserinfoEndpoint().toString();
        } catch (NullPointerException e) {
            LOGGER.info("no user endpoint found from the current logged in AuthServer");
            return "";
        }
    }

}
