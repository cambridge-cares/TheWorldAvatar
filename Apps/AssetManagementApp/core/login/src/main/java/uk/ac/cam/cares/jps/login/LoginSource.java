package uk.ac.cam.cares.jps.login;

import static uk.ac.cam.cares.jps.login.LoginErrorMessage.CONNECTION_ERROR;
import static uk.ac.cam.cares.jps.login.LoginErrorMessage.LOGIN_FAILURE;
import static uk.ac.cam.cares.jps.login.LoginErrorMessage.SKEW_SYSTEM_CLOCK;

import android.content.Context;
import android.content.Intent;

import androidx.annotation.Nullable;
import androidx.browser.customtabs.CustomTabsIntent;

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

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicReference;

import javax.inject.Inject;

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

    @Inject
    public LoginSource(Context context, AuthStateManager authStateManager, AuthServerConfiguration configuration) {
        this.authStateManager = authStateManager;
        this.configuration = configuration;
        this.context = context;
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
//            runOnUiThread(() -> Toast.makeText(this, "Connection failed, please check your network connection", Toast.LENGTH_SHORT).show());
                return;
            }

            LOGGER.info("Discovery document retrieved");
            authStateManager.replace(new AuthState(config));
            executor.submit(LoginSource.this::initializeClient);
        }
    }

    private void initializeClient() {
        LOGGER.info("Using static client ID: " + configuration.getClientId());
        clientId.set(configuration.getClientId());
        // todo: checkout why need to run on UI thread
//        runOnUiThread(this::initializeAuthRequest);
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


}
