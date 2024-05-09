package uk.ac.cam.cares.jps.bmsqueryapp;

import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.View;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.WorkerThread;
import androidx.appcompat.app.AppCompatActivity;
import androidx.browser.customtabs.CustomTabsIntent;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

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

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicReference;

import uk.ac.cam.cares.jps.bmsqueryapp.authorization.AuthServerConfiguration;
import uk.ac.cam.cares.jps.bmsqueryapp.authorization.AuthStateManager;
import uk.ac.cam.cares.jps.bmsqueryapp.databinding.ActivityLoginBinding;

public class LoginActivity extends AppCompatActivity {

    private static final Logger LOGGER = LogManager.getLogger(LoginActivity.class);

    private AuthorizationService authService;
    private AuthStateManager authStateManager;
    private AuthServerConfiguration configuration;

    private final AtomicReference<String> clientId = new AtomicReference<>();
    private final AtomicReference<AuthorizationRequest> authRequest = new AtomicReference<>();
    private final AtomicReference<CustomTabsIntent> authIntent = new AtomicReference<>();
    private CountDownLatch authIntentLatch = new CountDownLatch(1);
    private ExecutorService executor;

    private ActivityLoginBinding binding;

    @NonNull
    private BrowserMatcher browserMatcher = AnyBrowserMatcher.INSTANCE;
    private ActivityResultLauncher<Intent> authorizationLauncher;

    boolean doubleBackToExitPressedOnce = false;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        BasicConfigurator.configure();
        LOGGER.debug("onCreate called");

        binding = ActivityLoginBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        executor = Executors.newSingleThreadExecutor();
        authStateManager = AuthStateManager.getInstance(this);
        configuration = AuthServerConfiguration.getInstance(this);

        if (authStateManager.getCurrent().isAuthorized()
                && !configuration.hasConfigurationChanged()) {
            LOGGER.info("User is already authenticated, proceeding to main activity");
            startActivity(new Intent(this, MainActivity.class));
            finish();
            return;
        }

        if (configuration.hasConfigurationChanged()) {
            configuration.acceptConfiguration();
        }

        executor.submit(this::initializeAppAuth);

        binding.signInOrUpButton.setOnClickListener(view -> executor.submit(this::doAuth));

        authorizationLauncher = registerForActivityResult(
                new ActivityResultContracts.StartActivityForResult(),
                result -> {
                    if (result.getResultCode() == RESULT_CANCELED) {
                        runOnUiThread(() -> Toast.makeText(this, R.string.login_canceled, Toast.LENGTH_SHORT).show());
                    } else {
                        showLoading();
                        processAuthorizationResponse(result.getData());
                    }
                }
        );

    }

    @Override
    protected void onStart() {
        super.onStart();
        LOGGER.debug("onStart called");
        if (executor != null && executor.isShutdown()) {
            executor = Executors.newSingleThreadExecutor();
        }
    }

    @Override
    protected void onStop() {
        super.onStop();
        executor.shutdownNow();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();

        if (authService != null) {
            authService.dispose();
        }
    }

    @Override
    public void onBackPressed() {
        if (doubleBackToExitPressedOnce) {
            finishAffinity();
            return;
        }

        this.doubleBackToExitPressedOnce = true;
        Toast.makeText(this, "Please click BACK again to exit", Toast.LENGTH_SHORT).show();

        new Handler(Looper.getMainLooper()).postDelayed(() -> doubleBackToExitPressedOnce=false, 2000);
    }

    @WorkerThread
    private void initializeAppAuth() {
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
                this::handleConfigurationRetrievalResult,
                configuration.getConnectionBuilder());
    }

    private AuthorizationService createAuthorizationService() {
        LOGGER.info("Creating authorization service");
        AppAuthConfiguration.Builder builder = new AppAuthConfiguration.Builder();
        builder.setBrowserMatcher(browserMatcher);
        builder.setConnectionBuilder(configuration.getConnectionBuilder());
        builder.setSkipIssuerHttpsCheck(true);

        return new AuthorizationService(this, builder.build());
    }

    @WorkerThread
    private void handleConfigurationRetrievalResult(
            AuthorizationServiceConfiguration config,
            AuthorizationException ex) {
        if (config == null) {
            LOGGER.error("Failed to retrieve discovery document", ex);
            runOnUiThread(() -> Toast.makeText(this, "Connection failed, please check your network connection", Toast.LENGTH_SHORT).show());
            return;
        }

        LOGGER.info("Discovery document retrieved");
        authStateManager.replace(new AuthState(config));
        executor.submit(this::initializeClient);
    }

    @WorkerThread
    private void initializeClient() {
        LOGGER.info("Using static client ID: " + configuration.getClientId());
        clientId.set(configuration.getClientId());
        runOnUiThread(this::initializeAuthRequest);
    }

    @WorkerThread
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

    @WorkerThread
    private void doAuth() {
        LOGGER.info("doAuth");

        try {
            authIntentLatch.await();
        } catch (InterruptedException ex) {
            LOGGER.warn("Interrupted while waiting for auth intent");
        }

        Intent intent = authService.getAuthorizationRequestIntent(
                authRequest.get(),
                authIntent.get());
        authorizationLauncher.launch(intent);
    }

    private void processAuthorizationResponse(Intent data) {
        AuthorizationResponse response = AuthorizationResponse.fromIntent(data);
        AuthorizationException ex = AuthorizationException.fromIntent(data);

        if (response != null || ex != null) {
            authStateManager.updateAfterAuthorization(response, ex);
        }

        if (response != null && response.authorizationCode != null) {
            performTokenRequest(
                    response.createTokenExchangeRequest(),
                    this::handleCodeExchangeResponse);
        }
    }

    @MainThread
    private void performTokenRequest(
            TokenRequest request,
            AuthorizationService.TokenResponseCallback callback) {
        ClientAuthentication clientAuthentication;
        try {
            clientAuthentication = authStateManager.getCurrent().getClientAuthentication();
        } catch (ClientAuthentication.UnsupportedAuthenticationMethod ex) {
            LOGGER.error("Token request cannot be made, client authentication for the token "
                    + "endpoint could not be constructed (%s)", ex);
            Toast.makeText(this, R.string.fail_to_login, Toast.LENGTH_SHORT).show();
            return;
        }

        authService.performTokenRequest(
                request,
                clientAuthentication,
                callback);
    }

    @WorkerThread
    private void handleCodeExchangeResponse(
            @Nullable TokenResponse tokenResponse,
            @Nullable AuthorizationException authException) {
        authStateManager.updateAfterTokenResponse(tokenResponse, authException);
        hideLoading();
        if (!authStateManager.getCurrent().isAuthorized()) {
            if (authException.getCause() != null &&
                    authException.getCause().getMessage().matches("Issued at time is more than \\d+ minutes before or after the current time") &&
                    authException.code == 9) {
                // ID Token expired
                new MaterialAlertDialogBuilder(this)
                        .setTitle(R.string.fail_to_login_title)
                        .setMessage(R.string.login_failure_due_to_check_system_clock)
                        .setNegativeButton(R.string.ok, null)
                        .show();
                return;
            }

            // other reasons
            Toast.makeText(this, R.string.fail_to_login, Toast.LENGTH_SHORT).show();
        } else {
            Intent intent = new Intent(this, MainActivity.class);
            startActivity(intent);
            finish();
        }
    }

    private void showLoading() {
        binding.progressBar.setVisibility(View.VISIBLE);
        binding.signInOrUpButton.setVisibility(View.GONE);
    }

    private void hideLoading() {
        binding.progressBar.setVisibility(View.GONE);
        binding.signInOrUpButton.setVisibility(View.VISIBLE);
    }


}
