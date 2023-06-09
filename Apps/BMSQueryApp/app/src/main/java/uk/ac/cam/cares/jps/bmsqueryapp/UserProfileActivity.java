package uk.ac.cam.cares.jps.bmsqueryapp;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.content.res.ResourcesCompat;

import com.android.volley.Request;
import com.android.volley.toolbox.StringRequest;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import net.openid.appauth.AuthorizationException;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.Map;

import uk.ac.cam.cares.jps.bmsqueryapp.authorization.AuthorizationHelper;
import uk.ac.cam.cares.jps.bmsqueryapp.databinding.ActivityUserProfileBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.SingletonConnection;

public class UserProfileActivity extends AppCompatActivity {

    private static final Logger LOGGER = LogManager.getLogger(UserProfileActivity.class);
    ActivityUserProfileBinding binding;

    private AuthorizationHelper authHelper;
    private String KEY_EMAIL = "email";
    private String KEY_GIVEN_NAME = "given_name";
    private String KEY_FAMILY_NAME = "family_name";
    private ActivityResultLauncher<Intent> logoutLauncher;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        BasicConfigurator.configure();

        binding = ActivityUserProfileBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        authHelper = AuthorizationHelper.getInstance(this);

        binding.email.label.setText(R.string.email);
        binding.firstname.label.setText(R.string.firstname);
        binding.lastname.label.setText(R.string.lastname);

        authHelper.performActionWithFreshTokens(this::retrieveUserInfo);
        logoutLauncher = authHelper.getLogoutLauncher(this);

        binding.logout.content.setText(R.string.logout);
        binding.logout.getRoot().setBackground(ResourcesCompat.getDrawable(getResources(), R.drawable.ripple_item_background, null));
        binding.logout.getRoot().setOnClickListener(v -> logoutLauncher.launch(authHelper.getLogOutIntent()));

        binding.updatePw.content.setText(R.string.updatePassword);
        binding.updatePw.getRoot().setBackground(ResourcesCompat.getDrawable(getResources(), R.drawable.ripple_item_background, null));
        binding.updatePw.getRoot().setOnClickListener(v -> {
            // todo: trigger update password
        });

        binding.returnButton.setOnClickListener(v -> finish());
    }

    public void retrieveUserInfo(String accessToken, String idToken, AuthorizationException ex) {
        if (ex != null) {
            LOGGER.error("Failed to refresh access token. Reauthorization is needed.");
            new MaterialAlertDialogBuilder(this)
                    .setTitle(R.string.session_expired_title)
                    .setMessage(R.string.session_expired)
                    .setPositiveButton(R.string.ok, (dialogInterface, i) -> {
                        startActivity(new Intent(this, LoginActivity.class));
                        finish();
                    })
                    .show();
            return;
        }

        String userInfoEndpoint = authHelper.getUserInfoEndPoint();
        StringRequest request = new StringRequest(Request.Method.GET, userInfoEndpoint,
                response -> {
                    try {
                        JSONObject jsonResponse = new JSONObject(response);
                        if (jsonResponse.has(KEY_EMAIL)) {
                            binding.email.content.setText(jsonResponse.getString(KEY_EMAIL));
                        }
                        if (jsonResponse.has(KEY_GIVEN_NAME)) {
                            binding.firstname.content.setText(jsonResponse.getString(KEY_GIVEN_NAME));
                        }
                        if (jsonResponse.has(KEY_FAMILY_NAME)) {
                            binding.lastname.content.setText(jsonResponse.getString(KEY_FAMILY_NAME));
                        }
                    } catch (JSONException e) {
                        throw new RuntimeException(e);
                    }
                },
                error -> {
                    LOGGER.warn("No user info retrieved");
                    LOGGER.warn(error.getMessage());
                }) {
            public Map<String, String> getHeaders() {
                Map<String, String> params = new HashMap<>();
                params.put("Content-Type", "application/json");
                params.put("Authorization", "Bearer " + accessToken);
                return params;
            }
        };
        SingletonConnection.getInstance(this).addToRequestQueue(request);
    }
}
