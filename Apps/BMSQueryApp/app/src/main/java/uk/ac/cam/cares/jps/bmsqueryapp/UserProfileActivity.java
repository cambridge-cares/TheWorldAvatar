package uk.ac.cam.cares.jps.bmsqueryapp;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.widget.Toast;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;

import com.android.volley.Request;
import com.android.volley.toolbox.StringRequest;

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

    private static final int END_SESSION_REQUEST_CODE = 911;

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

        binding.logout.content.setText(R.string.logout);
        binding.logout.getRoot().setOnClickListener(v -> {
            startActivityForResult(authHelper.getLogOutIntent(), END_SESSION_REQUEST_CODE);
        });

        binding.updatePw.content.setText(R.string.updatePassword);
        binding.updatePw.getRoot().setOnClickListener(v -> {
            // todo: trigger update password
        });

        binding.returnButton.setOnClickListener(v -> finish());
    }

    public void retrieveUserInfo(String accessToken, String idToken, AuthorizationException ex) {
        if (ex != null) {
            LOGGER.error("Failed to refresh access token. Reauthorization is needed.");
            startActivity(new Intent(this, LoginActivity.class));
            finish();
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

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        if (requestCode == END_SESSION_REQUEST_CODE && resultCode == Activity.RESULT_OK) {
            authHelper.clearSharedPref();
            Intent loginIntent = new Intent(this, LoginActivity.class);
            loginIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(loginIntent);
            finish();
        } else {
            Toast.makeText(this, R.string.cancel_logout, Toast.LENGTH_SHORT).show();
        }
    }
}
