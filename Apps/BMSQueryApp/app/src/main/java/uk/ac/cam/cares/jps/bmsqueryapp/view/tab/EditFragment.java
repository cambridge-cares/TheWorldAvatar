package uk.ac.cam.cares.jps.bmsqueryapp.view.tab;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.InputMethodManager;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.volley.AuthFailureError;
import com.android.volley.Request;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.JsonObjectRequest;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import net.openid.appauth.AuthorizationException;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import okhttp3.HttpUrl;
import uk.ac.cam.cares.jps.bmsqueryapp.R;
import uk.ac.cam.cares.jps.bmsqueryapp.adapter.list.EditableAttributesAdapter;
import uk.ac.cam.cares.jps.bmsqueryapp.authorization.AuthorizationHelper;
import uk.ac.cam.cares.jps.bmsqueryapp.data.attribute.EditableAttribute;
import uk.ac.cam.cares.jps.bmsqueryapp.data.dict.IRIMapping;
import uk.ac.cam.cares.jps.bmsqueryapp.databinding.FragmentEditBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.Constants;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.SingletonConnection;

public class EditFragment extends Fragment {

    private static final Logger LOGGER = LogManager.getLogger(EditFragment.class);

    private FragmentEditBinding binding;

    private HttpUrl.Builder ESPHOME_CONTROL_URL;

    private HttpUrl.Builder WACNET_WRITE_URL;
    private List<EditableAttribute> editableAttributes = new ArrayList<>();

    private AuthorizationHelper authHelper;
    private ActivityResultLauncher<Intent> logoutLauncher;
    private Context applicationContext;

    public EditFragment() {
        super();
    }

    public EditFragment(List<EditableAttribute> editableAttributes) {
        super();
        this.editableAttributes = editableAttributes;
    }

    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        binding = FragmentEditBinding.inflate(inflater, container, false);
        BasicConfigurator.configure();

        ESPHOME_CONTROL_URL = Constants.constructUrlBuilder(requireContext().getString(R.string.lab_host), getResources().getString(R.string.bms_update_agent_set));
        WACNET_WRITE_URL = Constants.constructUrlBuilder(requireContext().getString(R.string.bms_update_agent_wacnet_write), requireContext());

        // the applicationContext is only used to show update success or failure after fragment detached
        applicationContext = requireActivity().getApplicationContext();

        authHelper = AuthorizationHelper.getInstance(getContext());
        logoutLauncher = authHelper.getLogoutLauncher(this);

        EditableAttributesAdapter attributeListAdapter = new EditableAttributesAdapter(editableAttributes);
        binding.editableAttributeRv.setLayoutManager(new LinearLayoutManager(getContext()));
        binding.editableAttributeRv.setAdapter(attributeListAdapter);

        if (editableAttributes.size() > 0) {
            binding.noEditableAttrTv.setVisibility(View.GONE);
            binding.editableAttributeScrollView.setVisibility(View.VISIBLE);
        }
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        binding.submitButton.setOnClickListener(view1 -> {
            try {
                String dataIRI = editableAttributes.get(0).getIri();
                if (dataIRI.equals("https://www.theworldavatar.com/kg/ontodevice/V_Setpoint-01-Temperature")) {
                    authHelper.performActionWithFreshTokens(this::createEditTemperatureRequest);
                } else if (dataIRI.equals(requireContext().getString(R.string.VAV_E7_1_flow_sp))
                        || dataIRI.equals(requireContext().getString(R.string.VAV_E7_2_flow_sp))
                        || dataIRI.equals(requireContext().getString(R.string.CH_7_7_CAV_flow_sp))) {
                    sendUpdateAirFlowCheckpointRequest(dataIRI);
                }
                hideKeyboardFrom(view.getContext(), view);
            } catch (NumberFormatException e) {
                Toast.makeText(this.getContext(), "The input value should be number.", Toast.LENGTH_SHORT).show();
            } catch (JSONException e) {
                Toast.makeText(this.getContext(), "Failed to submit the change, please resubmit later.", Toast.LENGTH_SHORT).show();
            }
        });
    }

    private void sendUpdateAirFlowCheckpointRequest(String dataIRI) throws JSONException {
        double newSetpoint = Double.parseDouble(editableAttributes.get(0).getValue());
        if (newSetpoint > 1100 || newSetpoint < 400) {
            new MaterialAlertDialogBuilder(requireActivity())
                    .setTitle(R.string.setpoint_out_of_range)
                    .setMessage(R.string.setpoint_should_be_within_1100_to_400)
                    .setPositiveButton(R.string.ok, null)
                    .show();
            return;
        }

        JSONObject params = new JSONObject();
        params.put("dataIRI", dataIRI);
        params.put("value", Double.parseDouble(editableAttributes.get(0).getValue()));
        params.put("clientProperties", "WRITE_CLIENT_PROPERTIES");

        //write values before changing control mode
        JsonObjectRequest jsonObjectRequest = new JsonObjectRequest(Request.Method.POST, WACNET_WRITE_URL.build().toString(), params, response -> {
            //create control params
            JSONObject controlParams = new JSONObject();
            try {
                IRIMapping iriMapping = new IRIMapping(requireContext());
                controlParams.put("dataIRI", iriMapping.getControlIRIFromEditableDataIRI(dataIRI));
                controlParams.put("value", 1.0);
                controlParams.put("clientProperties", "WRITE_CLIENT_PROPERTIES");
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
            JsonObjectRequest controlJsonObjectRequest = new JsonObjectRequest(Request.Method.POST, WACNET_WRITE_URL.build().toString(), controlParams, controlResponse -> {
                try {
                    String responseString = controlResponse.getString("message");
                    if (!responseString.contains("Successfully written")) {
                        Toast.makeText(this.getContext(), "Failed to submit the change, please resubmit later.", Toast.LENGTH_LONG).show();
                    }
                } catch (JSONException e) {
                    throw new RuntimeException(e);
                }
            }, error -> Toast.makeText(this.getContext(), "Failed to submit the change, please resubmit later.", Toast.LENGTH_SHORT).show());
            SingletonConnection.getInstance(this.getContext()).addToRequestQueue(controlJsonObjectRequest);

            try {
                String responseString = response.getString("message");
                Toast.makeText(this.getContext(), responseString, Toast.LENGTH_LONG).show();
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
            }, error -> Toast.makeText(this.getContext(), "Failed to submit the change, please resubmit later.", Toast.LENGTH_SHORT).show());
        SingletonConnection.getInstance(this.getContext()).addToRequestQueue(jsonObjectRequest);
    }

    private void createEditTemperatureRequest(String accessToken, String idToken, AuthorizationException ex) {
        if (ex != null) {
            LOGGER.error("Failed to refresh access token. Reauthorization is needed.");
            if (getContext() != null) {
                authHelper.showSessionExpiredDialog(requireActivity());
            }
            return;
        }

        try {
            JSONObject params = new JSONObject();
            params.put("dataIRI", editableAttributes.get(0).getIri());
            params.put("temperature", Double.parseDouble(editableAttributes.get(0).getValue()));
            params.put("clientProperties", "CLIENT_PROPERTIES");

            JsonObjectRequest jsonObjectRequest = new JsonObjectRequest(Request.Method.POST, ESPHOME_CONTROL_URL.build().toString(), params, response -> {
                try {
                    if (!response.has("fanStatus")) {
                        handleRequestFailure(new VolleyError(response.getString("message")));
                        return;
                    }

                    String fanStatus = response.getString("fanStatus");
                    LOGGER.info(fanStatus.isEmpty() ? fanStatus : "Successfully updated.");

                    Toast.makeText(applicationContext, fanStatus.isEmpty() ? fanStatus : "Successfully updated.", Toast.LENGTH_LONG).show();
                } catch (JSONException e) {
                    throw new RuntimeException(e);
                }
            }, this::handleRequestFailure) {
                @Override
                public Map<String, String> getHeaders() {
                    Map<String, String> params = new HashMap<>();
                    params.put("Content-Type", "application/json");
                    params.put("Authorization", "Bearer " + accessToken);
                    return params;
                }
            };
            SingletonConnection.getInstance(this.getContext()).addToRequestQueue(jsonObjectRequest);
        } catch (NumberFormatException e) {
            Toast.makeText(getContext(), "The input value should be number.", Toast.LENGTH_SHORT).show();
        } catch (JSONException e) {
            Toast.makeText(getContext(), "Failed to submit the change, please resubmit later.", Toast.LENGTH_SHORT).show();
        }
    }

    private void hideKeyboardFrom(Context context, View view) {
        InputMethodManager imm = (InputMethodManager) context.getSystemService(Activity.INPUT_METHOD_SERVICE);
        imm.hideSoftInputFromWindow(view.getWindowToken(), 0);
    }

    public void clearInputs() {
        ((EditableAttributesAdapter) Objects.requireNonNull(binding.editableAttributeRv.getAdapter())).clearInputData();

        LinearLayoutManager layoutManager = ((LinearLayoutManager) Objects.requireNonNull(binding.editableAttributeRv.getLayoutManager()));
        int firstVisiblePosition = layoutManager.findFirstVisibleItemPosition();
        int lastVisiblePosition = layoutManager.findLastVisibleItemPosition();
        for (int i = firstVisiblePosition; i <= lastVisiblePosition; i++) {
            EditableAttributesAdapter.EditableAttributeInputView viewHolder = (EditableAttributesAdapter.EditableAttributeInputView) binding.editableAttributeRv.findViewHolderForAdapterPosition(i);
            assert viewHolder != null;
            viewHolder.remvoeInput();
        }
    }

    private void handleRequestFailure(VolleyError response) {
        LOGGER.error(response.getMessage());
        if (getContext() == null) {
            // fragment detached, no need to do anything
            LOGGER.warn("fragment detached");
            Toast.makeText(applicationContext, "Failed to submit the change, please resubmit later.", Toast.LENGTH_SHORT).show();
            return;
        }

        if (response instanceof AuthFailureError) {
            if (response.networkResponse.statusCode == 403) {
                LOGGER.warn("Permission deny");
                new MaterialAlertDialogBuilder(requireActivity())
                        .setTitle(R.string.permission_deny)
                        .setMessage(R.string.permission_deny_explain)
                        .setPositiveButton(R.string.change_account, (dialogInterface, i) -> logoutLauncher.launch(authHelper.getLogOutIntent()))
                        .setNegativeButton(R.string.cancel, null)
                        .show();
            }
        } else {
            Toast.makeText(applicationContext, "Failed to submit the change, please resubmit later.", Toast.LENGTH_SHORT).show();
        }
    }
}
