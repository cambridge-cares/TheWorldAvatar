package uk.ac.cam.cares.jps.bmsqueryapp.view.tab;

import android.app.Activity;
import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.InputMethodManager;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.volley.Request;
import com.android.volley.toolbox.JsonObjectRequest;

import org.apache.log4j.BasicConfigurator;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

import okhttp3.HttpUrl;
import uk.ac.cam.cares.jps.bmsqueryapp.adapter.list.EditableAttributesAdapter;
import uk.ac.cam.cares.jps.bmsqueryapp.data.attribtue.EditableAttribute;
import uk.ac.cam.cares.jps.bmsqueryapp.databinding.FragmentEditBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.Constants;
import uk.ac.cam.cares.jps.bmsqueryapp.utils.SingletonConnection;

public class EditFragment extends Fragment {
    private FragmentEditBinding binding;

    private final HttpUrl.Builder ESPHOME_CONTROL_URL = Constants.constructUrlBuilder(Constants.HOST_LAB_WIFI, 3839, "bms-update-agent/set");
    private List<EditableAttribute> editableAttributes = new ArrayList<>();

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
//            String temperature = binding.temperatureEdit.getText().toString().trim();
            // TODO: backend not able to handle multiple attributes
            try {
//                Double temperatureDouble = Double.parseDouble(temperature);
                JSONObject params = new JSONObject();
                params.put("dataIRI", editableAttributes.get(0).getIri());
                params.put("temperature", Double.parseDouble(editableAttributes.get(0).getValue()));
                params.put("clientProperties", "CLIENT_PROPERTIES");

                JsonObjectRequest jsonObjectRequest = new JsonObjectRequest(Request.Method.POST, ESPHOME_CONTROL_URL.build().toString(), params, response -> {
                    try {
                        String fanStatus = response.getString("fanStatus");
                        Toast.makeText(this.getContext(), fanStatus.isEmpty() ? fanStatus : "Successfully updated.", Toast.LENGTH_LONG).show();
                    } catch (JSONException e) {
                        throw new RuntimeException(e);
                    }
                }, error -> Toast.makeText(this.getContext(), "Failed to submit the change, please resubmit later.", Toast.LENGTH_SHORT).show());
                SingletonConnection.getInstance(this.getContext()).addToRequestQueue(jsonObjectRequest);

//                binding.temperatureEdit.clearFocus();
                hideKeyboardFrom(view.getContext(), view);
            } catch (NumberFormatException e) {
                Toast.makeText(this.getContext(), "The input value should be number.", Toast.LENGTH_SHORT).show();
            } catch (JSONException e) {
                Toast.makeText(this.getContext(), "Failed to submit the change, please resubmit later.", Toast.LENGTH_SHORT).show();
            }

        });
    }

    private void hideKeyboardFrom(Context context, View view) {
        InputMethodManager imm = (InputMethodManager) context.getSystemService(Activity.INPUT_METHOD_SERVICE);
        imm.hideSoftInputFromWindow(view.getWindowToken(), 0);
    }
}
