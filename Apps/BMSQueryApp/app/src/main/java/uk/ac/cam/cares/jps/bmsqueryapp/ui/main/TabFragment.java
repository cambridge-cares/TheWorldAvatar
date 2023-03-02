package uk.ac.cam.cares.jps.bmsqueryapp.ui.main;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.toolbox.JsonObjectRequest;
import com.android.volley.toolbox.Volley;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.net.URI;
import java.net.URISyntaxException;

import uk.ac.cam.cares.jps.bmsqueryapp.R;
import uk.ac.cam.cares.jps.bmsqueryapp.databinding.FragmentMainBinding;
import uk.ac.cam.cares.jps.bmsqueryapp.util.RequestStatus;
import uk.ac.cam.cares.jps.bmsqueryapp.util.SingletonConnection;

/**
 * A placeholder fragment containing a simple view.
 */
public class TabFragment extends Fragment {

    private final String CONTENT_TYPE = "application/json";
    private final String DATA_IRI_KEY = "dataIRI";
    private final String CLIENT_PROPERTY_KEY = "clientProperties";
    private final String AGENTURI = "http://10.0.2.2:48086/bms-query-agent/retrieve"; //TODO: agent running on local host

    private FragmentMainBinding binding;
    public int requestedStatus;

    public TabFragment(int position) {
        super();
        requestedStatus = position;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container,
                             @Nullable Bundle savedInstanceState) {
        binding = FragmentMainBinding.inflate(inflater, container, false);

        Button refreshBt = binding.refreshBt;
        refreshBt.setOnClickListener(view -> {
            try {
                JSONObject jo = new JSONObject().put(DATA_IRI_KEY, RequestStatus.uidArray[requestedStatus]).put(CLIENT_PROPERTY_KEY, "CLIENT_PROPERTIES");

                JsonObjectRequest jsonRequest = new JsonObjectRequest(Request.Method.POST, AGENTURI, jo, response -> {
                    try {
                        JSONArray values = response.getJSONObject("Result").getJSONArray("values");
                        JSONArray times = response.getJSONObject("Result").getJSONArray("times");

                        binding.currentValueTv.setText("" + values.get(values.length()-1));
                        binding.collectionTimeTv.setText("" + times.get(times.length()-1));
                    } catch (JSONException e) {
                        throw new RuntimeException(e);
                    }
                }, error -> {
                    System.out.println(error);
                    Toast toast = Toast.makeText(this.getContext(), R.string.request_failed_try_again, Toast.LENGTH_SHORT);
                    toast.show();
                });

                SingletonConnection.getInstance(this.getContext()).addToRequestQueue(jsonRequest);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }

            // display the

        });

        return binding.getRoot();
    }


}