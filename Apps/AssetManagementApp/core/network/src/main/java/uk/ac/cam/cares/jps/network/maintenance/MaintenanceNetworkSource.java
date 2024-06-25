package uk.ac.cam.cares.jps.network.maintenance;

import android.content.Context;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.JsonObjectRequest;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import uk.ac.cam.cares.jps.model.Maintenance;
import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.R;

public class MaintenanceNetworkSource {
    private static final Logger LOGGER = Logger.getLogger(MaintenanceNetworkSource.class);

    Connection connection;
    Context context;

    public MaintenanceNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        this.connection = connection;
        this.context = applicationContext;
    }

    public void addMaintenance(Maintenance maintenance, Response.Listener<Boolean> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        JSONObject param = new JSONObject();
        JSONObject assetData = new JSONObject();
        try {
            assetData.put("ID", maintenance.getId());
            assetData.put("LastService", maintenance.getLastServiceDate());
            assetData.put("NextService", maintenance.getNextServiceDate());
            assetData.put("Interval", maintenance.getInterval());
            assetData.put("ServiceProvider", maintenance.getServiceProvider());

            param.put("assetData", assetData);
        } catch (JSONException e) {
            throw new RuntimeException(e);
        }

        Response.Listener<JSONObject> onSuccess = response -> {
            try {
                if (response.has("Result") && response.getString("Result").contains("Instantiation failed")) {
                    LOGGER.error(response.getString("Result"));
                    onFailureUpper.onErrorResponse(new VolleyError(response.getString("Result")));
                } else {
                    onSuccessUpper.onResponse(true);
                }
            } catch (JSONException e) {
                LOGGER.error(e.getMessage());
                onFailureUpper.onErrorResponse(new VolleyError("JSON exception: " + e.getMessage()));
            }
        };

        JsonObjectRequest request = new JsonObjectRequest(Request.Method.POST, context.getString(R.string.add_maintenance), param, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }
}
