package uk.ac.cam.cares.jps.network.maintenance;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.JsonObjectRequest;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.model.Maintenance;
import uk.ac.cam.cares.jps.network.Connection;

public class MaintenanceNetworkSource {
    private static final Logger LOGGER = Logger.getLogger(MaintenanceNetworkSource.class);

    String path = "asset-manager-agent/addmaintenance";

    Connection connection;

    public MaintenanceNetworkSource(Connection connection) {
        this.connection = connection;
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

        JsonObjectRequest request = new JsonObjectRequest(Request.Method.POST, path, param, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }
}
