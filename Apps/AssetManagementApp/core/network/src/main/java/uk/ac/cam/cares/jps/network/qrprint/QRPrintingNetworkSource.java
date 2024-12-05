package uk.ac.cam.cares.jps.network.qrprint;

import android.content.Context;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.JsonObjectRequest;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import uk.ac.cam.cares.jps.model.PrintItem;
import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.NetworkConfiguration;
import uk.ac.cam.cares.jps.network.R;

public class QRPrintingNetworkSource {

    private static final Logger LOGGER = Logger.getLogger(QRPrintingNetworkSource.class);

    Connection connection;
    Context context;

    @Inject
    public QRPrintingNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        BasicConfigurator.configure();
        this.connection = connection;
        this.context = applicationContext;
    }

    public void bulkPrintQRCodes(List<PrintItem> items, Response.Listener<Boolean> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = NetworkConfiguration.constructUrlBuilder(context.getString(R.string.print_qr_code), context).build().toString();

        JSONObject param = new JSONObject();
        JSONArray iris = new JSONArray();
        try {
            for (PrintItem item : items) {
                iris.put(item.getInventoryID());
            }

            JSONObject assetData = new JSONObject();
            assetData.put("IRI", iris);
            param.put("assetData", assetData);
        } catch (JSONException e) {
            LOGGER.error(e.getMessage());
            onFailureUpper.onErrorResponse(new VolleyError("JSON exception: " + e.getMessage()));
        }


        Response.Listener<JSONObject> onSuccess = response -> {
            try {
                if (response.getString("Result").contains("Bulk printing job initiated for ID")) {
                    onSuccessUpper.onResponse(true);
                } else {
                    LOGGER.error(response.getString("Result"));
                    onFailureUpper.onErrorResponse(new VolleyError(response.getString("Result")));
                }
            } catch (JSONException e) {
                LOGGER.error(e.getMessage());
                onFailureUpper.onErrorResponse(new VolleyError("JSON exception: " + e.getMessage()));
            }
        };

        JsonObjectRequest request = new JsonObjectRequest(Request.Method.POST, requestUri, param, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }
}
