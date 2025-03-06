package uk.ac.cam.cares.jps.network;

import android.content.Context;

import androidx.annotation.NonNull;

import com.android.volley.AuthFailureError;
import com.android.volley.DefaultRetryPolicy;
import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.StringRequest;
import com.android.volley.toolbox.Volley;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import okhttp3.HttpUrl;

/**
 * Network source for constructing, sending and processing trajectory related requests to server
 */
public class TrajectoryNetworkSource {

    private static final Logger LOGGER = Logger.getLogger(TrajectoryNetworkSource.class);
    private final RequestQueue requestQueue;
    private final Context context;


    /**
     * Constructor of the class. The instantiation is handled by dependency injection.
     * @param requestQueue Volley queue for network request.
     * @param context App context
     */
    public TrajectoryNetworkSource(RequestQueue requestQueue, Context context) {
        this.requestQueue = requestQueue;
        this.context = context;
    }

    /**
     * Get trajectory from server. It consists two steps:
     * 1. create geoserver layers and Postgres SQL functions with TrajectoryQueryAgent
     * 2. get geojson from geoserver for visualisation with iris
     *
     * @param onSuccessUpper Success callback
     * @param onFailureUpper Failure callback
     * @param lowerbound Unix timestamp for the lower bound of the date range
     * @param upperbound Unix timestamp for the upper bound of the date range
     */
    public void getTrajectory(String accessToken, long lowerbound, long upperbound, Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String createLayerUri = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
                .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryqueryagent_createLayer))
                .build().toString();
        LOGGER.info(createLayerUri);

        StringRequest createLayerRequest = buildCreateLayerRequest(accessToken, onSuccessUpper, onFailureUpper, createLayerUri, lowerbound, upperbound);
        requestQueue.add(createLayerRequest);
    }

    @NonNull
    private StringRequest buildCreateLayerRequest(String accessToken, Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper, String createLayerUri, long lowerbound, long upperbound) {
        Response.Listener<String> onCreateLayerSuccess = s -> {
            try {
                // Log the full server response
                LOGGER.debug("Full server response: " + s);
                JSONObject rawResponse = new JSONObject(s);

                StringRequest getTrajectoryRequest = buildGetTrajectoryRequest(accessToken,onSuccessUpper, onFailureUpper, rawResponse, lowerbound, upperbound);
                if (getTrajectoryRequest != null) {
                    requestQueue.add(getTrajectoryRequest);
                }
            } catch (JSONException e) {
                LOGGER.error("Received XML response instead of JSON: " + s);
                onFailureUpper.onErrorResponse(new VolleyError("geoserver error"));
            }
        };

       return new StringRequest(Request.Method.GET, createLayerUri, onCreateLayerSuccess, onFailureUpper) {
            @Override
            public Map<String, String> getHeaders() {
                Map<String, String> headers = new HashMap<>();
                headers.put("Authorization", "Bearer " + accessToken);
                return headers;
            }
        };
    }

private StringRequest buildGetTrajectoryRequest(String accessToken, Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper, JSONObject rawResponse, long lowerbound, long upperbound) throws JSONException {
    if (!rawResponse.has("message")) {
        LOGGER.error("Not able to handle the agent response. Please check the backend");
        onFailureUpper.onErrorResponse(new VolleyError("Server error"));
        return null;
    }

    if (rawResponse.getString("message").equals(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_phone_id_on_the_user))) {
        onFailureUpper.onErrorResponse(new VolleyError(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_phone_id_on_the_user)));
        return null;
    } else if (rawResponse.getString("message").equals(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_measurement_iri_missing))) {
        LOGGER.info("No trajectory retrieved for this user id");
        onSuccessUpper.onResponse("");
        return null;
    } else if (!rawResponse.getString("message").equals(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_layer_created))) {
        LOGGER.error("Not able to handle the agent response. Please check the backend");
        onFailureUpper.onErrorResponse(new VolleyError("Server error"));
        return null;
    }

    // Primary request URL
    String getTrajectoryActivityUri = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
            .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.geoserver_jwt_proxy_geoserver_twa_wfs))
            .addQueryParameter("service", "WFS")
            .addQueryParameter("version", "1.0.0")
            .addQueryParameter("request", "GetFeature")
            .addQueryParameter("typeName", "twa:trajectoryUserIdByActivity")
            .addQueryParameter("outputFormat", "application/json")
            .addQueryParameter("viewparams", String.format(Locale.ENGLISH, "upperbound:%d;lowerbound:%d;", upperbound, lowerbound))
            .build().toString();

    // Fallback request URL
    String getTrajectoryDefaultUri = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
            .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.geoserver_jwt_proxy_geoserver_twa_wfs))
            .addQueryParameter("service", "WFS")
            .addQueryParameter("version", "1.0.0")
            .addQueryParameter("request", "GetFeature")
            .addQueryParameter("typeName", "twa:trajectoryUserId")
            .addQueryParameter("outputFormat", "application/json")
            .addQueryParameter("viewparams", String.format(Locale.ENGLISH, "upperbound:%d;lowerbound:%d;", upperbound, lowerbound))
            .build().toString();

    LOGGER.info("Print out URI: " + getTrajectoryActivityUri);

    Response.Listener<String> onGetTrajectorySuccess = s1 -> {
        try {
            LOGGER.debug("Full server response: " + s1);

            JSONObject trajectoryResponse = new JSONObject(s1);

            // Check if response is empty or invalid
            if (trajectoryResponse.getInt("totalFeatures") == 0 || 
                (trajectoryResponse.getInt("totalFeatures") == 1 && 
                "null".equals(trajectoryResponse.getJSONArray("features").getJSONObject(0).getString("geometry")))) {

                LOGGER.info("No valid trajectory found, switching to default URI.");

                // Create and send fallback request
                StringRequest fallbackRequest = new StringRequest(Request.Method.GET, getTrajectoryDefaultUri, onSuccessUpper, onFailureUpper) {
                    @Override
                    public Map<String, String> getHeaders() throws AuthFailureError {
                        Map<String, String> headers = new HashMap<>();
                        headers.put("Authorization", "Bearer " + accessToken);
                        return headers;
                    }
                };
                fallbackRequest.setRetryPolicy(new DefaultRetryPolicy(10000, 2, DefaultRetryPolicy.DEFAULT_BACKOFF_MULT));

                RequestQueue requestQueue = Volley.newRequestQueue(context);
                requestQueue.add(fallbackRequest);

            } else {
                onSuccessUpper.onResponse(trajectoryResponse.toString());
            }
        } catch (JSONException e) {
            LOGGER.error("Received XML response instead of JSON: " + s1);
            onFailureUpper.onErrorResponse(new VolleyError("Geoserver error"));
        }
    };

    return new StringRequest(Request.Method.GET, getTrajectoryActivityUri, onGetTrajectorySuccess, onFailureUpper) {
        @Override
        public Map<String, String> getHeaders() throws AuthFailureError {
            Map<String, String> headers = new HashMap<>();
            headers.put("Authorization", "Bearer " + accessToken);
            return headers;
        }
    };
}

}