package uk.ac.cam.cares.jps.network;

import android.Manifest;
import android.content.Context;
import android.content.pm.PackageManager;
import android.location.Location;

import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;

import com.android.volley.AuthFailureError;
import com.android.volley.Request;
import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import okhttp3.HttpUrl;

import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.tasks.OnSuccessListener;

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

        // Get the user's current location and then build the request
        if (ActivityCompat.checkSelfPermission(context, Manifest.permission.ACCESS_FINE_LOCATION) != PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(context, Manifest.permission.ACCESS_COARSE_LOCATION) != PackageManager.PERMISSION_GRANTED) {
            return;
        }

        StringRequest createLayerRequest = buildCreateLayerRequest(accessToken, onSuccessUpper, onFailureUpper, createLayerUri, lowerbound, upperbound);
        requestQueue.add(createLayerRequest);
    }

    @NonNull
    private StringRequest buildCreateLayerRequest(String accessToken, Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper, String createLayerUri, long lowerbound, long upperbound) {
        Response.Listener<String> onCreateLayerSuccess = s -> {
            try {
                // Log the full server response
                LOGGER.error("Full server response: " + s);

                if (s.startsWith("<?xml")) {
                    // Handle XML response
                    LOGGER.error("Received XML response instead of JSON: " + s);
                    throw new RuntimeException("Unexpected XML response when JSON was expected");
                }
                JSONObject rawResponse = new JSONObject(s);

                StringRequest getTrajectoryRequest = buildGetTrajectoryRequest(accessToken,onSuccessUpper, onFailureUpper, rawResponse, lowerbound, upperbound);
                if (getTrajectoryRequest != null) {
                    requestQueue.add(getTrajectoryRequest);
                }
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

       return new StringRequest(Request.Method.GET, createLayerUri, onCreateLayerSuccess, onFailureUpper) {
            @Override
            public Map<String, String> getHeaders() throws AuthFailureError {
                Map<String, String> headers = new HashMap<>();
                headers.put("Authorization", "Bearer " + accessToken);
                return headers;
            }
        };
    }


    private StringRequest buildGetTrajectoryRequest(String accessToken, Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper, JSONObject rawResponse, long lowerbound, long upperbound) throws JSONException {
        if (!rawResponse.has("message")) {
            throw new RuntimeException("Not able to handle the agent response. Please check the backend");
        }

        if (rawResponse.getString("message").equals(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_phone_id_on_the_user))) {
            onFailureUpper.onErrorResponse(new VolleyError(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_phone_id_on_the_user)));
            return null;
        } else if (rawResponse.getString("message").equals(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_measurement_iri_missing))) {
            LOGGER.info("No trajectory retrieved for this user id");
            onSuccessUpper.onResponse("");
            return null;
        } else if (!rawResponse.getString("message").equals(context.getString(uk.ac.cam.cares.jps.utils.R.string.rtajectoryagent_layer_created))) {
            throw new RuntimeException("Not able to handle the agent response. Please check the backend");
        }



        String getTrajectoryUri = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
                .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.geoserver_jwt_proxy_geoserver_twa_wfs))
                .addQueryParameter("service", "WFS")
                .addQueryParameter("version", "1.0.0")
                .addQueryParameter("request", "GetFeature")
                .addQueryParameter("typeName", "twa:trajectoryUserId")
                .addQueryParameter("outputFormat", "application/json")
                .addQueryParameter("viewparams", String.format(Locale.ENGLISH, "upperbound:%d;lowerbound:%d;", upperbound, lowerbound))
                .build().toString();

        LOGGER.error("Print out uri: " + getTrajectoryUri);

        Response.Listener<String> onGetTrajectorySuccess = s1 -> {
            try {
                // Log the full server response
                LOGGER.error("Full server response: " + s1);

                if (s1.startsWith("<?xml")) {
                    // Handle XML response
                    LOGGER.error("Received XML response instead of JSON: " + s1);
                    throw new RuntimeException("Unexpected XML response when JSON was expected");
                }
                JSONObject trajectoryResponse = new JSONObject(s1);
                if (trajectoryResponse.getInt("totalFeatures") == 1 && trajectoryResponse
                        .getJSONArray("features")
                        .getJSONObject(0)
                        .getString("geometry").equals("null")) {
                    onSuccessUpper.onResponse("");
                } else {
                    onSuccessUpper.onResponse(trajectoryResponse.toString());
                }
            } catch (JSONException e) {
                LOGGER.error("Failed to parse JSON: " + s1, e);
                throw new RuntimeException(e);
            }
        };


        return new StringRequest(Request.Method.GET, getTrajectoryUri, onGetTrajectorySuccess, onFailureUpper) {
            @Override
            public Map<String, String> getHeaders() throws AuthFailureError {
                Map<String, String> headers = new HashMap<>();
                headers.put("Authorization", "Bearer " + accessToken);
                return headers;
            }
        };
    }
}
