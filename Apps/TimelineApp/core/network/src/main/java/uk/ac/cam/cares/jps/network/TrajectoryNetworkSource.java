package uk.ac.cam.cares.jps.network;

import android.content.Context;

import androidx.annotation.NonNull;

import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Locale;

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
     * @param userId User id for the logged in user.
     * @param date Chosen date for visualisation
     * @param onSuccessUpper Success callback
     * @param onFailureUpper Failure callback
     */
    public void getTrajectory(String userId, String date, Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String createLayerUri = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
                .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryqueryagent_createlayer))
                .addQueryParameter("userID", userId)
                .build().toString();
        LOGGER.info(createLayerUri);

        StringRequest createLayerRequest = buildCreateLayerRequest(onSuccessUpper, onFailureUpper, createLayerUri, date);
        requestQueue.add(createLayerRequest);
    }

    @NonNull
    private StringRequest buildCreateLayerRequest(Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper, String createLayerUri, String date) {
        Response.Listener<String> onCreateLayerSuccess = s -> {
            try {
                JSONObject rawResponse = new JSONObject(s);

                StringRequest getTrajectoryRequest = buildGetTrajectoryRequest(onSuccessUpper, onFailureUpper, rawResponse, date);
                if (getTrajectoryRequest != null) {
                    requestQueue.add(getTrajectoryRequest);
                }
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

        return new StringRequest(createLayerUri, onCreateLayerSuccess, onFailureUpper);
    }

    private StringRequest buildGetTrajectoryRequest(Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper, JSONObject rawResponse, String date) throws JSONException {
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

        String speedIRI = rawResponse.getString("speedIRI");
        String bearingIRI = rawResponse.getString("bearingIRI");
        String altitudeIRI = rawResponse.getString("altitudeIRI");
        String pointIRI = rawResponse.getString("pointIRI");

        String getTrajectoryUri = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
                .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.geoserver_twa_ows))
                .addQueryParameter("service", "WFS")
                .addQueryParameter("version", "1.0.0")
                .addQueryParameter("request", "GetFeature")
                .addQueryParameter("typeName", "twa:trajectoryLine")
                .addQueryParameter("outputFormat", "application/json")
                .addQueryParameter("viewparams", String.format(Locale.ENGLISH,
                        "pointiri:%s;speediri:%s;altitudeiri:%s;bearingiri:%s;date:%s;",
                        pointIRI, speedIRI, altitudeIRI, bearingIRI, date))
                .build().toString();

        Response.Listener<String> onGetTrajectorySuccess = s1 -> {
            try {
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
                throw new RuntimeException(e);
            }
        };
        StringRequest getTrajectoryRequest = new StringRequest(getTrajectoryUri, onGetTrajectorySuccess, onFailureUpper);
        return getTrajectoryRequest;
    }
}
