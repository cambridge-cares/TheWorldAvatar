package uk.ac.cam.cares.jps.network;

import android.content.Context;

import androidx.annotation.NonNull;

import com.android.volley.RequestQueue;
import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Locale;

import okhttp3.HttpUrl;


public class TrajectoryNetworkSource {

    private static final Logger LOGGER = Logger.getLogger(TrajectoryNetworkSource.class);
    private RequestQueue requestQueue;
    private Context context;

    public TrajectoryNetworkSource(RequestQueue requestQueue, Context context) {
        this.requestQueue = requestQueue;
        this.context = context;
    }

    public void getTrajectory(String userId, Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String createLayerUri = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.host_with_port)).newBuilder()
                .addPathSegments(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryqueryagent_createlayer))
                .addQueryParameter("userID", userId)
                .build().toString();
        LOGGER.info(createLayerUri);

        StringRequest createLayerRequest = buildCreateLayerRequest(onSuccessUpper, onFailureUpper, createLayerUri);
        requestQueue.add(createLayerRequest);
    }

    @NonNull
    private StringRequest buildCreateLayerRequest(Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper, String createLayerUri) {
        Response.Listener<String> onCreateLayerSuccess = s -> {
            try {
                JSONObject rawResponse = new JSONObject(s);

                StringRequest getTrajectoryRequest = buildGetTrajectoryRequest(onSuccessUpper, onFailureUpper, rawResponse);
                requestQueue.add(getTrajectoryRequest);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

        StringRequest createLayerRequest = new StringRequest(createLayerUri, onCreateLayerSuccess, onFailureUpper);
        return createLayerRequest;
    }

    @NonNull
    private StringRequest buildGetTrajectoryRequest(Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper, JSONObject rawResponse) throws JSONException {
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
                        "pointiri:%s;speediri:%s;altitudeiri:%s;bearingiri:%s;",
                        pointIRI, speedIRI, altitudeIRI, bearingIRI))
                .build().toString();

        Response.Listener<String> onGetTrajectorySuccess = s1 -> {
            try {
                JSONObject trajectoryResponse = new JSONObject(s1);
                onSuccessUpper.onResponse(trajectoryResponse.toString());
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };
        StringRequest getTrajectoryRequest = new StringRequest(getTrajectoryUri, onGetTrajectorySuccess, onFailureUpper);
        return getTrajectoryRequest;
    }
}
