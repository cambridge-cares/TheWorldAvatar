package uk.ac.cam.cares.jps.network;

import androidx.annotation.NonNull;

import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.Locale;


public class TrajectoryNetworkSource {

    private static final Logger LOGGER = Logger.getLogger(TrajectoryNetworkSource.class);
    Connection connection;
    public TrajectoryNetworkSource(Connection connection) {
        this.connection = connection;
    }

    public void getTrajectory(Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        // todo: should fetch the userId from the user module
        String createLayerUri = NetworkConfiguration.constructUrlBuilder("trajectoryqueryagent/createlayer")
                .addQueryParameter("userID", "487a672a-032a-4364-9dff-5651fa046a7c")
                .build().toString();
        LOGGER.info(createLayerUri);

        StringRequest createLayerRequest = buildCreateLayerRequest(onSuccessUpper, onFailureUpper, createLayerUri);
        connection.addToRequestQueue(createLayerRequest);
    }

    @NonNull
    private StringRequest buildCreateLayerRequest(Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper, String createLayerUri) {
        Response.Listener<String> onCreateLayerSuccess = s -> {
            try {
                JSONObject rawResponse = new JSONObject(s);

                StringRequest getTrajectoryRequest = buildGetTrajectoryRequest(onSuccessUpper, onFailureUpper, rawResponse);
                connection.addToRequestQueue(getTrajectoryRequest);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

        StringRequest createLayerRequest = new StringRequest(createLayerUri, onCreateLayerSuccess, onFailureUpper);
        return createLayerRequest;
    }

    @NonNull
    private static StringRequest buildGetTrajectoryRequest(Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper, JSONObject rawResponse) throws JSONException {
        String speedIRI = rawResponse.getString("speedIRI");
        String bearingIRI = rawResponse.getString("bearingIRI");
        String altitudeIRI = rawResponse.getString("altitudeIRI");
        String pointIRI = rawResponse.getString("pointIRI");

        String getTrajectoryUri = NetworkConfiguration.constructUrlBuilder("geoserver/twa/ows")
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
