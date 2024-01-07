package uk.ac.cam.cares.jps.network.route;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.NetworkConfiguration;

public class RouteNetworkSource {
    private static final Logger LOGGER = Logger.getLogger(VertexNetworkSource.class);

    String path = "geoserver/wfs";
    Connection connection;

    String service = "WFS";
    String version = "1.0.0";
    String request = "GetFeature";
    String typeName = "pirmasens:routing_pointsofinterest";
    String outputFormat = "application/json";


    @Inject
    public RouteNetworkSource(Connection connection) {
        BasicConfigurator.configure();
        this.connection = connection;
    }

    public void getRouteGeoJsonData(String startId, String endId, Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = NetworkConfiguration.constructUrlBuilder(path)
                .addQueryParameter("service", service)
                .addQueryParameter("version", version)
                .addQueryParameter("request", request)
                .addQueryParameter("typeName", typeName)
                .addQueryParameter("outputformat", outputFormat)
                .addQueryParameter("viewparams", "source:" + startId + ";target:" + endId)
                .build().toString();
        LOGGER.info(requestUri);

        Response.Listener<String> onSuccess = response -> {
            try {
                JSONObject rawResponse = new JSONObject(response);
                onSuccessUpper.onResponse(rawResponse.toString());
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

        StringRequest request = new StringRequest(Request.Method.GET, requestUri, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }
}
