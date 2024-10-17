package uk.ac.cam.cares.jps.network.route;

import android.content.Context;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import javax.inject.Inject;

import okhttp3.HttpUrl;
import uk.ac.cam.cares.jps.network.Connection;

public class VertexNetworkSource {
    private static final Logger LOGGER = Logger.getLogger(VertexNetworkSource.class);

    Connection connection;
    Context context;

    String service = "WFS";
    String version = "1.0.0";
    String request = "GetFeature";
    String typeName = "pirmasens:routing_ways_vertices_pgr";
    String outputFormat = "application/json";


    @Inject
    public VertexNetworkSource(Connection connection, Context context) {
        BasicConfigurator.configure();
        this.connection = connection;
        this.context = context;
    }

    public void getVertexId(double lng, double lat, Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.geoserver_wfs)).newBuilder()
                .addQueryParameter("service", service)
                .addQueryParameter("version", version)
                .addQueryParameter("request", request)
                .addQueryParameter("typeName", typeName)
                .addQueryParameter("outputformat", outputFormat)
                .addQueryParameter("viewparams", "lon:" + lng + ";lat:" + lat)
                .build().toString();
        LOGGER.info(requestUri);

        Response.Listener<String> onSuccess = response -> {
            try {
                JSONObject rawResponse = new JSONObject(response);
                String pointId = rawResponse.getJSONArray("features").getJSONObject(0).getJSONObject("properties").getString("id");
                onSuccessUpper.onResponse(pointId);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

        StringRequest request = new StringRequest(Request.Method.GET, requestUri, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }
}
