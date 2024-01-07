package uk.ac.cam.cares.jps.network.poi;

import androidx.annotation.NonNull;

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
import uk.ac.cam.cares.jps.network.route.VertexNetworkSource;

public class ToiletNetworkSource {

    private static final Logger LOGGER = Logger.getLogger(VertexNetworkSource.class);

    String path = "geoserver/pirmasens/wfs";
    Connection connection;

    String service = "WFS";
    String version = "2.0.0";
    String request = "GetFeature";
    String typeName = "pirmaasens:points";
    String outputFormat = "application/json";

    // TODO: Generalize amenity filter to work with Building, School, ...
    String amenityFilter = "amenity='toilets'";


    @Inject
    public ToiletNetworkSource(Connection connection) {
        BasicConfigurator.configure();
        this.connection = connection;
    }

    public void getToiletsData(Response.Listener<String> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        //https://github.com/cambridge-cares/TheWorldAvatar/blob/fd29120e46068686d4671e6e319fbdfe2666562e/JPS_VIRTUALSENSOR/DispersionInteractor/src/main/resources/pirmasensData.json

        String requestUri = getRequestUri();

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

    @NonNull
    public String getRequestUri() {
        String requestUri = NetworkConfiguration.constructUrlBuilder(path)
                .addQueryParameter("service", service)
                .addQueryParameter("version", version)
                .addQueryParameter("request", request)
                .addQueryParameter("typeName", typeName)
                .addQueryParameter("outputFormat", outputFormat)
                .addQueryParameter("cql_filter", amenityFilter)
//                .addQueryParameter("viewparams", "source:" + startId + ";target:" + endId)
                .build().toString();
        LOGGER.info(requestUri);
        return requestUri;
    }
}
