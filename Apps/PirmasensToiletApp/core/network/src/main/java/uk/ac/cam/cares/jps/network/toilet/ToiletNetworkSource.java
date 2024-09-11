package uk.ac.cam.cares.jps.network.toilet;

import android.content.Context;

import androidx.annotation.NonNull;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.VolleyError;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;

import okhttp3.HttpUrl;
import uk.ac.cam.cares.jps.model.Toilet;
import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.route.VertexNetworkSource;

public class ToiletNetworkSource {

    private static final Logger LOGGER = Logger.getLogger(VertexNetworkSource.class);

    Connection connection;
    Context context;

    String service = "WFS";
    String version = "2.0.0";
    String request = "GetFeature";
    String typeName = "pirmasens:ps_data";
    String outputFormat = "application/json";

    // TODO: Generalize amenity filter to work with Building, School, ...
    String amenityFilter = "amenity='toilets'";

    @Inject
    public ToiletNetworkSource(Connection connection, Context context) {
        BasicConfigurator.configure();
        this.connection = connection;
        this.context = context;
    }

    public void getToiletsData(Response.Listener<List<Toilet>> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = getRequestUri();

        Response.Listener<String> onSuccess = response -> {
            try {
                List<Toilet> toilets = new ArrayList<>();

                JSONObject featureCollection = new JSONObject(response);
                if ("FeatureCollection".equals(featureCollection.optString("type"))) {
                    JSONArray features = featureCollection.getJSONArray("features");

                    for (int i = 0; i < features.length(); i++) {
                        JSONObject feature = features.getJSONObject(i);
                        JSONObject geometry = feature.getJSONObject("geometry");
                        JSONObject properties = feature.getJSONObject("properties");

                        if ("Point".equals(geometry.optString("type"))) {
                            JSONArray coordinates = geometry.getJSONArray("coordinates");
                            String id = properties.getString("id");
                            // Extract longitude and latitude
                            double longitude = coordinates.getDouble(0);
                            double latitude = coordinates.getDouble(1);

                            Toilet toilet = new Toilet(longitude, latitude);
                            toilet.setId(id);
                            toilets.add(toilet);
                        }
                    }

                    onSuccessUpper.onResponse(toilets);
                } else {
                    LOGGER.error("Invalid GeoJSON: Not a FeatureCollection");
                    onFailureUpper.onErrorResponse(new VolleyError("Invalid GeoJSON: Not a FeatureCollection"));
                }

            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

        StringRequest request = new StringRequest(Request.Method.GET, requestUri, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }

    @NonNull
    public String getRequestUri() {
        String requestUri = HttpUrl.get(context.getString(uk.ac.cam.cares.jps.utils.R.string.geoserver_pirmasens_wfs)).newBuilder()
                .addQueryParameter("service", service)
                .addQueryParameter("version", version)
                .addQueryParameter("request", request)
                .addQueryParameter("typeName", typeName)
                .addQueryParameter("outputFormat", outputFormat)
                .addQueryParameter("cql_filter", amenityFilter)
                .build().toString();
        LOGGER.info(requestUri);
        return requestUri;
    }
}
