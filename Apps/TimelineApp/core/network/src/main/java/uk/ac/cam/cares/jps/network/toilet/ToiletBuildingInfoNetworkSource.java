package uk.ac.cam.cares.jps.network.toilet;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.model.Toilet;
import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.NetworkConfiguration;
import uk.ac.cam.cares.jps.network.route.VertexNetworkSource;

public class ToiletBuildingInfoNetworkSource {
    Connection connection;

    private static final Logger LOGGER = Logger.getLogger(ToiletBuildingInfoNetworkSource.class);

    // geoserver setting
    String geoServerPath = "geoserver/pirmasens/wfs";
    String service = "WFS";
    String version = "1.0.0";
    String request = "GetFeature";
    String typeName = "pirmasens:polygons_nearby_building_info";
    String outputFormat = "application/json";

    @Inject
    public ToiletBuildingInfoNetworkSource(Connection connection) {
        BasicConfigurator.configure();
        this.connection = connection;
    }

    public void getBuildingInfoData(double lng, double lat, Response.Listener<Toilet> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        Response.Listener<String> onGetBuildingInfo = response -> {
            try {
                JSONObject buildingFeature = new JSONObject(response)
                        .getJSONArray("features").getJSONObject(0)
                        .getJSONObject("properties");

                String houseNumber = buildingFeature.optString("housenumber");
                String street = buildingFeature.optString("street");
                String city = buildingFeature.optString("city");
                String postcode = buildingFeature.optString("postcode");
                String houseName = buildingFeature.optString("housename");

                Toilet temp = new Toilet(lng, lat);
                if (!buildingFeature.optString("street").isEmpty()) {
                    String address = String.format("%s %s, %s %s", houseNumber.equals("null") ? "" : houseNumber,
                            street.equals("null") ? "" : street,
                            city.equals("null") ? "" : city,
                            postcode.equals("null") ? "" : postcode);
                    temp.setAddress(address);
                }

                temp.setName(houseName.equals("null") ? "" : houseName);
                LOGGER.info("toilet address: " + temp.getAddress() + ", toilet name: " + temp.getName());
                onSuccessUpper.onResponse(temp);

            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

        StringRequest geoServerRequest = new StringRequest(Request.Method.GET, getBuildingInfoRequestUri(lng, lat), onGetBuildingInfo, onFailureUpper);
        connection.addToRequestQueue(geoServerRequest);
    }

    private String getBuildingInfoRequestUri(double lng, double lat) {
        String requestUri = NetworkConfiguration.constructUrlBuilder(geoServerPath)
                .addQueryParameter("service", service)
                .addQueryParameter("version", version)
                .addQueryParameter("request", request)
                .addQueryParameter("typeName", typeName)
                .addQueryParameter("outputFormat", outputFormat)
                .addQueryParameter("viewparams", String.format("lon:%f;lat:%f;", lng, lat))
                .build().toString();
        LOGGER.info(requestUri);
        return requestUri;
    }
}
