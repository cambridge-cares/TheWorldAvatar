package uk.ac.cam.cares.jps.network.toilet;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.List;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.model.Price;
import uk.ac.cam.cares.jps.model.Toilet;
import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.NetworkConfiguration;
import uk.ac.cam.cares.jps.network.route.VertexNetworkSource;

public class ToiletInfoNetworkSource {

    private static final Logger LOGGER = Logger.getLogger(ToiletInfoNetworkSource.class);
    Connection connection;

    // geoserver setting
    String geoServerPath = "geoserver/pirmasens/wfs";

    String service = "WFS";
    String version = "1.0.0";
    String request = "GetFeature";
    String typeName = "pirmaasens:points_toilet_id";
    String outputFormat = "application/json";

    // feature info agent setting
    String fiaPath = "feature-info-agent/get";
    String toiletIriPrefix = "https://www.theworldavatar.com/kg/ontocitytoilets/poi_";
    String kgEndpoint = "http://pirmasens-blazegraph:8080/blazegraph/namespace/pirmasens/sparql";

    @Inject
    public ToiletInfoNetworkSource(Connection connection) {
        BasicConfigurator.configure();
        this.connection = connection;
    }

    public void getToiletInfoData(double lng, double lat, Response.Listener<Toilet> onSuccessUpper, Response.ErrorListener onFailureUpper) {

        Response.Listener<String> onGetToiletId = response -> {
            try {
                JSONObject responseJson = new JSONObject(response);
                String osmId = responseJson.getJSONArray("features").getJSONObject(0).getJSONObject("properties").getString("osm_id");

                // send request to FIA after receiving the toilet osm_id from geoserver
                Response.Listener<String> onGetToiletInfo = toiletInfo -> {
                    try {
                        JSONObject toiletInfoJson = new JSONObject(toiletInfo).getJSONObject("meta");
                        LOGGER.info(toiletInfoJson);

                        // todo: the mapping need to be updated if ontology is refined
                        Toilet toilet = new Toilet(lng, lat);
                        toilet.setWheelchair(toiletInfoJson.optString("has wheelchair"));
                        toilet.setName(toiletInfoJson.optString("has name"));

                        // fee related
                        toilet.setFee(toiletInfoJson.optString("has fee"));
                        if (!toiletInfoJson.optString("has amount").isEmpty()) {
                            toilet.setPrice(new Price(toiletInfoJson.optString("has amount"), toiletInfoJson.optString("has currency")));
                        }

                        // time related
                        toilet.setOpenTime(toiletInfoJson.optString("opens on"));
                        toilet.setEndTime(toiletInfoJson.optString("closes on"));

                        toilet.setHasFemale(toiletInfoJson.optString("is for female").contains("yes"));
                        toilet.setHasMale(toiletInfoJson.optString("is for male").contains("yes"));

                        toilet.addOtherInfo("access", toiletInfoJson.optString("access"));
                        toilet.addOtherInfo("operator", toiletInfoJson.optString("has operator"));

                        onSuccessUpper.onResponse(toilet);
                    } catch (JSONException e) {
                        throw new RuntimeException(e);
                    }
                };

                StringRequest fiaRequest = new StringRequest(Request.Method.GET, getFIARequestUri(osmId), onGetToiletInfo, onFailureUpper);
                connection.addToRequestQueue(fiaRequest);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }

        };

        StringRequest geoServerRequest = new StringRequest(Request.Method.GET, getGeoServerRequestUri(lng, lat), onGetToiletId, onFailureUpper);
        connection.addToRequestQueue(geoServerRequest);
    }

    private String getGeoServerRequestUri(double lng, double lat) {
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

    private String getFIARequestUri(String toiletId) {
        String iri = toiletIriPrefix + toiletId;
        String requestUri = NetworkConfiguration.constructUrlBuilder(fiaPath)
                .addQueryParameter("iri", iri)
                .addQueryParameter("endpoint", kgEndpoint)
                .build().toString();
        LOGGER.info(requestUri);
        return requestUri;
    }
}
