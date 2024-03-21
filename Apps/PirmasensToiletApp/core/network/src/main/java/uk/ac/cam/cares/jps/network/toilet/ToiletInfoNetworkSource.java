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
    String typeName = "pirmasens:ps_data";
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

    public void getToiletInfoData(String id, Response.Listener<Toilet> onSuccessUpper, Response.ErrorListener onFailureUpper) {

        String osmId = id;
        // send request to FIA after receiving the toilet osm_id from geoserver
        Response.Listener<String> onGetToiletInfo = toiletInfo -> {
            try {
                JSONObject toiletInfoJson = new JSONObject(toiletInfo).getJSONObject("meta");
                LOGGER.info("toiletInfoJson" + toiletInfoJson);

                // todo: the mapping need to be updated if ontology is refined
                Toilet toilet = new Toilet(0, 0);
                toilet.setWheelchair(toiletInfoJson.optString("has wheelchair"));
                toilet.setName(toiletInfoJson.optString("has name"));

                // address
                toilet.setStreetAddress(toiletInfoJson.optString("street address"));
                toilet.setLocality(toiletInfoJson.optString("locality"));
                toilet.setPostalCode(toiletInfoJson.optString("postal code"));

                // price related
                toilet.setPrice(new Price(toiletInfoJson.optDouble("price"), toiletInfoJson.optString("price currency")));


                // time related
                toilet.setOpenTime(toiletInfoJson.optString("opens on"));
                toilet.setEndTime(toiletInfoJson.optString("closes on"));

                toilet.setHasFemale(toiletInfoJson.optString("is for female").contains("true"));
                toilet.setHasMale(toiletInfoJson.optString("is for male").contains("true"));

                toilet.setImage(toiletInfoJson.optString("has image"));

                toilet.addOtherInfo("Operator", toiletInfoJson.optString("has operator"));

                onSuccessUpper.onResponse(toilet);
            } catch (JSONException e) {
                throw new RuntimeException(e);
            }
        };

        StringRequest fiaRequest = new StringRequest(Request.Method.GET, getFIARequestUri(osmId), onGetToiletInfo, onFailureUpper);
        connection.addToRequestQueue(fiaRequest);
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
