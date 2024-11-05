package uk.ac.cam.cares.jps.network.otherinfo;

import android.content.Context;

import com.android.volley.Request;
import com.android.volley.Response;
import com.android.volley.toolbox.StringRequest;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import uk.ac.cam.cares.jps.model.building.Building;
import uk.ac.cam.cares.jps.model.building.Instance;
import uk.ac.cam.cares.jps.network.Connection;
import uk.ac.cam.cares.jps.network.NetworkConfiguration;
import uk.ac.cam.cares.jps.network.R;

public class BMSNetworkSource {

    private static final Logger LOGGER = Logger.getLogger(BMSNetworkSource.class);
    Connection connection;
    Context context;

    @Inject
    public BMSNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        BasicConfigurator.configure();
        this.connection = connection;
        this.context = applicationContext;
    }

    public void getBuildingInfo(Response.Listener<List<Instance>> onSuccessUpper, Response.ErrorListener onFailureUpper) {
        String requestUri = NetworkConfiguration.constructUrlBuilder(context.getString(R.string.get_buildings), context).build().toString();
        LOGGER.info(requestUri);

        Response.Listener<String> onSuccess = responseStr -> {
            JSONObject response;
            List<Instance> buildings = new ArrayList<>();
            try {
                response = new JSONObject(responseStr).getJSONObject("buildings");
                Iterator<String> iter = response.keys();
                while (iter.hasNext()) {
                    String buildingIri = iter.next();
                    Building building = new Building(response.getJSONObject(buildingIri), buildingIri);
                    buildings.add(building);
                }
                onSuccessUpper.onResponse(buildings);
            } catch (JSONException e) {
                throw new RuntimeException("Unable to parse the JSONObject returned from the BMSQueryAgent");
            }

            LOGGER.info("Finished building. Created " + buildings.size() + "buildings.");
        };

        StringRequest request = new StringRequest(Request.Method.POST, requestUri, onSuccess, onFailureUpper);
        connection.addToRequestQueue(request);
    }
}
