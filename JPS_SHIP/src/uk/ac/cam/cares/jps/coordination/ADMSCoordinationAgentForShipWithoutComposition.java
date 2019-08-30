package uk.ac.cam.cares.jps.coordination;

import org.apache.http.client.methods.HttpPost;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;


@WebServlet("/ADMSCoordinationAgentForShipWithoutComposition")
public class ADMSCoordinationAgentForShipWithoutComposition extends JPSHttpServlet {

    private static final long serialVersionUID = -2264681360832342804L;
    private static final String PARAM_KEY_SHIP = "ship";

    @Override
    protected void doGetJPS(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        logger = LoggerFactory.getLogger(ADMSCoordinationAgentForShipWithoutComposition.class);
        super.doGetJPS(request, response);
    }

    @Override
    protected JSONObject processRequestParameters(JSONObject requestParams) {

        String regionToCityResult = execute("/JPS/RegionToCity", requestParams.toString());
        String city = new JSONObject(regionToCityResult).getString("city");
        requestParams.put("city", city);
        logger.info("city FROM COORDINATION AGENT: " + city);
        logger.info("overall json= " + requestParams.toString());

        String result = execute("/JPS/GetBuildingListFromRegion", requestParams.toString());
        JSONArray building = new JSONObject(result).getJSONArray("building");
        requestParams.put("building", building);
        logger.info("building FROM COORDINATION AGENT: " + building.toString());

        if (city.toLowerCase().contains("kong")) {
            result = execute("/JPS_SHIP/GetHKUWeatherData", regionToCityResult);
        } else {
            result = execute("/JPS_COMPOSITION/CityToWeather", regionToCityResult);
        }
        JSONObject weatherstate = new JSONObject(result).getJSONObject("weatherstate");
        requestParams.put("weatherstate", weatherstate);

        logger.info("calling postgres= " + requestParams.toString());
        String url = KeyValueManager.get(IKeys.URL_POSITIONQUERY);
        url += "/getEntitiesWithinRegion";
        String resultship = AgentCaller.executeGetWithURLAndJSON(url, requestParams.toString());

        JSONObject jsonShip = new JSONObject(resultship);

        String reactionMechanism = requestParams.optString("reactionmechanism");
        JSONArray newwaste;

        if (reactionMechanism.equals("")) {
            //@TODO [AC] - Async call will not work untill the above condition is removed.
            // Queueing third party software calls with limited number of licenses should be handled by agents,
            // which implement calls to them separately.
            newwaste = getNewWasteAsync(reactionMechanism, jsonShip);
        } else {
            newwaste = getNewWasteSync(reactionMechanism, jsonShip);
        }

        requestParams.put("waste", newwaste);
        requestParams.put(PARAM_KEY_SHIP, jsonShip);

        result = execute("/JPS/ADMSAgent", requestParams.toString(), HttpPost.METHOD_NAME);
        String folder = new JSONObject(result).getString("folder");
        requestParams.put("folder", folder);

        return requestParams;
    }

    private JSONArray getNewWasteAsync(String reactionMechanism, JSONObject jsonShip) {
        JSONArray newwaste = new JSONArray();
        ArrayList<CompletableFuture> wastes = new ArrayList<>();
        int sizeofshipselected = jsonShip.length();

        for (int i = 0; i < sizeofshipselected; i++) {
            logger.info("Ship AGENT called: " + i);
            JSONObject jsonReactionShip = new JSONObject();
            jsonReactionShip.put("reactionmechanism", reactionMechanism);
            jsonReactionShip.put("ship", jsonShip.getJSONObject("collection").getJSONArray("items").getJSONObject(i));

            CompletableFuture<String> getAsync = CompletableFuture.supplyAsync(() ->
                    execute("/JPS_SHIP/ShipAgent", jsonReactionShip.toString()));

            CompletableFuture<String> processAsync = getAsync.thenApply(wasteResult ->
                    new JSONObject(wasteResult).getString("waste"));
            wastes.add(processAsync);
        }

        for (CompletableFuture waste : wastes) {
            try {
                newwaste.put(waste.get());
            } catch (InterruptedException | ExecutionException e) {
                e.printStackTrace();
            }
        }

        return newwaste;
    }

    private JSONArray getNewWasteSync(String reactionMechanism, JSONObject jsonShip) {
        JSONArray newwaste = new JSONArray();
        int sizeofshipselected = jsonShip.length();

        for (int i = 0; i < sizeofshipselected; i++) {
            logger.info("Ship AGENT called: " + i);
            JSONObject jsonReactionShip = new JSONObject();
            jsonReactionShip.put("reactionmechanism", reactionMechanism);
            jsonReactionShip.put("ship", jsonShip.getJSONObject("collection").getJSONArray("items").getJSONObject(i));
            String wasteResult = execute("/JPS_SHIP/ShipAgent", jsonReactionShip.toString());
            String waste = new JSONObject(wasteResult).getString("waste");
            newwaste.put(waste);
        }
        return newwaste;
    }
}
