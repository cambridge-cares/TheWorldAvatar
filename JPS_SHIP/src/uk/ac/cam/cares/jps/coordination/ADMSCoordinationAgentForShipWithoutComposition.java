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

        JSONObject responseParams = requestParams;

        String regionToCityResult = execute("/JPS/RegionToCity", requestParams.toString());
        String city = new JSONObject(regionToCityResult).getString("city");
        responseParams.put("city", city);
        logger.info("city FROM COORDINATION AGENT: " + city);
        logger.info("overall json= " + responseParams.toString());

        String result = execute("/JPS/GetBuildingListFromRegion", responseParams.toString());
        JSONArray building = new JSONObject(result).getJSONArray("building");
        responseParams.put("building", building);
        logger.info("building FROM COORDINATION AGENT: " + building.toString());

        if (city.toLowerCase().contains("kong")) {
            result = execute("/JPS_SHIP/GetHKUWeatherData", regionToCityResult);
        } else {
            result = execute("/JPS_COMPOSITION/CityToWeather", regionToCityResult);
        }
        JSONObject weatherstate = new JSONObject(result).getJSONObject("weatherstate");
        responseParams.put("weatherstate", weatherstate);

        logger.info("calling postgres= " + requestParams.toString());
        String url = KeyValueManager.get(IKeys.URL_POSITIONQUERY);
        url += "/getEntitiesWithinRegion";
        String resultship = AgentCaller.executeGetWithURLAndJSON(url, requestParams.toString());

        JSONObject jsonShip = new JSONObject(resultship);
        int sizeofshipselected=jsonShip.getJSONObject("collection").getJSONArray("items").length();
        
        String waste =""; //temp to test
        JSONArray newwaste = new JSONArray();
//        for (int i = 0; i < sizeofshipselected; i++) { temporary solution to check whether the loop is ok
        for (int i = 0; i < 5; i++) {
        	logger.info("=================ship agent is called for "+i+" times========================");
            JSONObject jsonReactionShip = new JSONObject();
            String reactionMechanism = requestParams.getString("reactionmechanism");
            jsonReactionShip.put("reactionmechanism", reactionMechanism);
            jsonReactionShip.put("ship", jsonShip.getJSONObject("collection").getJSONArray("items").getJSONObject(i));
            

            String wasteResult = execute("/JPS_SHIP/ShipAgent", jsonReactionShip.toString());
             waste = new JSONObject(wasteResult).getString("waste");
             newwaste.put(waste);
        }
        
        responseParams.put("waste", newwaste); //temp to test

        responseParams.put(PARAM_KEY_SHIP, jsonShip);

        result = execute("/JPS/ADMSAgent", responseParams.toString(), HttpPost.METHOD_NAME);
        String folder = new JSONObject(result).getString("folder");
        responseParams.put("folder", folder);

        return responseParams;
    }

}
