package uk.ac.cam.cares.jps.coordination;

import org.apache.http.client.methods.HttpGet;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
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

    @Override
    protected void doGetJPS(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        logger = LoggerFactory.getLogger(ADMSCoordinationAgentForShipWithoutComposition.class);
        super.doGetJPS(request, response);
    }

    @Override
    protected void processRequestParameters() {

        responseParams = requestParams;

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
        JSONObject jsonReactionShip = new JSONObject();
        String reactionMechanism = requestParams.getString("reactionmechanism");
        jsonReactionShip.put("reactionmechanism", reactionMechanism);

        for (int i = 0; i < 1; i++) {

            String wasteResult = execute("/JPS_SHIP/ShipAgent", jsonReactionShip.toString());
            String waste = new JSONObject(wasteResult).getString("waste");
            responseParams.put("waste", waste);
        }

        responseParams.put("ship", jsonShip);

        result = execute("/JPS/ADMSAgent", responseParams.toString(), HttpGet.METHOD_NAME);
        String folder = new JSONObject(result).getString("folder");
        responseParams.put("folder", folder);
    }

}
