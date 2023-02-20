package uk.ac.cam.cares.jps.coordination;

import java.util.ArrayList;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.http.client.methods.HttpPost;
import org.apache.jena.atlas.json.JSON;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;


@WebServlet("/ADMSCoordinationAgentForShipWithoutComposition")
public class ADMSCoordinationAgentForShipWithoutComposition extends JPSAgent {

    private static final long serialVersionUID = -2264681360832342804L;
    private static final String PARAM_KEY_SHIP = "ship";
    private AgentCallerWrapper wrapper = new AgentCallerWrapper();

    /*@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(ADMSCoordinationAgentForShipWithoutComposition.class);
    }*/

    public ADMSCoordinationAgentForShipWithoutComposition(AgentCallerWrapper wrapper)
    {
        this.wrapper = wrapper;
    }

    public ADMSCoordinationAgentForShipWithoutComposition(){}

    /*@Override
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
        requestParams.put(PARAM_KEY_SHIP, jsonShip);

        if (((JSONArray) ((JSONObject) jsonShip.get("collection")).get("items")).length() != 0) {
            String reactionMechanism = requestParams.optString("reactionmechanism");
            JSONArray newwaste;

            newwaste = getNewWasteAsync(reactionMechanism, jsonShip);

            requestParams.put("waste", newwaste);

            result = execute("/JPS/ADMSAgent", requestParams.toString(), HttpPost.METHOD_NAME);
            String folder = new JSONObject(result).getString("folder");
            requestParams.put("folder", folder);
        }

        return requestParams;
    }
*/
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if(validateInput(requestParams)){
            //String regionToCityResult = execute("/JPS/RegionToCity", requestParams.toString());
            String regionToCityResult = AgentCaller.executeGet("/JPS/RegionToCity","query", requestParams.toString());
            String city = new JSONObject(regionToCityResult).getString("city");
            requestParams.put("city", city);
            logger.info("city FROM COORDINATION AGENT: " + city);
            logger.info("overall json= " + requestParams.toString());
            //String result = execute("/JPS/GetBuildingListFromRegion", requestParams.toString());
            String result = AgentCaller.executeGet("/JPS/GetBuildingListFromRegion", "query", requestParams.toString());
            JSONArray building = new JSONObject(result).getJSONArray("building");
            requestParams.put("building", building);
            logger.info("building FROM COORDINATION AGENT: " + building.toString());
            if (city.toLowerCase().contains("kong")) {
                //result = execute("/JPS_SHIP/GetHKUWeatherData", regionToCityResult);
                result=AgentCaller.executeGet("/JPS_SHIP/GetHKUWeatherData","query",regionToCityResult);
            } else {
                //result = execute("/JPS_COMPOSITION/CityToWeather", regionToCityResult);
                result=AgentCaller.executeGet("/JPS_COMPOSITION/CityToWeather","query",regionToCityResult);
            }
            JSONObject weatherstate = new JSONObject(result).getJSONObject("weatherstate");
            requestParams.put("weatherstate", weatherstate);
            logger.info("calling postgres= " + requestParams.toString());
            String url = KeyValueManager.get(IKeys.URL_POSITIONQUERY);
            url += "/getEntitiesWithinRegion";
            String resultship = AgentCaller.executeGetWithURLAndJSON(url, requestParams.toString());
            JSONObject jsonShip = new JSONObject(resultship);
            requestParams.put(PARAM_KEY_SHIP, jsonShip);
            if (((JSONArray) ((JSONObject) jsonShip.get("collection")).get("items")).length() != 0) {
                String folder=createFolder(requestParams,jsonShip);
                requestParams.put("folder", folder);
            }
        }
        return requestParams;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        boolean validate=true;
        if (requestParams.isEmpty()) {
            throw new BadRequestException("RequestParam is empty");
        }else if(!checkAgent(requestParams)){
            throw new BadRequestException("In the requestParam object either the key:agent is missing or is null or is empty.");
        }else if(!checkLocation(requestParams)){
            throw new BadRequestException("In the requestParam object either the key:location is missing or is null or is empty.");
        }else if(!checkReactionMechanism(requestParams)){
            throw new BadRequestException("In the requestParam object either the key:reactionmechanism is missing or is null or is empty.");
        }else if(!checkRegion(requestParams)){
            throw new BadRequestException("In the requestParam object either the key:region is missing or is null or is empty.");
        } else {
            if (!checkSrsname(requestParams)) {
                throw new BadRequestException("In the region object either the key:srsname is missing or is null or is empty.");
            }
            if (!checkLowerCorner(requestParams)) {
                throw new BadRequestException("In the region object the structure of key:lowercorner is incorrect. Check if all keys in lowercorner are assigned.");
            }
            if (!checkUpperCorner(requestParams)) {
                throw new BadRequestException("In the region object the structure of key:uppercorner is incorrect. Check if all keys in uppercorner are assigned.");
            }
        }
        return validate;
    }

    private boolean checkAgent(JSONObject requestParams){
        boolean validate=true;
        if(!requestParams.has("agent") || requestParams.isNull("agent")){
            validate=false;
        }
        if(validate){
            String agent=requestParams.getString("agent");
            if(agent.isEmpty())
                validate=false;
        }
        return validate;
    }

    private boolean checkLocation(JSONObject requestParams){
        boolean validate=true;
        if(!requestParams.has("location") || requestParams.isNull("location")){
            validate=false;
        }
        if(validate){
            String location=requestParams.getString("location");
            if(location.isEmpty())
                validate=false;
        }
        return validate;
    }

    private boolean checkReactionMechanism(JSONObject requestParams){
        boolean validate=true;
        if(!requestParams.has("reactionmechanism") || requestParams.isNull("reactionmechanism")){
            validate=false;
        }
        if(validate){
            String reactionMech=requestParams.getString("reactionmechanism");
            if(reactionMech.isEmpty())
                validate=false;
        }
        return validate;
    }

    private boolean checkRegion(JSONObject requestParams){
        boolean validate=true;
        if(!requestParams.has("region") || requestParams.isNull("region")){
            validate=false;
        }
        if(validate){
            JSONObject region= requestParams.getJSONObject("region");
            if(region.isEmpty())
                validate=false;
        }
        return validate;
    }

    private boolean checkSrsname(JSONObject requestParams){
        boolean validate=true;
        JSONObject region = requestParams.getJSONObject("region");
        if(!region.has("srsname") || region.isNull("srsname")){
            validate=false;
        }
        if(validate){
            String srs=region.getString("srsname");
            if(srs.isEmpty())
                validate=false;
        }
        return validate;
    }

    private boolean checkLowerCorner(JSONObject requestParams){
        boolean validate=true;
        JSONObject region = requestParams.getJSONObject("region");
        if(!region.has("lowercorner") || region.isNull("lowercorner")){
            validate=false;
        }
        if(validate){
            JSONObject lowerCorner=region.getJSONObject("lowercorner");
            if (lowerCorner.isEmpty() || !lowerCorner.has("lowerx")|| !lowerCorner.has("lowery")){
                validate=false;
            }
            if(validate){
                String lx = lowerCorner.get("lowerx").toString();
                String ly = lowerCorner.get("lowery").toString();
                if( lowerCorner.isNull("lowerx") || lowerCorner.isNull("lowery") || lx.isEmpty() || ly.isEmpty()){
                    validate=false;
                }
            }
        }
        return validate;
    }

    private boolean checkUpperCorner(JSONObject requestParams){
        boolean validate=true;
        JSONObject region =requestParams.getJSONObject("region");
        if(!region.has("uppercorner") || region.isNull("uppercorner")){
            validate=false;
        }
        if(validate){
            JSONObject upperCorner=region.getJSONObject("uppercorner");
            if(upperCorner.isEmpty() || !upperCorner.has("upperx") || !upperCorner.has("uppery")){
                validate=false;
            }
            if(validate){
                String ux=upperCorner.get("upperx").toString();
                String uy=upperCorner.get("uppery").toString();
                if(upperCorner.isNull("upperx") || upperCorner.isNull("uppery") || ux.isEmpty() || uy.isEmpty()){
                    validate=false;
                }
            }
        }
        return validate;
    }

    private String createFolder(JSONObject requestParams, JSONObject jsonShip){
        String reactionMechanism = requestParams.optString("reactionmechanism");
        JSONArray newwaste= getNewWasteAsync(reactionMechanism, jsonShip);

        requestParams.put("waste", newwaste);
        //result = execute("/JPS/ADMSAgent", requestParams.toString(), HttpPost.METHOD_NAME);
        String result= AgentCaller.executePost("/JPS/ADMSAgent", requestParams.toString());
        String folder = new JSONObject(result).getString("folder");
        return folder;
    }

    private JSONArray getNewWasteAsync(String reactionMechanism, JSONObject jsonShip) {
        JSONArray newwaste = new JSONArray();
        ArrayList<CompletableFuture> wastes = new ArrayList<>();
        JSONArray ships = jsonShip.getJSONObject("collection").getJSONArray("items");
        int sizeofshipselected = ships.length();
        for (int i = 0; i < sizeofshipselected; i++) {
            logger.info("Ship AGENT called: " + i);
            JSONObject jsonReactionShip = new JSONObject();
            jsonReactionShip.put("reactionmechanism", reactionMechanism);
            jsonReactionShip.put("ship", ships.getJSONObject(i));
            //CompletableFuture<String> getAsync = CompletableFuture.supplyAsync(() ->
            //     execute("/JPS_SHIP/ShipAgent", jsonReactionShip.toString()));
            CompletableFuture<String> getAsync = CompletableFuture.supplyAsync(() -> wrapper.executeGet("/JPS_SHIP/ShipAgent", "query", jsonReactionShip.toString() ));
            CompletableFuture<String> processAsync = getAsync.thenApply(wasteResult -> new JSONObject(wasteResult).getString("waste"));
            wastes.add(processAsync);
        }
        for (CompletableFuture waste : wastes) {
            try {
                newwaste.put(waste.get());
            } catch (InterruptedException | ExecutionException e) {
                throw new JPSRuntimeException(e.getMessage());
            }
        }
        return newwaste;
    }
}
