package uk.ac.cam.cares.jps.coordination;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

@WebServlet("/ADMSCoordinationAgentForShipWithoutComposition")
public class ADMSCoordinationAgentForShipWithoutComposition extends HttpServlet {

	private static final long serialVersionUID = -2264681360832342804L;
	Logger logger = LoggerFactory.getLogger(ADMSCoordinationAgentForShipWithoutComposition.class);

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
	
		String jsonInput = AgentCaller.readJsonParameter(request).toString();		
		JSONObject result = executeWithoutComposition(jsonInput);
		AgentCaller.writeJsonParameter(response, result);
	}
	
	public JSONObject executeWithoutComposition(String jsonInput) {
		
		try {
			
			JSONObject jo = new JSONObject(jsonInput);
			
			// get a serialized JSON array of ship IRIs
			String jsonArrayOfShipIRI = AgentCaller.executeGet("/JPS_SHIP/GetShipListFromRegion", "query", jsonInput);
			
			JSONObject jsonShipIRIs = new JSONObject(jsonArrayOfShipIRI);
			JSONArray shipIRIs = jsonShipIRIs.getJSONArray("shipIRIs");
			jo.put("ship", shipIRIs);
			
			JSONObject jsonReactionShip = new JSONObject();
			String reactionMechanism = jo.getString("reactionmechanism");
			jsonReactionShip.put("reactionmechanism", reactionMechanism);
						
//			for (int i = 0; i < shipIRIs.length(); i++) {
//				String shipIRI = shipIRIs.getString(i);
//				jsonReactionShip.put("ship", shipIRI);
//				
//				String wasteResult = AgentCaller.executeGet("/JPS_SHIP/ShipAgent", "query", jsonReactionShip.toString());
//				String waste = new JSONObject(wasteResult).getString("waste");
////				jo.put("waste", waste);
//			}
			// TODO: SC
			// Iterate over list of ship iris and perform query of each ship.
			
						
			String regionToCityResult = execute("/JPS/RegionToCity", jsonInput);
			String city = new JSONObject(regionToCityResult).getString("city");
			jo.put("city", city);
			
			String result = execute("/JPS/GetBuildingListFromRegion", jo.toString());
			JSONArray building = new JSONObject(result).getJSONArray("building");
			jo.put("building", building);
			
			result = execute("/JPS_COMPOSITION/CityToWeather", regionToCityResult);
			JSONObject weatherstate = new JSONObject(result).getJSONObject("weatherstate");
			jo.put("weatherstate", weatherstate);
			
//			result = execute("/JPS/ADMSAgent", jo.toString());
//			String folder = new JSONObject(result).getString("folder");
//			jo.put("folder", folder);
			
			return jo;
			
		} catch (JSONException e) {
			logger.error(e.getMessage(), e);
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	private String execute(String path, String jsonInput) {

		logger.info("execute for path=" + path + ", json=" + jsonInput);
		String result = AgentCaller.executeGet(path, "query", jsonInput);
		logger.info("execution result=" + result);
		return result;
	}
}
