package uk.ac.cam.cares.jps.dispersion.visualisation;

import java.io.IOException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.dispersion.sensorsparql.SensorSparql;

/**
 * This servlet is called from the javascript jpsShip.js code to create new air quality stations on the map.
 * First, it will query the existing sensors in the endpoint to get the numbers, then it will
 * create a new one with a unique IRI
 * @author Kok Foong Lee
 *
 */
@WebServlet("/CreateNewSensor")
public class CreateNewSensor extends JPSHttpServlet{
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
		if (validateInput(request)) {
			JSONObject r = AgentCaller.readJsonParameter(request);
			double lat = Double.parseDouble(String.valueOf(r.get("lat")));
			double lng = Double.parseDouble(String.valueOf(r.get("lng")));
			double [] xy = {lng,lat};
			
			SensorSparql ss = new SensorSparql();
			JSONArray stations = ss.queryAllAirStations();
			
			//work out how many stations exist in the endpoint and give a unique name
			//functionality to delete does not exist yet so this won't break!
			int station_number = stations.length() + 1;
			String station_name = "virtualsensor" + station_number;
			String stationiri = ss.createAirQualityStation(station_name, xy);
			
			// call SensorUpdaterAgent to populate this sensors with values
			// The agent won't do anything if there are no dispersion matrices 
			JSONObject sensor_request = new JSONObject();
			sensor_request.put(Region.keyAirStationIRI, stationiri);
			AgentCaller.executeGetWithJsonParameter("JPS_DISPERSION/SensorUpdaterAgent", sensor_request.toString());
			response.setContentType("application/json");
			response.getWriter().write(sensor_request.toString()); // javascript needs this for the marker
		}
	}

	private boolean validateInput(HttpServletRequest request) {
		boolean valid = false;
    	try {
    		JSONObject r = AgentCaller.readJsonParameter(request);
    		Double.parseDouble(String.valueOf(r.get("lat")));
    		Double.parseDouble(String.valueOf(r.get("lng")));
    		valid = true;
    	} catch (NumberFormatException e) {
    		throw new BadRequestException(e);
    	}
    	return valid;
	}
}
