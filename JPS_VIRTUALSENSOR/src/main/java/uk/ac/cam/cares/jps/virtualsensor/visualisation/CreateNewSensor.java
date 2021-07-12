package uk.ac.cam.cares.jps.virtualsensor.visualisation;

import java.io.IOException;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.virtualsensor.objects.Point;
import uk.ac.cam.cares.jps.virtualsensor.sparql.DispSimSparql;
import uk.ac.cam.cares.jps.virtualsensor.sparql.SensorSparql;

/**
 * This servlet is called from the javascript jpsShip.js code to create new air quality stations on the map.
 * First, it will query the existing sensors in the endpoint to get the numbers, then it will
 * create a new one with a unique IRI
 * @author Kok Foong Lee
 *
 */
@WebServlet("/CreateNewSensor")
public class CreateNewSensor extends JPSHttpServlet{
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) {
		if (validateInput(request)) {
			JSONObject r = AgentCaller.readJsonParameter(request);
			double lat = Double.parseDouble(String.valueOf(r.get("lat")));
			double lng = Double.parseDouble(String.valueOf(r.get("lng")));
			double [] xyz = {lng,lat,10}; // hardcoded height
			
			//work out how many stations exist in the endpoint and give a unique name
			//functionality to delete does not exist yet so this won't break!
			int station_number = SensorSparql.GetNumAirStations() + 1;
			String station_name = "virtualsensor" + station_number;
			String stationiri = SensorSparql.createAirQualityStation(station_name, xyz);
			
			// check if any simulations exist at this point
			Point p = new Point();
			p.setX(lng);
			p.setY(lat);
			p.setSrsname("EPSG:4326");
			List<String> sims = DispSimSparql.GetSimIRIForCoordinates(p);
			
			if (sims.size()>0) {
				// there might be more than one sim at this point, but just link 1 to avoid bugs
				DispSimSparql.AddAirQualityStation(sims.get(0), stationiri);
			} else {
				double[] scope_dimensions = {20e3,20e3};
				String sim_iri = DispSimSparql.CreateDispSim(p, scope_dimensions);
				DispSimSparql.AddAirQualityStation(sim_iri, stationiri);
			}
			
			// call SensorUpdaterAgent to populate this sensor with values 
			JSONObject sensor_request = new JSONObject();
			sensor_request.put(SensorSparql.keyAirStationIRI, stationiri);
			AgentCaller.executeGetWithJsonParameter("JPS_VIRTUALSENSOR/SensorUpdaterAgent", sensor_request.toString());

			// javascript needs these in the response, couldn't figure out how to do it nicely in js
			JSONObject output = new JSONObject();
			output.put("lat", lat);
			output.put("lng", lng);
			output.put(SensorSparql.keyAirStationIRI, stationiri);
			response.setContentType("application/json");
			try {
				response.getWriter().write(output.toString()); // javascript needs this for the marker
			} catch (IOException e) {
				throw new JPSRuntimeException(e);
			} 
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
