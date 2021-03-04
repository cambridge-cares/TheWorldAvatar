package uk.ac.cam.cares.jps.virtualsensor.visualisation;

import java.io.IOException;
import java.net.URL;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.virtualsensor.sparql.SensorSparql;

@WebServlet("/QuerySensorProperties")
public class QuerySensorProperties extends JPSHttpServlet{
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
		if (validateInput(request)) {
			JSONObject r = AgentCaller.readJsonParameter(request);

			String stationiri = r.getString(Region.keyAirStationIRI);
			JSONArray result = SensorSparql.queryAirStationProperties(stationiri);
			response.setContentType("application/json");
			response.getWriter().write(result.toString());
		}
    }

    private boolean validateInput(HttpServletRequest request) {
    	boolean valid = false;
    	try {
    		JSONObject r = AgentCaller.readJsonParameter(request);
    		new URL(r.getString(Region.keyAirStationIRI)).toURI();
    		valid = true;
    	} catch (Exception e) {
    		throw new BadRequestException(e);
    	}
    	return valid;
    }
}
