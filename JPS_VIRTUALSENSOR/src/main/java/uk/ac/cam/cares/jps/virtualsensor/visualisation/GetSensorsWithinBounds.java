package uk.ac.cam.cares.jps.virtualsensor.visualisation;

import java.io.IOException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.region.Region;
import uk.ac.cam.cares.jps.base.region.Scope;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.virtualsensor.sparql.SensorSparql;

/**
 * This servlet is called from the jpsShip javascript and returns the stations within the current bounds
 * of the map.
 * @author Kok Foong Lee
 *
 */

@WebServlet("/GetSensorsWithinBounds")
public class GetSensorsWithinBounds extends JPSHttpServlet{
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
		if (validateInput(request)) {
			JSONObject r = AgentCaller.readJsonParameter(request);
			JSONObject joUppercorner = new JSONObject();
			JSONObject joLowercorner = new JSONObject();
			JSONObject joScope = new JSONObject();
	
			joUppercorner.put(Region.keyUpperx, r.get(Region.keyUpperx));
			joUppercorner.put(Region.keyUppery, r.get(Region.keyUppery));
			joLowercorner.put(Region.keyLowerx, r.get(Region.keyLowerx));
			joLowercorner.put(Region.keyLowery, r.get(Region.keyLowery));
			joScope.put(Region.keyUppercorner, joUppercorner);
			joScope.put(Region.keyLowercorner, joLowercorner);
			joScope.put(Region.keySrsname, "EPSG:4326");

			Scope sc = new Scope(joScope);

			JSONArray result = SensorSparql.queryAirStationsWithinScope(sc);
			response.setContentType("application/json");
			response.getWriter().write(result.toString());
		}
    }

    private boolean validateInput(HttpServletRequest request) {
    	boolean valid = false;
    	try {
    		JSONObject r = AgentCaller.readJsonParameter(request);
    		Double.parseDouble(String.valueOf(r.get(Region.keyUpperx)));
    		Double.parseDouble(String.valueOf(r.get(Region.keyUppery)));
    		Double.parseDouble(String.valueOf(r.get(Region.keyLowerx)));
    		Double.parseDouble(String.valueOf(r.get(Region.keyLowery)));
    		valid = true;
    	} catch (NumberFormatException e) {
    		throw new BadRequestException(e);
    	}
    	return valid;
    }
}

