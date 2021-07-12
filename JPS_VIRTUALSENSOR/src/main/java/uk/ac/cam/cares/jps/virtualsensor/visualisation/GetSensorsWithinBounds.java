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
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.virtualsensor.objects.Point;
import uk.ac.cam.cares.jps.virtualsensor.objects.Scope;
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

			Scope sc = new Scope();
			Point lowerCorner = new Point();
			lowerCorner.setX(r.getDouble(Region.keyLowerx));
			lowerCorner.setY(r.getDouble(Region.keyLowery));
			lowerCorner.setSrsname("EPSG:4326");
			
			Point upperCorner = new Point();
			upperCorner.setX(r.getDouble(Region.keyUpperx));
			upperCorner.setY(r.getDouble(Region.keyUppery));
			upperCorner.setSrsname("EPSG:4326");
			
			sc.setLowerCorner(lowerCorner);
			sc.setUpperCorner(upperCorner);
			sc.setSrsName("EPSG:4326");

			JSONArray result = SensorSparql.queryAirStationsWithinScope(sc);
			response.setContentType("application/json");
			response.getWriter().write(result.toString());
		}
    }

    private boolean validateInput(HttpServletRequest request) {
    	boolean valid = false;
    	try {
    		JSONObject r = AgentCaller.readJsonParameter(request);
    		r.getDouble(Region.keyUpperx);
    		r.getDouble(Region.keyUppery);
    		r.getDouble(Region.keyLowerx);
    		r.getDouble(Region.keyLowery);
    		valid = true;
    	} catch (NumberFormatException e) {
    		throw new BadRequestException(e);
    	}
    	return valid;
    }
}

