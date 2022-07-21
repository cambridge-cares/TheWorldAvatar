package uk.ac.cam.cares.jps.agent.weather;

import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * returns a list of stations within the given circle
 * 
 * Inputs: {"centre": lat#lon, "radius": x}
 * lat, lon should be numerics (latitude and longitude)
 * x is the radius in kilometres (a number)
 * 
 * output: {"station": [LIST OF STATION IRI]}
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {"/GetStationsInCircle"})
public class GetStationsInCircle extends JPSAgent{

	// Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(GetStationsInCircle.class);
	private static final long serialVersionUID = 1L;

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		JSONObject response = new JSONObject();
		
		if (validateInput(requestParams)) {
			Config.initProperties();
    		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
    		WeatherQueryClient weatherClient = new WeatherQueryClient(storeClient);
    		
			String centre = requestParams.getString("centre");
			double radius = requestParams.getDouble("radius");
			
			try {
				List<String> stations = weatherClient.getStationsInCircle(centre, radius);
				response.put("station", stations);
			} catch (Exception e) {
				LOGGER.error("Query failed");
				throw new JPSRuntimeException(e);
			}	
		}
		
		return response;
	}
	
	@Override
	public boolean validateInput(JSONObject requestParams) {
		try {
			String[] centre = requestParams.getString("centre").split("#");
			Double.parseDouble(centre[0]); //lat
			Double.parseDouble(centre[1]); //lon
			requestParams.getDouble("radius");
			return true;
		} catch (Exception e)  {
			LOGGER.error(e.getMessage());
			throw new BadRequestException(e);
		}
	}
}
