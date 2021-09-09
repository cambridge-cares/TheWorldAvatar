package uk.ac.cam.cares.jps.agent.weather;

import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * returns a list of stations in the given rectangle
 * Execute get request using AgentCaller
 * 
 * input: {"southwest":"lat#lon", "northeast":"lat#lon"}
 * 
 * output: {"station": [LIST OF STATIONS]}
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {"/GetStationsInRectangle"})
public class GetStationsInRectangle extends JPSAgent{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private static final Logger LOGGER = LogManager.getLogger(GetStationsInRectangle.class);

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		JSONObject response = new JSONObject();
		
		if (validateInput(requestParams)) {
			Config.initProperties();
    		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
    		WeatherQueryClient weatherClient = new WeatherQueryClient(storeClient);
    		
			String southwest = requestParams.getString("southwest");
			String northeast = requestParams.getString("northeast");
			
			List<String> stations = weatherClient.getStationsInRectangle(southwest, northeast);
			
			response.put("station", stations);
		}
		
		return response;
	}
	
	@Override
	public boolean validateInput(JSONObject requestParams) {
		try {
			// both inputs are latlon in the format "lat#lon"
			String[] southwest = requestParams.getString("southwest").split("#");
			String[] northeast = requestParams.getString("northeast").split("#");
			Double.parseDouble(southwest[0]); 
			Double.parseDouble(southwest[1]);
			Double.parseDouble(northeast[0]);
			Double.parseDouble(northeast[1]);
			return true;
		} catch (Exception e) {
			String errmsg = "Invalid input for GetStationsInRectangle";
			LOGGER.error(errmsg);
			throw new BadRequestException(errmsg,e);
		}
	}
}
