package uk.ac.cam.cares.jps.agent.weather;

import java.net.URI;
import java.time.Instant;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Updates the given station with latest data from the API
 * Ignores request is station was last updated 30 minutes ago
 * Format of input: {"station": "http://station1"}
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {"/UpdateStation"})
public class UpdateStation extends JPSAgent{

	private static final long serialVersionUID = 1L;
	// for logging
	private static final Logger LOGGER = LogManager.getLogger(UpdateStation.class);

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject response = new JSONObject();
    	
    	if (validateInput(requestParams)) {
	    	// will only read the file if it's null
	    	Config.initProperties();
	    	
	    	Config.initProperties();
			RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
			TimeSeriesClient<Long> tsClient = new TimeSeriesClient<Long>(storeClient, Long.class, Config.dburl, Config.dbuser, Config.dbpassword);
			
			// replaced with mock client in the junit tests
			WeatherQueryClient weatherClient = new WeatherQueryClient(storeClient, tsClient);
	    	
			String station = requestParams.getString("station");
			
			//updates station if it's more than 30 minute old
			long currenttime = Instant.now().getEpochSecond();
			long lastupdate = weatherClient.getLastUpdateTime(station);
			if ((currenttime-lastupdate) > 1800) {
				// this will ensure the servlet will always return a response even if the API call fails
				try {
					weatherClient.updateStation(station);
					LOGGER.info("Updated station: <" + station + "> with latest data");
				} catch (Exception e) {
					LOGGER.error("Weather update failed");
					throw new JPSRuntimeException(e);
				}
			} else {
				LOGGER.info("<" + station + "> was last updated within 30 minutes ago, update request will be ignored");
			}
			response.put("status", "update successful");
    	}
    	return response;
	}
	
	@Override
	public boolean validateInput(JSONObject requestParams) {
		try {
			new URI(requestParams.getString("station"));
			return true;
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			throw new BadRequestException(e);
		}
	}
}
