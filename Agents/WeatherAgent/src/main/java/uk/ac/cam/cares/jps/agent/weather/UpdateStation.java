package uk.ac.cam.cares.jps.agent.weather;

import java.io.IOException;
import java.time.Instant;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Updates the given station with latest data from the API
 * Ignores request is station was last updated 30 minutes ago
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {"/UpdateStation"})
public class UpdateStation extends HttpServlet{

	private static final long serialVersionUID = 1L;
	// for logging
	private static final Logger LOGGER = LogManager.getLogger(UpdateStation.class);

	protected void doPut(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {   
		new Config().initProperties(); 	
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
		TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
		
		WeatherQueryClient weatherClient = new WeatherQueryClient(storeClient, tsClient);
		
		String station = req.getParameter("iri");
		
		//updates station if it's more than 30 minute old
		String response;
		Instant currenttime = Instant.now();
		Instant lastupdate = weatherClient.getLastUpdateTime(station);
		if ((currenttime.getEpochSecond()-lastupdate.getEpochSecond()) > 1800) {
			// this will ensure the servlet will always return a response even if the API call fails
			try {
				weatherClient.updateStation(station);
				response = "Updated station: <" + station + "> with latest data";
			} catch (Exception e) {
				response = "Weather update failed";
				LOGGER.error(response);
				LOGGER.error(e.getMessage());
				throw new RuntimeException(e);
			}
		} else {
			response = "<" + station + "> was last updated within 30 minutes ago, update request will be ignored";
		}
		resp.getWriter().write(response);
	}
}
