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
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {"/UpdateStation"})
public class UpdateStation extends HttpServlet{
	private static final long serialVersionUID = 1L;
	// for logging
	private static final Logger LOGGER = LogManager.getLogger(UpdateStation.class);
	WeatherQueryClient weatherClient;

	@Override
	protected void doPut(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		LOGGER.info("Received update request");   
		new Config().initProperties(); 	

		if (weatherClient == null) {
			RemoteStoreClient kgClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
			TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(kgClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
			RemoteStoreClient ontopClient = new RemoteStoreClient(Config.ontop_url);
			
			weatherClient = new WeatherQueryClient(kgClient, tsClient, ontopClient);
		}
		
		String station = req.getParameter("iri");
		String timestampAsString = req.getParameter("timestamp");
		
		String response;
		// this will ensure the servlet will always return a response even if the API call fails
		try {
			weatherClient.updateStation(station, timestampAsString);
			response = "Updated station: <" + station + "> with latest data";
		} catch (Exception e) {
			response = "Weather update failed";
			LOGGER.error(response);
			LOGGER.error(e.getMessage());
			throw new RuntimeException(e);
		}
		resp.getWriter().write(response);
	}

	/**
	 * used for junit test only
	 * @param weatherQueryClient
	 */
	void setWeatherQueryClient(WeatherQueryClient weatherQueryClient) {
		this.weatherClient = weatherQueryClient;
	}
}
