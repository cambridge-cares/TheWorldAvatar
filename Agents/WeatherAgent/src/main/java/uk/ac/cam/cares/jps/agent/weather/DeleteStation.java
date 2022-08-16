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

@WebServlet(urlPatterns = {"/DeleteStation"})
public class DeleteStation extends HttpServlet{

	private static final long serialVersionUID = 1L;
	// for logging
	private static final Logger LOGGER = LogManager.getLogger(DeleteStation.class);
	private WeatherQueryClient weatherClient = null;

	protected void doDelete(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        new Config().initProperties();
    
		RemoteStoreClient kgClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
		TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(kgClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
		
		// replaced with mock client in the junit tests
		if (weatherClient == null) {
			weatherClient = new WeatherQueryClient(kgClient, tsClient);
		}

		String station = req.getParameter("iri");
		WeatherPostGISClient postgisClient = new WeatherPostGISClient();
		
		String response;
		try {
			postgisClient.deleteRow(station);
			weatherClient.deleteStation(station);
			response = "Deleted station: <" + station + ">";
			LOGGER.info(response);
		} catch (Exception e) {
			response = "Delete station failed";
			LOGGER.error(e.getMessage());
			throw new RuntimeException(e);
		}

		resp.getWriter().write(response);
    }

	/**
     * this setter is created purely for the purpose of junit testing where 
     * the weather client is replaced with a mock client that does not 
     * connect to the weather API
     * @param weatherClient
     */
    void setWeatherQueryClient(WeatherQueryClient weatherClient) {
    	this.weatherClient = weatherClient;
    }
}
