package uk.ac.cam.cares.jps.agent.weather;

import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
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
		LOGGER.info("Received DELETE request");
        new Config().initProperties();
		
		// replaced with mock client in the junit tests
		if (weatherClient == null) {
			RemoteStoreClient kgClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
			TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(kgClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
			weatherClient = new WeatherQueryClient(kgClient, tsClient, null);
		}

		String station = req.getParameter("iri");
		WeatherPostGISClient postgisClient = new WeatherPostGISClient(Config.dburl,Config.dbuser, Config.dbpassword);
		
		String response;
		try (Connection conn = postgisClient.getConnection()) {
			postgisClient.deleteRow(station, conn);
		} catch (SQLException e) {
			LOGGER.error("Probably failed to disconnect from rdb");
			LOGGER.error(e.getMessage());
		}
		weatherClient.deleteStation(station);
		response = "Deleted station: <" + station + ">";
		LOGGER.info(response);

		try {
			resp.getWriter().write(response);
		} catch (IOException e) {
			LOGGER.error(e.getMessage());
			LOGGER.error("Failed to write response");
		}
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
