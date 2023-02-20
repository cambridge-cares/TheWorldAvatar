package uk.ac.cam.cares.jps.agent.weather;

import java.io.IOException;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.entity.ContentType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.springframework.core.io.ClassPathResource;

import com.cmclinnovations.stack.clients.ontop.OntopClient;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Two mandatory parameters: lat, lon
 * optional parameter: name
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {"/CreateStation"})
public class CreateStation extends HttpServlet {
	private static final long serialVersionUID = 1L;

	// Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(CreateStation.class);
    
    private WeatherQueryClient weatherClient;
	private WeatherPostGISClient postgisClient;

	protected void doPut(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		new Config().initProperties();
		LOGGER.info("Received POST request to create a new weather station");
        LOGGER.info("Received request: " + req);

		double lat;
		double lon;
		try {
			lat = Double.parseDouble(req.getParameter("lat"));
			lon = Double.parseDouble(req.getParameter("lon"));
		} catch (Exception e) {
			LOGGER.error("Error parsing input, make sure lat and lon are specified as parameters");
			LOGGER.error(e.getMessage());
			throw new RuntimeException(e);
		}

		// replaced with mock client in the junit tests
		if (weatherClient == null ) {
			RemoteStoreClient kgClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
			TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(kgClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
			RemoteStoreClient ontopClient = new RemoteStoreClient(Config.ontop_url);
			weatherClient = new WeatherQueryClient(kgClient, tsClient, ontopClient);
		}

		if (postgisClient == null) {
			postgisClient = new WeatherPostGISClient(Config.dburl,Config.dbuser, Config.dbpassword);
		}

		String station;
		String response = "";
		try (Connection conn = postgisClient.getConnection()) {
			if (!postgisClient.checkTableExists(Config.LAYERNAME, conn)) {
				// add ontop mapping file
				Path obda_file = new ClassPathResource("ontop.obda").getFile().toPath();
				new OntopClient().updateOBDA(obda_file);
	
				station = weatherClient.createStation(lat,lon,req.getParameter("name"));
				JSONObject response_jo = new JSONObject();
				response_jo.put("station", station);
				response = response_jo.toString();
			} else {
				// table exists, check table contents for an equivalent point
				if (!postgisClient.checkPointExists(lat, lon, conn)) {
					station = weatherClient.createStation(lat,lon,req.getParameter("name"));
					JSONObject response_jo = new JSONObject();
					response_jo.put("station", station);
					response = response_jo.toString();
					resp.setContentType(ContentType.APPLICATION_JSON.getMimeType());
				} else {
					response = "There is already a station at the given coordinates";
				}
			}
		} catch (SQLException e) {
			LOGGER.error("Probably failed to disconnect");
			LOGGER.error(e.getMessage());
		}

		LOGGER.info(response);
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

	void setPostGISClient(WeatherPostGISClient postgisClient) {
		this.postgisClient = postgisClient;
	}
}
