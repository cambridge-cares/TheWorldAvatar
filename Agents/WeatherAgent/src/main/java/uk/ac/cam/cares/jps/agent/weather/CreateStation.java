package uk.ac.cam.cares.jps.agent.weather;

import java.io.IOException;
import java.nio.file.Path;
import java.time.Instant;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
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
    
    private WeatherQueryClient weatherClient = null;

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

		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
		TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
		
		// replaced with mock client in the junit tests
		if (weatherClient == null ) {
			weatherClient = new WeatherQueryClient(storeClient, tsClient);
		}

		WeatherPostGISClient postgisClient = new WeatherPostGISClient();
		if (!postgisClient.checkTableExists(Config.LAYERNAME)) {
			// add ontop mapping file
			Path obda_file = new ClassPathResource("ontop.obda").getFile().toPath();
			new OntopClient().updateOBDA(obda_file);

			String station = weatherClient.createStation(lat,lon,req.getParameter("name"));
			resp.getWriter().write("Created weather station <" + station + ">");
		} else {
			// table exists, check table contents for an equivalent point
			if (!postgisClient.checkPointExists(lat, lon)) {
				String station = weatherClient.createStation(lat,lon,req.getParameter("name"));
				String response = "Created weather station <" + station + ">";
				LOGGER.info(response);
				resp.getWriter().write(response);
			} else {
				resp.getWriter().write("There is already a station at the given coordinates");
			}
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
