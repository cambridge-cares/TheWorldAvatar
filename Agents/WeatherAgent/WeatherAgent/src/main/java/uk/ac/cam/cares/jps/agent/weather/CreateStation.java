package uk.ac.cam.cares.jps.agent.weather;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * This servlet should be called using the AgentCaller class with executeGet
 * input to the CreateStation servlet: JSONObject with the key "latlon", the value need to be in the format of "lat#lon"
 * e.g. {"latlon":"0.0#1.0"}. This is the format required by Blazegraph
 * will give the IRI of the created station in the response
 * e.g. {"station: "http://station1"}
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {"/CreateStation"})
public class CreateStation extends JPSAgent {

	private static final long serialVersionUID = 1L;

	// Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(CreateStation.class);
    
    private WeatherQueryClient weatherClient = null;

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
    	JSONObject response = new JSONObject();
    	
    	if (validateInput(requestParams)) {
    		String latlon = requestParams.getString("latlon");
    		
    		Config.initProperties();
    		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
    		TimeSeriesClient<Long> tsClient = new TimeSeriesClient<Long>(storeClient, Long.class, Config.dburl, Config.dbuser, Config.dbpassword);
    		
    		// replaced with mock client in the junit tests
    		if (weatherClient == null ) {
    			weatherClient = new WeatherQueryClient(storeClient, tsClient);
    		}
    		
    		String station = weatherClient.createStation(latlon);
    		response.put("station", station);
    		LOGGER.info("Created weather station <" + station + "> at the given coordinates: " + latlon);
    	}
        return response;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        try {
        	String[] latlon = requestParams.getString("latlon").split("#");
        	Double.parseDouble(latlon[0]); Double.parseDouble(latlon[1]);
        	return true;
        } catch (Exception e) {
        	LOGGER.error(e.getMessage());
        	throw new BadRequestException(e);
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
