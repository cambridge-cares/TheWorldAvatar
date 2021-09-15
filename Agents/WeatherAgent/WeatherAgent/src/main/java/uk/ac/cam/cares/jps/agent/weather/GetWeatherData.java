package uk.ac.cam.cares.jps.agent.weather;

import java.net.URI;
import java.time.Instant;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Overview: takes a station IRI as input and returns a serialised time series object using gson
 * 
 * Accepts two URLs: 1) WeatherAgent/GetWeatherData/history - history
 * 2) WeatherAgent/GetWeatherData/latest - only gives the latest data
 * 
 * Call this servlet using the AgentCaller class
 * Mandatory input: "station" as the key, e.g. {"station": "http://stationIRI"}
 * optional input if using "history" option, {"hour": x}, - gives time series from current time up to x hours before current time
 * x should be an integer
 * 
 * Output is a serialised TimeSeries<Long> object using gson, to deserialise it, do the following in your code
 * Type timeSeriesType = new TypeToken<TimeSeries<Long>>() {}.getType();
 * TimeSeries<Long> ts_deserialise = new Gson().fromJson(gson, timeSeriesType);
 * 
 * Automatically updates station with latest data if it's more than 1 hour old
 * @author Kok Foong Lee
 *
 */
@WebServlet(urlPatterns = {GetWeatherData.urlPatternHistory,GetWeatherData.urlPatternLatest})
public class GetWeatherData extends JPSAgent{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// returns entire time series history of this weather station
	static final String urlPatternHistory = "/GetWeatherData/history";
	// only return the latest data in the table
	static final String urlPatternLatest = "/GetWeatherData/latest";
	
	// for logging
	private static final Logger LOGGER = LogManager.getLogger(GetWeatherData.class);
	
	private WeatherQueryClient weatherClient = null;
	
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    	JSONObject response = null;
    	
    	if (validateInput(requestParams, request)) {
	    	// will only read the file if it's null
	    	Config.initProperties();
	    	
	    	Config.initProperties();
			RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
			TimeSeriesClient<Long> tsClient = new TimeSeriesClient<Long>(storeClient, Long.class, Config.dburl, Config.dbuser, Config.dbpassword);
			
			// replaced with mock client in the junit tests
			if (weatherClient == null ) {
    			weatherClient = new WeatherQueryClient(storeClient, tsClient);
    		}
	    	
			String station = requestParams.getString("station");
			
			//updates station if it's more than 1 hour old
			long currenttime = Instant.now().getEpochSecond();
			long lastupdate = weatherClient.getLastUpdateTime(station);
			if ((currenttime-lastupdate) > 3600) {
				// this will ensure the servlet will always return a response even if the API call fails
				try {
					weatherClient.updateStation(station);
				} catch (Exception e) {
					LOGGER.error("API weather update failed, returned values are not up-to-date.");
				}
			}
			
	        String path = request.getServletPath();
	        TimeSeries<Long> ts = null; // time series object containing weather data
	        switch (path) {
	        	case urlPatternHistory:
	        		ts = weatherClient.getHistoricalWeatherData(station, requestParams.getInt("hour"));
	        		break;
	        	case urlPatternLatest:
	        		ts = weatherClient.getLatestWeatherData(station);
	        		break;
	        }
	        // serialise time series object into json
	        response = new JSONObject(new Gson().toJson(ts));
    	}
    	
		return response;
    }
    
    public boolean validateInput(JSONObject requestParams, HttpServletRequest request) {
    	String path = request.getServletPath();
    	try {
    		switch (path) {
	        	case urlPatternHistory:
	        		new URI(requestParams.getString("station"));
	        		requestParams.getInt("hour");
	        		return true;
	        	case urlPatternLatest: // has an extra input required
	        		new URI(requestParams.getString("station"));
	        		return true;
	        	default:
	        		String err_msg = "Invalid servlet path";
	        		LOGGER.error(err_msg);
	        		throw new JPSRuntimeException(err_msg);
	        }
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
