package uk.ac.cam.cares.jps.agent.weather;

import java.net.URI;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;

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
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
    	JSONObject response = null;
    	
    	if (validateInput(requestParams)) {
	    	// will only read the file if it's null
	    	Config.initProperties();
	    	
	    	Config.initProperties();
			RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
			TimeSeriesClient<Long> tsClient = new TimeSeriesClient<Long>(storeClient, Long.class, Config.dburl, Config.dbuser, Config.dbpassword);
			
			WeatherQueryClient weatherClient = new WeatherQueryClient(storeClient, tsClient); 
	    	
			String station = requestParams.getString("station");
			
			//updates station if it's more than 1 hour old
			weatherClient.updateStation(station);
			
	        String path = request.getServletPath();
	        TimeSeries<Long> ts; // time series object containing weather data
	        switch (path) {
	        	case urlPatternHistory:
	        		ts = weatherClient.getHistoricalWeatherData(station, requestParams.getInt("hour"));
	        		break;
	        	case urlPatternLatest:
	        		ts = weatherClient.getLatestWeatherData(station);
	        		break;
	        	default:
	        		String err_msg = "Invalid servlet path";
	        		LOGGER.error(err_msg);
	        		throw new JPSRuntimeException(err_msg);
	        }
	        response = new JSONObject(new Gson().toJson(ts));
    	}
    	
		return response;
    }
    
    @Override
    public boolean validateInput(JSONObject requestParams) {
    	try {
    		new URI(requestParams.getString("station"));
    		return true;
    	} catch (Exception e) {
    		String err_msg = "Invalid input";
    		LOGGER.error(err_msg);
    		throw new JPSRuntimeException(err_msg);
    	}
    }
}
