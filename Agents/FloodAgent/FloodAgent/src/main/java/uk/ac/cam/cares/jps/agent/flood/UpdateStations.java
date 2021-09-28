package uk.ac.cam.cares.jps.agent.flood;

import java.time.Instant;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

/**
 * Downloads data for 1 day and uploads it to the PostgreSQL database
 * Input: date in the form "2021-09-20"
 * @author Kok Foong Lee
 *
 */
public class UpdateStations {
	// Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(UpdateStations.class);
    
    static Map<String, List<Instant>> datatime_map;
    static Map<String, List<Double>> datavalue_map;
    
    // err msg
    private static final String ARG_MISMATCH = "Only one date argument is allowed";
	
	public static void main(String[] args) {
		LocalDate date;
		
		// input validation
		if (args.length != 1) {
			LOGGER.error(ARG_MISMATCH);
			throw new JPSRuntimeException(ARG_MISMATCH);
		}
		try {
			date = LocalDate.parse(args[0]);
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(e);
		}

		// obtain data from API for a specific date
        String response = getDataFromAPI(date);
        
        // process data into tables before upload
        processAPIResponse(response);
        
        // upload to postgres
        uploadDataToRDB();
	}
	
	private static String getDataFromAPI(LocalDate date) {
		try {
			URIBuilder gov_URL = new URIBuilder("http://environment.data.gov.uk/flood-monitoring/data/readings")
					.setParameter("date", date.toString());
			// option to set the limit for testing purposes
//			gov_URL.addParameter("_limit", "1000");
	        HttpGet request = new HttpGet(gov_URL.build());
			
			CloseableHttpClient httpclient = HttpClients.createDefault();
			
			LOGGER.debug("Downloading data from API");
	        CloseableHttpResponse response = httpclient.execute(request);
	        LOGGER.debug("Download complete");
	        
	        return EntityUtils.toString(response.getEntity());
		} catch (Exception e) {
		    LOGGER.error(e.getMessage());
		    throw new JPSRuntimeException(e);
		}	
	}
	
	/**
	 * puts data into datatime_map and datavalue_map
	 * @param response
	 */
	static void processAPIResponse(String response) {
		LOGGER.info("Processing data from API");
		// convert response to JSON Object
        JSONObject response_jo = new JSONObject(response);
        JSONArray readings = response_jo.getJSONArray("items");
        
        // collect data belonging to the same URL into lists
        // this reduces the number of uploads required
        datatime_map = new HashMap<>();
        datavalue_map = new HashMap<>();
        String dataIRI = null;
        int num_fail = 0;
        for (int i = 0; i < readings.length(); i++) {
        	try {
	        	dataIRI = readings.getJSONObject(i).getString("measure");
	        	double value = readings.getJSONObject(i).getDouble("value");
	        	Instant timestamp = Instant.parse(readings.getJSONObject(i).getString("dateTime"));
	        	
	        	if (datatime_map.containsKey(dataIRI)) {
	        		// add timestamp to the list
	        		datatime_map.get(dataIRI).add(timestamp);
	        		datavalue_map.get(dataIRI).add(value);
	        	} else {
	        		// instantiate new lists and add them to the map
	        		List<Instant> times = new ArrayList<>();
	        		List<Double> values = new ArrayList<>();
	        		times.add(timestamp);
	        		values.add(value);
	        		datatime_map.put(dataIRI, times);
	        		datavalue_map.put(dataIRI, values);
	        	}
        	} catch (Exception e) {
        		num_fail += 1;
        		LOGGER.error(readings.getJSONObject(i));
        		LOGGER.error(e.getMessage());
        	}
        }
        LOGGER.info("Received a total of " + Integer.toString(readings.length())+ " readings");
        LOGGER.info("Organised into " + Integer.toString(readings.length()) + " groups");
        LOGGER.info("Failed to add " + Integer.toString(num_fail)+ " readings due to inconsistencies");
	}
	
	static void uploadDataToRDB() {
		Config.initProperties();
        // create a time series object for each data set and upload to db 1 by 1
        RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl,Config.kguser,Config.kgpassword);
		TimeSeriesClient<Instant> tsClient = 
				new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
        
        Iterator<String> iter = datatime_map.keySet().iterator();
        int num_failures = 0;
        LOGGER.info("Uploading data to postgres");
        while (iter.hasNext()) {
        	String dataIRI = iter.next();
        	List<List<?>> values = new ArrayList<>();
        	values.add(datavalue_map.get(dataIRI));
        	
        	TimeSeries<Instant> ts = new TimeSeries<Instant>(datatime_map.get(dataIRI), Arrays.asList(dataIRI), values);
        	try {
        		tsClient.addTimeSeriesData(ts);
        	} catch (Exception e) {
        		num_failures += 1;
        		LOGGER.error("Failed to add <" + dataIRI + ">");
        		LOGGER.error(e.getMessage());
        	}
        }
        LOGGER.info("Failed to add " + Integer.toString(num_failures) + " values from API");
	}
}
