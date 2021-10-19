package uk.ac.cam.cares.jps.agent.flood;

import java.io.IOException;
import java.net.URISyntaxException;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.http.HttpEntity;
import org.apache.http.ParseException;
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
    
    // boolean to check if RDB data is uploaded
    static boolean updated;
    
    // err msg
    private static final String ARG_MISMATCH = "Only one date argument is allowed";
	
    // will be replaced with mocks in junit tests
    private static APIConnector api = null;
    private static FloodSparql sparqlClient = null;
    private static TimeSeriesClient<Instant> tsClient = null;
    
    // setters to replace these with mocks
    public static void setAPIConnector(APIConnector api) {
    	UpdateStations.api = api;
    }
    public static void setSparqlClient(FloodSparql sparqlClient) {
    	UpdateStations.sparqlClient = sparqlClient;
    }
    public static void setTsClient(TimeSeriesClient<Instant> tsClient) {
    	UpdateStations.tsClient = tsClient;
    }
    
	public static void main(String[] args) {
		Config.initProperties();
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

		// if these are null, they are in deployed mode, otherwise they should
    	// be set with mocks using their respective setters
    	if (api == null) {
    		UpdateStations.api = new APIConnector("http://environment.data.gov.uk/flood-monitoring/data/readings");
    		UpdateStations.api.setParameter("date", date.toString());
    	}
    	if (sparqlClient == null) {
    		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl, Config.kguser, Config.kgpassword);
    		UpdateStations.sparqlClient = new FloodSparql(storeClient);
    	}
    	if (tsClient == null) {
    		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl, Config.kguser, Config.kgpassword);
    		UpdateStations.tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
    	}
    	
		List<Map<?,?>> processed_data;
    	try {            
            // process data into tables before upload
            processed_data = processAPIResponse(api);
    	} catch (Exception e) {
    		LOGGER.error(e.getMessage());
    		throw new JPSRuntimeException(e);
    	}
        
        updated = false;
        // upload to postgres
        uploadDataToRDB(date, tsClient, sparqlClient, processed_data);
        
        // update last updated date
        sparqlClient.updateLastDate(date);
	}
	
	/**
	 * puts data into datatime_map and datavalue_map
	 * first element is datatime_map, second element is datavalue_map
	 * @param response
	 * @throws IOException 
	 * @throws ParseException 
	 * @throws URISyntaxException 
	 */
	static List<Map<?,?>> processAPIResponse(APIConnector api) throws ParseException, IOException, URISyntaxException {
		LOGGER.info("Processing data from API");
		HttpEntity response = api.getData();
		// convert response to JSON Object
		String response_string = EntityUtils.toString(response);
        JSONObject response_jo = new JSONObject(response_string);
        JSONArray readings = response_jo.getJSONArray("items");
        
        // collect data belonging to the same URL into lists
        // this reduces the number of uploads required
        Map<String, List<Instant>> datatime_map = new HashMap<>();
        Map<String, List<Double>> datavalue_map = new HashMap<>();
        String dataIRI = null;
        int num_fail = 0;
        for (int i = 0; i < readings.length(); i++) {
        	try {
	        	dataIRI = readings.getJSONObject(i).getString("measure");
	        	
	        	// if it is a JSON Array, take the average
	        	// not clear why more than 1 value is given
	        	Double value = null;
	        	try {
	        		value = readings.getJSONObject(i).getDouble("value");
	        	} catch (Exception e) {
	        		// some data points have 2 values, not sure why
	        		// in this case we take the average
	        		value = readings.getJSONObject(i).getJSONArray("value").toList().stream().mapToDouble(x -> (double) x).average().getAsDouble();
	        		LOGGER.info("More than 1 value is given for a data point");
	        		LOGGER.info(readings.getJSONObject(i));
	        		LOGGER.info("Taking the average");
	        	}
	        	
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
        
        List<Map<?,?>> processed_data = new ArrayList<>();
        processed_data.add(datatime_map);
        processed_data.add(datavalue_map);
        
        LOGGER.info("Received a total of " + Integer.toString(readings.length())+ " readings");
        LOGGER.info("Organised into " + Integer.toString(datatime_map.size()) + " groups");
        LOGGER.info("Failed to process " + Integer.toString(num_fail)+ " readings");
        
        return processed_data;
	}
	
	@SuppressWarnings("unchecked")
	static void uploadDataToRDB(LocalDate date, TimeSeriesClient<Instant> tsClient, FloodSparql sparqlClient,
			List<Map<?,?>> processed_data) {
		Map<String, List<Instant>> datatime_map = (Map<String, List<Instant>>) processed_data.get(0);
		Map<String, List<Double>> datavalue_map = (Map<String, List<Double>>) processed_data.get(1);
        Iterator<String> iter = datatime_map.keySet().iterator();
        int num_failures = 0;
        
        LOGGER.info("Uploading data to postgres");
        while (iter.hasNext()) {
        	String dataIRI = iter.next();
        	
        	// try to initialise table if it does not exist
        	if (!tsClient.checkDataHasTimeSeries(dataIRI)) {
        		LOGGER.info(dataIRI + " is not present in the initial rdf data");
    			LOGGER.info("Attempting to initialise <" + dataIRI + ">");
    			
				try {
					// Obtain station name for this measure
					HttpEntity response = new APIConnector(dataIRI).getData();
					JSONObject response_jo = new JSONObject(EntityUtils.toString(response));
					
					// get the station that measures this quantity
					JSONObject items = response_jo.getJSONObject("items");
					String station = items.getString("station");
					String unit = items.getString("unitName");
					String parameterName = items.getString("parameterName");
					String qualifier = items.getString("parameter");
					
					// add this missing information in blazegraph and rdb
					sparqlClient.addMeasureToStation(station, dataIRI,unit,parameterName,qualifier);
					
					// check if station exists, if not, instantiate
					if (!sparqlClient.checkStationExists(station)) {
						response = new APIConnector(station).getData();
						response_jo = new JSONObject(EntityUtils.toString(response));
						items = response_jo.getJSONObject("items");
						double lat = items.getDouble("lat");
						double lon = items.getDouble("long");
						String stationRef = items.getString("stationReference");
						
						sparqlClient.addNewStation(station, lat, lon, stationRef);
					}
					
					tsClient.initTimeSeries(Arrays.asList(dataIRI), Arrays.asList(Double.class), null);
					
					LOGGER.info("Created new table successfully");
				} catch (Exception e1) {
					num_failures += 1;
					LOGGER.error(e1.getMessage());
					LOGGER.error("Failed to initialise <" + dataIRI + ">");
					continue;
				} 
        	}
        	
        	// avoid adding duplicate data
        	// this may happen when the previous update was terminated halfway
        	// extract date from the latest timestamp
        	try {
	        	LocalDate lastUpdateDate = LocalDateTime.ofInstant(tsClient.getMaxTime(dataIRI), ZoneId.of("UTC")).toLocalDate();
	        	if (!date.isAfter(lastUpdateDate)) {
	        		LOGGER.info(dataIRI + " is already up-to-date");
	        		continue;
	        	}
        	} catch (Exception e) {
        		// dataIRI is initialised but contains no data yet
        	}
        	
        	// create time series object to upload to the client
        	List<List<?>> values = new ArrayList<>();
        	values.add(datavalue_map.get(dataIRI));
        	TimeSeries<Instant> ts = new TimeSeries<Instant>(datatime_map.get(dataIRI), Arrays.asList(dataIRI), values);
        	tsClient.addTimeSeriesData(ts);
        }
        
        LOGGER.info("Failed to add " + Integer.toString(num_failures) + " data set out of the processed data");
        // consider updated if at least one was updated..
        if (num_failures < datatime_map.size()) {
        	updated = true;
        }
        
        tsClient.disconnectRDB();
	}
}
