package uk.ac.cam.cares.jps.agent.flood;

import java.io.File;
import java.io.FileOutputStream;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

public class WriteOutputs {
	// Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(WriteOutputs.class);
    
	// will be replaced with mocks in junit tests
    private static FloodSparql sparqlClient = null;
    private static TimeSeriesClient<Instant> tsClient = null;
    
    // err msg
    private static final String ARG_MISMATCH = "Only one date argument is allowed";
    
    // setters to replace these with mocks
    public static void setSparqlClient(FloodSparql sparqlClient) {
    	WriteOutputs.sparqlClient = sparqlClient;
    }
    public static void setTsClient(TimeSeriesClient<Instant> tsClient) {
    	WriteOutputs.tsClient = tsClient;
    }
	
	public static void main(String[] args) {
		// input needs to be a valid date
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
		Config.initProperties();
		if (sparqlClient == null) {
    		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl, Config.kguser, Config.kgpassword);
    		WriteOutputs.sparqlClient = new FloodSparql(storeClient);
    	}
    	if (tsClient == null) {
    		RemoteStoreClient storeClient = new RemoteStoreClient(Config.kgurl,Config.kgurl, Config.kguser, Config.kgpassword);
    		WriteOutputs.tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);
    	}
    	
    	writeStationsToGeojson();
    	writeTimeSeriesJson(date);
	}
	
	/**
	 * location of file - Config.outputdir
	 * name of file  - stations.geojson
	 */
	static void writeStationsToGeojson() {
		File file = new File(Paths.get(Config.outputdir,"stations.geojson").toString());
		
		if (!file.exists()) {
			// create geojson file
			JSONObject featureCollection = new JSONObject();
			featureCollection.put("type", "FeatureCollection");
			JSONArray features = new JSONArray();
			
			// List with three lists, 1 - station names, 2 - lat, 3 - lon
			List<List<?>> queryResults = sparqlClient.getStationsWithCoordinates();
			
			for (int i = 0; i < queryResults.get(0).size(); i++) {
				// each station will be a feature within FeatureCollection
				JSONObject feature = new JSONObject();
				
				List<String> station_names = (List<String>) queryResults.get(0);
				List<Double> lat_values = (List<Double>) queryResults.get(1);
				List<Double> lon_values = (List<Double>) queryResults.get(2);
				
				//type
				feature.put("type", "Feature");
				
				//properties
				JSONObject property = new JSONObject();
				property.put("name", station_names.get(i));
				feature.put("properties", property);
				
				// geometry
				JSONObject geometry = new JSONObject();
				geometry.put("type", "Point");
				geometry.put("coordinates", Arrays.asList(lon_values.get(i), lat_values.get(i)));
				feature.put("geometry", geometry);
				
				// add to main array
				features.put(feature);
			}
			
			featureCollection.put("features", features);
			
			// write to file
			try {
				file.createNewFile();
				FileOutputStream outputStream = new FileOutputStream(file);
				byte[] strToBytes = featureCollection.toString(4).getBytes();
				outputStream.write(strToBytes);
				outputStream.close();
				LOGGER.info("Created " + file.getAbsolutePath());
			} catch (Exception e) {
				LOGGER.error(e.getMessage());
				throw new JPSRuntimeException(e);
			}
		} else {
			LOGGER.info(file.getAbsolutePath() + " already exists, skipping writeStationsToGeojson");
		}
	}
	
	/**
	 * writes out data for a specific date
	 * @param date
	 */
	static void writeTimeSeriesJson(LocalDate date) {
		// write to file 
		File file = new File(Paths.get(Config.outputdir,"flood_" + date + ".json").toString());
		
		if (!file.exists()) {
			List<String> measures = sparqlClient.getMeasures();
			Instant lowerbound = date.atStartOfDay(ZoneOffset.UTC).toInstant();
			Instant upperbound = date.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant().minusSeconds(1);
			
			// collect time series into a list first
			List<TimeSeries<Instant>> ts_list = new ArrayList<>();
			
			for (int i = 0; i < measures.size(); i++) {
				try {
					TimeSeries<Instant> ts = tsClient.getTimeSeriesWithinBounds(Arrays.asList(measures.get(i)), lowerbound, upperbound);
					
					List<Instant> time = ts.getTimes();
					
					// ignore blank time series
					if(time.size() > 0) {
						ts_list.add(ts);
					}
				} catch (Exception e) {
					LOGGER.error(e.getMessage());
					LOGGER.error("Failed to query time series for " + measures.get(i));
				}
			}
			
			// prepare JSON output
			//index 0: data name, list 1: unit, list 2: vis ID
			List<String> measuresToPlot = new ArrayList<>();
			for (TimeSeries<Instant> ts : ts_list) {
				measuresToPlot.add(ts.getDataIRIs().get(0));
			}
			
			List<List<?>> measureProps = sparqlClient.getMeasurePropertiesForVis(measuresToPlot);
			
			List<List<String>> table_header = new ArrayList<>();
			for (String header : (List<String>) measureProps.get(0)) {
				table_header.add(Arrays.asList(header));
			}
			
			List<List<String>> units = new ArrayList<>();
			for (String unit : (List<String>) measureProps.get(1)) {
				units.add(Arrays.asList(unit));
			}
			
			JSONArray ts_array = tsClient.convertToJSON(ts_list, 
					(List<Integer>) measureProps.get(2), units, table_header);
			
			// write to file
			try {
				file.createNewFile();
				FileOutputStream outputStream = new FileOutputStream(file);
				byte[] strToBytes = ts_array.toString(4).getBytes();
				outputStream.write(strToBytes);
				outputStream.close();
				LOGGER.info("Created " + file.getAbsolutePath());
			} catch (Exception e) {
				LOGGER.error(e.getMessage());
				throw new JPSRuntimeException(e);
			}
		} else {
			LOGGER.info(file.getAbsolutePath() + " already exists, skipping writeTimeSeriesJson");
		}
	}
}
