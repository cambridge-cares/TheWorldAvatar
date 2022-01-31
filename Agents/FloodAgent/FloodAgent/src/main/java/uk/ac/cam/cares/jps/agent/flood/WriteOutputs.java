package uk.ac.cam.cares.jps.agent.flood;

import java.io.File;
import java.io.FileOutputStream;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
    
    // output files
    private static String dataFolder = "data";
    private static String fixedDirectory = "fixed";
    
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
    	
    	// create directory
    	File directories = new File(Paths.get(Config.outputdir, dataFolder, fixedDirectory).toString());
    	if (!directories.exists()) {
    		directories.mkdirs();
    	}
    	writeOverallMetaFile();
    	writeLayerTree();
    	writeFixedMeta(date);
    	
    	List<List<?>> queryResults = sparqlClient.getStationsWithCoordinates();
    	writeStationsToGeojson(queryResults);
    	writeStationsMeta(queryResults);
    	writeTimeSeriesJson(date);
	}
	
	static void writeOverallMetaFile() {
		double[] mapcentre = {0.3976, 52.7543};
	    String selectionsTitle = "Current Scenario:";
	    
        File file = new File(Paths.get(Config.outputdir, dataFolder, "overall-meta.json").toString());
		
		JSONObject json = new JSONObject();
		json.put("name", "Flood monitoring stations");
		json.put("description", "Locations of stations");
		json.put("apiKey", Config.apiKey);
		json.put("defaultCenter", mapcentre);
		json.put("defaultZoom", 8);
		json.put("selectionsTitle", selectionsTitle);
		json.put("fixedDirectory", fixedDirectory);
		json.put("defaultBearing", 0.0);
		json.put("defaultPitch", 0.0);
		
		// write to file
		writeToFile(file, json.toString(4));
	}
	
	static void writeLayerTree() {
		File file = new File(Paths.get(Config.outputdir, dataFolder, "layer-tree.json").toString());
		
		JSONObject assets = new JSONObject();
		assets.put("groupName", "Assets");
		
		JSONArray layers = new JSONArray();
		
		// layer tree
		JSONObject layer = new JSONObject();
		layer.put("layerName", "stations");
		layer.put("defaultState", "visible");
		layer.put("layerIDs", new JSONArray().put("stations"));
		layers.put(layer);
		
		assets.put("layers", layers);
		
		JSONArray allLayer = new JSONArray().put(assets);
		
		// write to file
		writeToFile(file, allLayer.toString(4));
	}
	
	static void writeFixedMeta(LocalDate date) {
		File file = new File(Paths.get(Config.outputdir, dataFolder, fixedDirectory, "meta.json").toString());
		
		JSONObject json = new JSONObject();
		
		JSONArray dataSets = new JSONArray();
			
		JSONObject dataSet = new JSONObject();
		dataSet.put("name", "stations");
		dataSet.put("locationFile", "flood-stations.geojson");
		dataSet.put("locationType", "point");
		dataSet.put("metaFiles", new JSONArray().put("stationsmeta.json"));
		dataSet.put("timeseriesFiles", new JSONArray().put("flood-" + date + "-timeseries.json"));
		
		dataSets.put(dataSet);
		
		json.put("dataSets", dataSets);
		
		// write to file
		writeToFile(file,json.toString(4));
	}
	
	/**
	 * location of file - Config.outputdir
	 * name of file  - stations.geojson
	 */
	static void writeStationsToGeojson(List<List<?>> queryResults) {
		File geojsonfile = new File(Paths.get(Config.outputdir,dataFolder, fixedDirectory,"flood-stations.geojson").toString());
		
		// create geojson file
		JSONObject featureCollection = new JSONObject();
		featureCollection.put("type", "FeatureCollection");
		JSONArray features = new JSONArray();
		
		// List with three lists, 0 - station names, 1 - lat, 2 - lon, 3 - id		
		List<String> station_names = (List<String>) queryResults.get(0);
		List<Double> lat_values = (List<Double>) queryResults.get(1);
		List<Double> lon_values = (List<Double>) queryResults.get(2);
		List<Integer> ids = (List<Integer>) queryResults.get(3);
		
		for (int i = 0; i < queryResults.get(0).size(); i++) {
			// each station will be a feature within FeatureCollection
			JSONObject feature = new JSONObject();
			
			//type
			feature.put("type", "Feature");
			
			//id for mapbox
			feature.put("id", ids.get(i));
			
			//properties (display name and styling)
			JSONObject property = new JSONObject();
			property.put("displayName", station_names.get(i));
			property.put("circle-color", "rgb(204,41,41)");
			property.put("circle-stroke-width", 1);
			property.put("circle-stroke-color", "#000000"); // black
			property.put("circle-opacity", 0.75);
			
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
		writeToFile(geojsonfile, featureCollection.toString(4));
	}
	
	static void writeStationsMeta(List<List<?>> queryResults) {
		File metafile = new File(Paths.get(Config.outputdir,dataFolder, fixedDirectory,"stationsmeta.json").toString());
		
		List<String> station_names = (List<String>) queryResults.get(0);
		List<Integer> ids = (List<Integer>) queryResults.get(3);
		
		JSONArray metaDataCollection = new JSONArray();
		
		for (int i = 0; i < queryResults.get(0).size(); i++) {
			// meta
			JSONObject metadata = new JSONObject();
			metadata.put("id", ids.get(i));
			metadata.put("name",station_names.get(i));
			metaDataCollection.put(metadata);
		}
		
		// write to file
		writeToFile(metafile, metaDataCollection.toString(4));
	}
	
	/**
	 * writes out data for a specific date
	 * @param date
	 */
	static void writeTimeSeriesJson(LocalDate date) {
		// write to file 
		File file = new File(Paths.get(Config.outputdir,dataFolder, fixedDirectory,"flood-" + date + "-timeseries.json").toString());

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
		
		// index 0: data name, 1: unit, 2: vis ID
		List<Map<String,?>> measureProps = sparqlClient.getMeasurePropertiesForVis(measuresToPlot);
		
		List<Map<String,String>> table_header = new ArrayList<>();
		List<Map<String,String>> units = new ArrayList<>();
		List<Integer> visId = new ArrayList<>();
		for (String measure : measuresToPlot) {
			Map<String,String> measure_header_map = new HashMap<>();
			measure_header_map.put(measure, (String) measureProps.get(0).get(measure));
			table_header.add(measure_header_map);
			
			Map<String,String> measure_unit_map = new HashMap<>();
			measure_unit_map.put(measure, (String) measureProps.get(1).get(measure));
			units.add(measure_unit_map);
			
			visId.add((Integer) measureProps.get(2).get(measure));
		}
		
		JSONArray ts_array = tsClient.convertToJSON(ts_list, 
				visId, units, table_header);
		
		// write to file
		writeToFile(file, ts_array.toString(4));
	}
	
	private static void writeToFile(File file, String output) {
		try {
			file.createNewFile();
			FileOutputStream outputStream = new FileOutputStream(file);
			byte[] strToBytes = output.getBytes();
			outputStream.write(strToBytes);
			outputStream.close();
			LOGGER.info("Created " + file.getAbsolutePath());
		} catch (Exception e) {
			LOGGER.error(e.getMessage());
			throw new JPSRuntimeException(e);
		}
	}
}
