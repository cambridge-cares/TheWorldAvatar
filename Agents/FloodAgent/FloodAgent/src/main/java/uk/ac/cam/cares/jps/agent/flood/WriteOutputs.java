package uk.ac.cam.cares.jps.agent.flood;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.agent.flood.objects.Station;
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
    private static String mainDirectory = "main";
    
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

    	// restrict query area based on these environment variables
    	// need to be in the format of lat#lon, e.g. 50#0.1
    	String southwest = System.getenv("SOUTH_WEST");
    	String northeast = System.getenv("NORTH_EAST");
    	
    	List<Station> stations = sparqlClient.getStationsWithCoordinates(southwest, northeast);
    	
    	// remove old outputs if exist
    	removeOldOutput();
    	
    	// create directory
    	File directories = new File(Paths.get(Config.outputdir, dataFolder, mainDirectory).toString());
    	directories.mkdirs();
    	
    	// then write the files..
    	writeOverallMetaFile();
    	writeLayerTree();
    	writeMainMeta(date);
    	writeStationsToGeojson(stations);
    	writeStationsMeta(stations);
    	writeTimeSeriesJson(stations, date);
	}
	
	static void removeOldOutput() {
		LOGGER.info("Trying to delete old data files");
		Path dataFolderPath = null;
		try {
			dataFolderPath = Paths.get(Config.outputdir, dataFolder);
			FileUtils.cleanDirectory(dataFolderPath.toFile());
		} catch (IOException | IllegalArgumentException e) {
			LOGGER.warn(e.getMessage());
		}
	}
	
	static void writeOverallMetaFile() {
		double[] mapcentre = {0.3976, 52.7543};
	    
        File file = new File(Paths.get(Config.outputdir, dataFolder, "meta.json").toString());
		
		JSONObject json = new JSONObject();
		JSONObject global = new JSONObject();
		global.put("defaultCenter", mapcentre);
		global.put("defaultZoom", 8.75);
		global.put("defaultBearing", 0.0);
		global.put("defaultPitch", 0.0);

		JSONObject local = new JSONObject();
		local.put("label", "Group");
		JSONArray groups = new JSONArray();
		JSONObject group1 = new JSONObject();
		group1.put("name", "Main");
		group1.put("directory", mainDirectory);
		groups.put(group1);
		local.put("groups", groups);

		json.put("global", global);
		json.put("local", local);
		
		// write to file
		writeToFile(file, json.toString(4));
	}
	
	static void writeLayerTree() {
		File file = new File(Paths.get(Config.outputdir, dataFolder, "tree.json").toString());
		
		JSONArray layers = new JSONArray();
		
		// layer tree
		JSONObject layer = new JSONObject();
		layer.put("layerName", "River stations");
		layer.put("defaultState", "visible");
		layer.put("layerIDs", new JSONArray().put("stations"));
		layers.put(layer);
		
		// write to file
		writeToFile(file, layers.toString(4));
	}
	
	static void writeMainMeta(LocalDate date) {
		File file = new File(Paths.get(Config.outputdir, dataFolder, mainDirectory, "meta.json").toString());
		
		JSONObject json = new JSONObject();
		
		JSONArray dataSets = new JSONArray();
			
		JSONObject dataSet = new JSONObject();
		dataSet.put("name", "stations");
		dataSet.put("dataLocation", "flood-stations.geojson");
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
	static void writeStationsToGeojson(List<Station> stations) {
		File geojsonfile = new File(Paths.get(Config.outputdir,dataFolder, mainDirectory,"flood-stations.geojson").toString());
		
		// create geojson file
		JSONObject featureCollection = new JSONObject();
		featureCollection.put("type", "FeatureCollection");
		JSONArray features = new JSONArray();
		
		for (Station station : stations) {
			// each station will be a feature within FeatureCollection
			JSONObject feature = new JSONObject();
			
			//type
			feature.put("type", "Feature");
			
			//id for mapbox
			feature.put("id", station.getVisId());
			
			//properties (display name and styling)
			JSONObject property = new JSONObject();
			property.put("displayName", station.getLabel());
			property.put("circle-color", "rgb(204,41,41)");
			property.put("circle-stroke-width", 1);
			property.put("circle-stroke-color", "#000000"); // black
			property.put("circle-opacity", 0.75);
			feature.put("properties", property);
			
			// geometry
			JSONObject geometry = new JSONObject();
			geometry.put("type", "Point");
			geometry.put("coordinates", Arrays.asList(station.getLon(), station.getLat()));
			feature.put("geometry", geometry);
			
			// add to main array
			features.put(feature);
		}
		
		featureCollection.put("features", features);
		
		// write to file
		writeToFile(geojsonfile, featureCollection.toString(4));
	}
	
	static void writeStationsMeta(List<Station> stations) {
		File metafile = new File(Paths.get(Config.outputdir,dataFolder, mainDirectory,"stationsmeta.json").toString());
		
		JSONArray metaDataCollection = new JSONArray();
		
		for (Station station : stations) {
			// meta
			JSONObject metadata = new JSONObject();
			
			metadata.put("id", station.getVisId());
			
			for (String property : station.getDisplayProperties().keySet()) {
				metadata.put(property, station.getDisplayProperties().get(property));
			}
			
			// force order on the side panel
			JSONArray properties_order = new JSONArray();
			List<String> preferred_order = Arrays.asList("Name", "River", "Catchment", "Town", "Date opened", "Identifier", "Latitude", "Longitude");
			for (String key : preferred_order) {
				if (station.getDisplayProperties().containsKey(key)) {
					properties_order.put(key);
				}
			}
			metadata.put("display_order", properties_order);
			metaDataCollection.put(metadata);
		}
		
		// write to file
		writeToFile(metafile, metaDataCollection.toString(4));
	}
	
	/**
	 * writes out data for a specific date
	 * @param date
	 */
	static void writeTimeSeriesJson(List<Station> stations, LocalDate date) {
		// write to file 
		File file = new File(Paths.get(Config.outputdir,dataFolder, mainDirectory,"flood-" + date + "-timeseries.json").toString());

		List<String> measures = sparqlClient.getMeasures(stations);
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
