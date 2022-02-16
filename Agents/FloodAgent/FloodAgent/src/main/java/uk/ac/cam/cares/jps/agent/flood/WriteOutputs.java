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
		if (Boolean.parseBoolean(System.getenv("SKIP_RIVER"))) {
			LOGGER.info("SKIP_RIVER is set to true, this code will not write any outputs");
			System.exit(0);
		}
		
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
    	
    	Map<String, Station> stations = sparqlClient.getStationsWithCoordinates(southwest, northeast);
    	
    	// add time series to station objects, will also remove stations without any data
    	queryTimeSeries(stations, date);
    	
    	// remove old outputs if exist
    	removeOldOutput();

    	// create directory
    	File directories = new File(Paths.get(Config.outputdir, mainDirectory).toString());
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
			dataFolderPath = Paths.get(Config.outputdir);
			FileUtils.cleanDirectory(dataFolderPath.toFile());
		} catch (IOException | IllegalArgumentException e) {
			LOGGER.warn(e.getMessage());
		}
	}
	
	/**
	 * set time series objects to each station object, if a station does not have any time series
	 * the entry gets removed
	 * @param stations
	 * @param date
	 */
	static void queryTimeSeries(Map<String, Station> stations, LocalDate date) {
		Instant lowerbound = date.atStartOfDay(ZoneOffset.UTC).toInstant();
		Instant upperbound = date.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant().minusSeconds(1);
		List<String> stationsList = new ArrayList<>(stations.keySet());
				
		for (String stationIri : stationsList) {
			try {
				Station station = stations.get(stationIri);
				List<String> measures = station.getMeasures();
				
				for (String measure : measures) {
					TimeSeries<Instant> ts = tsClient.getTimeSeriesWithinBounds(Arrays.asList(measure), lowerbound, upperbound);
					List<Instant> time = ts.getTimes();
					
					// ignore blank time series
					if(time.size() > 0) {
						station.addTimeseries(ts);
					}
				}				
				
				if (station.getTimeSeriesList().size() == 0) {
					// do not plot this station if it does not have any data
					stations.remove(stationIri);
				}
			} catch (Exception e) {
				LOGGER.error(e.getMessage());
				LOGGER.error("Failed to query time series for " + stationIri);
			}
		}
	}
	
	static void writeOverallMetaFile() {
		double[] mapcentre = {0.3976, 52.7543};
	    
        File file = new File(Paths.get(Config.outputdir, "meta.json").toString());
		
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
		File file = new File(Paths.get(Config.outputdir, "tree.json").toString());
		
		JSONArray layers = new JSONArray();
		
		// layer tree
		JSONObject layer = new JSONObject();
		layer.put("layerName", "Environment Agency");
		layer.put("defaultState", "visible");
		layer.put("layerIDs", new JSONArray().put("stations"));
		layers.put(layer);
		
		// write to file
		writeToFile(file, layers.toString(4));
	}
	
	static void writeMainMeta(LocalDate date) {
		File file = new File(Paths.get(Config.outputdir, mainDirectory, "meta.json").toString());
		
		JSONObject json = new JSONObject();
		
		JSONArray dataSets = new JSONArray();
			
		JSONObject dataSet = new JSONObject();
		dataSet.put("name", "stations");
		dataSet.put("dataLocation", "flood-stations.geojson");
		dataSet.put("locationType", "symbol");
		dataSet.put("metaFiles", new JSONArray().put("stationsmeta.json"));
		dataSet.put("timeseriesFiles", new JSONArray().put("flood-" + date + "-timeseries.json"));
		
		// clustering properties
		dataSet.put("cluster", true);
		dataSet.put("clusterRadius", 30);

		// preparing cluster properties
		JSONObject clusterProperties = new JSONObject();
		
		JSONArray icon_image = new JSONArray();
		icon_image.put("string").put("ea-empty");
		
		JSONArray text_colour = new JSONArray();
		text_colour.put("string").put("#68bf56");
		
		clusterProperties.put("icon-image", icon_image);
		clusterProperties.put("text-color", text_colour);
		
		dataSet.put("clusterProperties", clusterProperties);
		
		dataSets.put(dataSet);
		
		json.put("dataSets", dataSets);
		
		// write to file
		writeToFile(file,json.toString(4));
	}
	
	/**
	 * location of file - Config.outputdir
	 * name of file  - stations.geojson
	 */
	static void writeStationsToGeojson(Map<String, Station> stations) {
		File geojsonfile = new File(Paths.get(Config.outputdir, mainDirectory,"flood-stations.geojson").toString());
		
		// create geojson file
		JSONObject featureCollection = new JSONObject();
		featureCollection.put("type", "FeatureCollection");
		JSONArray features = new JSONArray();
		
		for (String stationIri : stations.keySet()) {
			Station station = stations.get(stationIri);
			// each station will be a feature within FeatureCollection
			JSONObject feature = new JSONObject();
			
			//type
			feature.put("type", "Feature");
			
			//id for mapbox
			feature.put("id", station.getVisId());
			
			//properties (display name and styling)
			JSONObject property = new JSONObject();
			property.put("displayName", "Environment Agency: " + station.getLabel() + " (" + station.getIdentifier() + ")");
			property.put("description", station.getDescription());
			// icon properties
			property.put("icon-image", station.getIconImage());
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
	
	static void writeStationsMeta(Map<String, Station> stations) {
		File metafile = new File(Paths.get(Config.outputdir, mainDirectory,"stationsmeta.json").toString());
		
		JSONArray metaDataCollection = new JSONArray();
		
		for (String stationIri : stations.keySet()) {
			Station station = stations.get(stationIri);
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
	static void writeTimeSeriesJson(Map<String, Station> stations, LocalDate date) {
		// write to file 
		File file = new File(Paths.get(Config.outputdir, mainDirectory,"flood-" + date + "-timeseries.json").toString());
		
		// collect time series into a list first
		List<TimeSeries<Instant>> ts_list = new ArrayList<>();
		List<Map<String,String>> table_header = new ArrayList<>();
		List<Map<String,String>> units = new ArrayList<>();
		List<Integer> visId = new ArrayList<>();
		
		for (String stationIri : stations.keySet()) {
			Station station = stations.get(stationIri);
			TimeSeries<Instant> combined_ts = station.getCombinedTimeSeries(tsClient);
			ts_list.add(combined_ts);
			
			Map<String,String> measure_header_map = new HashMap<>();
			Map<String,String> measure_unit_map = new HashMap<>();
			
			for (String measureIri : combined_ts.getDataIRIs()) {
				String header = station.getMeasureName(measureIri) + " (" + station.getMeasureSubTypeName(measureIri) + ")";
				measure_header_map.put(measureIri, header);
				measure_unit_map.put(measureIri, station.getMeasureUnit(measureIri));
			}
			table_header.add(measure_header_map);
			units.add(measure_unit_map);
			visId.add(station.getVisId());
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
