package uk.ac.cam.cares.jps.agent.thingsboard;

import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONException;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.time.*;
import java.util.*;
import java.util.stream.Collectors;
import java.util.TimeZone;


import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jooq.exception.DataAccessException;


/**
 * Class to retrieve data from the ThingsBoard API and storing it with connection to The World Avatar (Knowledge Base).
 * @author  */
public class ThingsBoardInputAgent{


	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ThingsBoardInputAgentLauncher.class);

    /**
     * The time series client to interact with the knowledge graph and data storage
     */
    private TimeSeriesClient<OffsetDateTime> tsClient;
    /**
     * A list of mappings between JSON keys and the corresponding IRI, contains one mapping per time series
     */
    private List<JSONKeyToIRIMapper> mappings;
    /**
     * The prefix to use when no IRI exists for a JSON key originally
     */
    public static final String generatedIRIPrefix = TimeSeriesSparql.TIMESERIES_NAMESPACE + "thingsboard";
    /**
     * The time unit used for all time series maintained by the ThingsBoard agent
     */
    public static final String timeUnit = OffsetDateTime.class.getSimpleName();
    /**
     * The JSON key for the timestamp
     */
    public static final String timestampKey = "ts";
    /**
     * The Zone offset of the timestamp (https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/ZoneOffset.html)
     */
    public static final ZoneOffset ZONE_OFFSET = ZoneOffset.UTC;

    /**
     * Standard constructor which reads in JSON key to IRI mappings from the config folder
     * defined in the provided properties file.
     * @param propertiesFile The properties file from which to read the path of the mapping folder.
     */
    public ThingsBoardInputAgent(String propertiesFile) throws IOException {
        // Set the mapping between JSON keys and IRIs
        try (InputStream input = new FileInputStream(propertiesFile)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            String mappingFolder;
            try {
            // Read the mappings folder from the properties file
            mappingFolder = System.getenv(prop.getProperty("thingsboard.mappingfolder"));
            }
            catch (NullPointerException e) {
            	throw new IOException ("The key thingsboard.mappingfolder cannot be found in the properties file.");
            }
            if (mappingFolder == null) {
                throw new InvalidPropertiesFormatException("The properties file does not contain the key thingsboard.mappingfolder " +
                        "with a path to the folder containing the required JSON key to IRI mappings.");
            }
            // Read the JSON key to IRI mappings from
            readMappings(mappingFolder);
        }
    }

    /**
     * Retrieves the number of time series the input agent is handling.
     * @return  The number of time series maintained by the agent.
     */
    public int getNumberOfTimeSeries() {
        return mappings.size();
    }

    /**
     * Setter for the time series client.
     * @param tsClient The time series client to use.
     */
    public void setTsClient(TimeSeriesClient<OffsetDateTime> tsClient) {
        this.tsClient = tsClient;
    }

    /**
     * Reads the JSON key to IRI mappings from files in the provided folder.
     * @param mappingFolder The path to the folder in which the mapping files are located.
     */
    private void readMappings(String mappingFolder) throws IOException {
        mappings = new ArrayList<>();
        File folder = new File(mappingFolder);
        File[] mappingFiles = folder.listFiles();
        // Make sure the folder exists and contains files
        if (mappingFiles == null) {
            throw new IOException("Folder does not exist: " + mappingFolder);
        }
        if (mappingFiles.length == 0) {
            throw new IOException("No files in the folder: " + mappingFolder);
        }
        // Create a mapper for each file
        else {
            for (File mappingFile: mappingFiles) {
                JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(ThingsBoardInputAgent.generatedIRIPrefix, mappingFile.getAbsolutePath());
                mappings.add(mapper);
                // Save the mappings back to the file to ensure using same IRIs next time
                mapper.saveToFile(mappingFile.getAbsolutePath());
            }
        }
    }

    /**
     * Initializes all time series maintained by the agent (represented by the key to IRI mappings) if they do no exist
     * using the time series client.
     */
    public void initializeTimeSeriesIfNotExist() {
        // Iterate through all mappings (each represents one time series)
        for (JSONKeyToIRIMapper mapping: mappings) {
            // The IRIs used by the current mapping
            List<String> iris = mapping.getAllIRIs();
            // Check whether IRIs have a time series linked and if not initialize the corresponding time series
            if(!timeSeriesExist(iris)) {
                // Get the classes (datatype) corresponding to each JSON key needed for initialization
                List<Class<?>> classes = iris.stream().map(this::getClassFromJSONKey).collect(Collectors.toList());
                // Initialize the time series
                try {
                tsClient.initTimeSeries(iris, classes, timeUnit, Type.INSTANTANEOUS, null, null);
                LOGGER.info(String.format("Initialized time series with the following IRIs: %s", String.join(", ", iris)));
            } catch (Exception e) {
            	throw new JPSRuntimeException("Could not initialize timeseries!");
            }
                }
        }
    }

    /**
     * Checks whether a time series exists by checking whether any of the IRIs
     * that should be attached to the time series has no attachment using the time series client.
     * @param iris The IRIs that should be attached to the same time series provided as list of strings.
     * @return True if all IRIs have a time series attached, false otherwise.
     */
    private boolean timeSeriesExist(List<String> iris) {
        // If any of the IRIs does not have a time series the time series does not exist
        for(String iri: iris) {
        	try {
	            if (!tsClient.checkDataHasTimeSeries(iri)) {
	                return false;
	            }
	        // If central RDB lookup table ("dbTable") has not been initialised, the time series does not exist
        	} catch (DataAccessException e) {
        		if (e.getMessage().contains("ERROR: relation \"dbTable\" does not exist")) {
        			return false;
        		}
        		else {
        			throw e;
        		}        		
        	} 
        }
        return true;
    }

    /**
     * Updates the database with new readings.
     * @param ElectricalTemperatureHumidityReadings The readings received from the ThingsBoard API
     */
    public void updateData(JSONObject ElectricalTemperatureHumidityReadings)throws IllegalArgumentException {
        // Transform readings in hashmap containing a list of objects for each JSON key,
        // will be empty if the JSON Object is empty
    	Map<String, List<?>> timeStampReadingsMap = jsonObjectToMapForTimeStamp(ElectricalTemperatureHumidityReadings);
    	Map<String, List<?>> electricalTemperatureHumidityReadingsMap = jsonObjectToMap(ElectricalTemperatureHumidityReadings);
        
        
        // Only do something if all readings contain data
        if(!electricalTemperatureHumidityReadingsMap.isEmpty() && !timeStampReadingsMap.isEmpty()) {
            List<TimeSeries<OffsetDateTime>> timeSeries;
            try {
                timeSeries = convertReadingsToTimeSeries(electricalTemperatureHumidityReadingsMap, timeStampReadingsMap);
            }
            // Is a problem as time series objects must be the same every time to ensure proper insert into the database
            catch (NoSuchElementException e) {
                throw new IllegalArgumentException("Readings can not be converted to proper time series!", e);
            }
            // Update each time series
            for (TimeSeries<OffsetDateTime> ts : timeSeries) {
                // Retrieve current maximum time to avoid duplicate entries (can be null if no data is in the database yet)
                OffsetDateTime endDataTime;
                try{
                	endDataTime = tsClient.getMaxTime(ts.getDataIRIs().get(0));
                } catch (Exception e) {
                	throw new JPSRuntimeException("Could not get max time!");
                }
                OffsetDateTime startCurrentTime = ts.getTimes().get(0);
                // If there is already a maximum time
                if (endDataTime != null) {
                    // If the new data overlaps with existing timestamps, prune the new ones
                    if (startCurrentTime.isBefore(endDataTime)) {
                        ts = pruneTimeSeries(ts, endDataTime);
                    }
                }
                // Only update if there actually is data
                if (!ts.getTimes().isEmpty()) {
                	try {
                    tsClient.addTimeSeriesData(ts);
                    LOGGER.debug(String.format("Time series updated for following IRIs: %s", String.join(", ", ts.getDataIRIs())));
                } catch (Exception e) {
                	throw new JPSRuntimeException("Could not add timeseries data!");
                }
                }
                
            }
        }
        // Is a problem as time series objects must be the same every time to ensure proper insert into the database
        else {
            throw new IllegalArgumentException("Readings can not be empty!");
        	}
    }

    /**
     * Transform a JSON Object into a Map, where values per key are gathered into a list. For this method, the key is "ts" and the values are timestamps.
     * @param readings The JSON Object to convert
     * @return The timestamps(in date time format) in form of a Map
     * @throws JSONException if some keys are not found or the value is not a JSONArray
     */
    public Map<String, List<?>> jsonObjectToMapForTimeStamp(JSONObject readings) {
        // First save the values as Object //
    	//readingsMap is a map with keys as String and each key is mapped to a list of objects.
        Map<String, List<Object>> readingsMapTimeStamp = new HashMap<>();
        String[] keys = new String[readings.length()];
        JSONArray tsAndValue = new JSONArray();
        int a = 0;
        for (Iterator<String> it = readings.keys(); it.hasNext();) {     
        	String key = it.next(); 	
        	keys[a] = key;
                tsAndValue = readings.getJSONArray(keys[a]);  
        	a++;
        }
                //Go through the JSON objects in the array one by one
                //JSON objects are arranged with latest ts and value at the top
                for (int j = tsAndValue.length() - 1; j >= 0; j--) {
                // Get the timestamp and add it to the corresponding list
                	JSONObject timeSeriesEntry = tsAndValue.getJSONObject(j);
                	long timestamp = timeSeriesEntry.getLong("ts");
                	//convert unix timestamp in milliseconds to date time format
                	
                	Date date = new java.util.Date(timestamp);
                	SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
                	sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
                	Object ts = sdf.format(date);
                	
                	
                // Handle cases where the API returned null
                if (ts == JSONObject.NULL) {
                    // Handling depends on the datatype of the current key
                    String datatype = getClassFromJSONKey(timestampKey).getSimpleName();
                    // If it is a number use NaN (not a number)
                    if (datatype.equals(Integer.class.getSimpleName()) | datatype.equals(Double.class.getSimpleName())) {
                        ts = Double.NaN;
                    }
                    // Otherwise, use the string NA (not available)
                    else {
                        ts = "NA";
                    }
                }
                // If the key is not present yet initialize the list
                if (!readingsMapTimeStamp.containsKey("ts")) {
                    readingsMapTimeStamp.put("ts", new ArrayList<>());
                }
                readingsMapTimeStamp.get("ts").add(ts);
                    
        }

        // Convert the values to the proper datatype //
        Map<String, List<?>> readingsMapTyped = new HashMap<>();
        for (String key1: readingsMapTimeStamp.keySet()) {
            // Get the class (datatype) corresponding to the key
            String datatype = getClassFromJSONKey(key1).getSimpleName();
            // Get current list with object type
            List<Object> valuesUntyped = readingsMapTimeStamp.get(key1);
            List<?> valuesTyped;
            // Use mapping to cast the values into integer, double, boolean or string
            // The Number cast is required for org.json datatypes
            if (datatype.equals(Double.class.getSimpleName())) {
                valuesTyped = valuesUntyped.stream().map(value -> ((Number) value).doubleValue()).collect(Collectors.toList());
            }
            else {
                valuesTyped = valuesUntyped.stream().map(Object::toString).collect(Collectors.toList());
            }
            readingsMapTyped.put(key1, valuesTyped);
        }
        return readingsMapTyped;
    }
    
    /**
     * Transform a JSON Object into a Map, where values per key are gathered into a list.
     * The JSON Object has key-value pairs where the keys are Current, Voltage, Power etc and the values are JSON Arrays.
     * Each JSON Array consist of multiple JSON Objects. Each JSON Objects consist of two key-value pairs with the keys being ts and value.
     * @param readings The JSON Object to convert
     * @return The readings in form of a Map
     */
    private Map<String, List<?>> jsonObjectToMap(JSONObject readings) {
        // First save the values as Object //
    	//readingsMap is a map with keys as String and each key is mapped to a list of objects.
        Map<String, List<Object>> readingsMap = new HashMap<>();
        // Go through the readings in the JSON object one by one
            // Iterate through the keys of the JSON object
            for (Iterator<String> it = readings.keys(); it.hasNext();) {
                String key = it.next();                
                //Each key such as Current, Voltage etc has JSON arrays as its own value consisting
                //of multiple JSON objects {"ts"= ...,"value" = ... } separated by a comma
                JSONArray tsAndValue = readings.getJSONArray(key);
                //Go through the JSON objects in the array one by one
                for (int j = tsAndValue.length() - 1; j >= 0; j--) {
                // Get the value and add it to the corresponding list
                	JSONObject timeSeriesEntry = tsAndValue.getJSONObject(j);
                	Object value = timeSeriesEntry.get("value");
                //The values are of string type in the JSON Object, convert them to double
                	try {
                    	value = Double.valueOf(timeSeriesEntry.get("value").toString());
                    	}
                	catch (NumberFormatException e) {
                		value = Double.NaN;
                	}
                	catch (NullPointerException e) {
                		value = "NA";
                	}
                // If the key is not present yet initialize the list
                if (!readingsMap.containsKey(key)) {
                    readingsMap.put(key, new ArrayList<>());
                }
                readingsMap.get(key).add(value);
            }
            }

        // Convert the values to the proper datatype //
        Map<String, List<?>> readingsMapTyped = new HashMap<>();
        for (String key: readingsMap.keySet()) {
            // Get the class (datatype) corresponding to the key
            String datatype = getClassFromJSONKey(key).getSimpleName();
            // Get current list with object type
            List<Object> valuesUntyped = readingsMap.get(key);
            List<?> valuesTyped;
            // Use mapping to cast the values into integer, double, boolean or string
            // The Number cast is required for org.json datatypes
            if (datatype.equals(Double.class.getSimpleName())) {
                valuesTyped = valuesUntyped.stream().map(value -> ((Number) value).doubleValue()).collect(Collectors.toList());
            }
            else {
                valuesTyped = valuesUntyped.stream().map(Object::toString).collect(Collectors.toList());
            }
            readingsMapTyped.put(key, valuesTyped);
        }
        return readingsMapTyped;
    }

    /**
     * Converts the readings in form of maps to time series' using the mappings from JSON key to IRI.
     * @param ElectricalTemperatureHumidityReadings The readings as map.
     * @param TimestampReadings The timestamps as map.
     * @return A list of time series objects (one per mapping) that can be used with the time series client.
     */
    private List<TimeSeries<OffsetDateTime>> convertReadingsToTimeSeries(Map<String, List<?>> ElectricalTemperatureHumidityReadings,
                                                                        Map<String, List<?>> TimestampReadings
                                                                        )
            throws  NoSuchElementException {
        // Extract the timestamps by mapping the private conversion method on the list items
        // that are supposed to be string (toString() is necessary as the map contains lists of different types)
        List<OffsetDateTime> allTimestamps = TimestampReadings.get(ThingsBoardInputAgent.timestampKey).stream()
                .map(timestamp -> (convertStringToOffsetDateTime(timestamp.toString()))).collect(Collectors.toList());
        // Construct a time series object for each mapping
        List<TimeSeries<OffsetDateTime>> timeSeries = new ArrayList<>();
        for (JSONKeyToIRIMapper mapping: mappings) {
            // Initialize the list of IRIs
            List<String> iris = new ArrayList<>();
            // Initialize the list of list of values
            List<List<?>> values = new ArrayList<>();
            // Go through all keys in the mapping
            for(String key: mapping.getAllJSONKeys()) {
                // Add IRI
                iris.add(mapping.getIRI(key));
                if (ElectricalTemperatureHumidityReadings.containsKey(key)) {
                    values.add(ElectricalTemperatureHumidityReadings.get(key));
                }
                // Will create a problem as length of iris and values do not match when creating the time series.
                // Could add an empty list, but the length of the list needs to match length of times. So what values to
                // fill it with?
                else {
                    throw new NoSuchElementException("The key " + key + " is not contained in the readings!");
                }
            }
            List<OffsetDateTime> times = allTimestamps;
            // Create the time series object and add it to the list
            TimeSeries<OffsetDateTime> currentTimeSeries = new TimeSeries<>(times, iris, values);
            timeSeries.add(currentTimeSeries);
        }

        return timeSeries;
    }

    /**
     * Converts a string into a datetime object with zone information using the zone globally define for the agent.
     * @param timestamp The timestamp as string, the format should be equal to 2007-12-03T10:15:30.
     * @return The resulting datetime object.
     */
    private OffsetDateTime convertStringToOffsetDateTime(String timestamp) {
        // Convert first to a local time
        LocalDateTime localTime = LocalDateTime.parse(timestamp);
        // Then add the zone id
        return OffsetDateTime.of(localTime, ThingsBoardInputAgent.ZONE_OFFSET);
    }

    /**
     * Prunes a times series so that all timestamps and corresponding values start after the threshold.
     * @param timeSeries The times series tp prune
     * @param timeThreshold The threshold before which no data should occur
     * @return The resulting datetime object.
     */
    private TimeSeries<OffsetDateTime> pruneTimeSeries(TimeSeries<OffsetDateTime> timeSeries, OffsetDateTime timeThreshold) {
        // Find the index from which to start
        List<OffsetDateTime> times = timeSeries.getTimes();
        int index = 0;
        while(index < times.size()) {
            if (times.get(index).isAfter(timeThreshold)) {
                break;
            }
            index++;
        }
        // Prune timestamps
        List<OffsetDateTime> newTimes = new ArrayList<>();
        // There are timestamps above the threshold
        if (index != times.size()) {
            // Prune the times
            newTimes = new ArrayList<>(times.subList(index, times.size()));
        }
        // Prune data
        List<List<?>> newValues = new ArrayList<>();
        // Prune the values
        for (String iri: timeSeries.getDataIRIs()) {
            // There are timestamps above the threshold
            if (index != times.size()) {
                newValues.add(timeSeries.getValues(iri).subList(index, times.size()));
            }
            else {
                newValues.add(new ArrayList<>());
            }
        }
        LOGGER.info("The timeseries will be added after the threshold " + timeThreshold);
        return new TimeSeries<>(newTimes, timeSeries.getDataIRIs(), newValues);
    }

    /**
     * Returns the class (datatype) corresponding to a JSON key.
     * @param jsonKey The JSON key as string.
     * @return The corresponding class as Class<?> object.
     */
    private Class<?> getClassFromJSONKey(String jsonKey) {
        if (jsonKey.contains(timestampKey))
        	//The timestamp has been converted from unix timestamp in milliseconds to date time format
        	//As such it should be a String
        	return String.class;
        else {
        	//Default is Double since ThingsBoard main purpose is to store measurements from sensors
        	//As such there should not be any readings stored in ThingsBoard that is considered to be boolean, String or integer
        	return Double.class;
        }
    }
}

