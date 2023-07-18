package uk.ac.cam.cares.jps.agent.caresWeatherStation;

import org.json.JSONArray;
import org.json.JSONObject;
import org.jooq.exception.DataAccessException;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;


import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Class to retrieve data from the weather station API and storing it with connection to The World Avatar (Knowledge Base).
 * @author  GMMajal*/
public class CARESWeatherStationInputAgent {

    /**
     * Logger for reporting info/errors.
     */

    private static final Logger LOGGER = LogManager.getLogger(CARESWeatherStationInputAgentLauncher.class);

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
    public static final String generatedIRIPrefix = TimeSeriesSparql.TIMESERIES_NAMESPACE + "caresWeatherStation";
    /**
     * The time unit used for all time series maintained by the caresWeatherStation input agent
     */
    public static final String timeUnit = OffsetDateTime.class.getSimpleName();
    /**
     * The JSON key for the timestamp
     */
    public static final String timestampKey = "obsTimeUtc";


    //JSON key denoting whether the observations obtained from the weather station passed quality control checks
    public static final String status= "qcStatus";
    /**
     * The Zone offset of the timestamp (https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/ZoneOffset.html)
     */
    public static final ZoneOffset ZONE_OFFSET = ZoneOffset.UTC;

    /**
     * Standard constructor which reads in JSON key to IRI mappings from the config folder
     * defined in the provided properties file.
     * @param propertiesFile The properties file from which to read the path of the mapping folder.
     */
    public CARESWeatherStationInputAgent(String propertiesFile) throws IOException {
        // Set the mapping between JSON keys and IRIs
        try (InputStream input = new FileInputStream(propertiesFile)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            String mappingFolder;
            // Read the mappings folder from the properties file
            try {
                // Read the mappings folder from the properties file
                mappingFolder = System.getenv(prop.getProperty("caresWeatherStation.mappingfolder"));
            }
            catch (NullPointerException e) {
                throw new IOException ("The key caresWeatherStation.mappingfolder cannot be found in the properties file.");
            }
            if (mappingFolder == null) {
                throw new InvalidPropertiesFormatException("The properties file does not contain the key caresWeatherStation.mappingfolder " +
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
                JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(CARESWeatherStationInputAgent.generatedIRIPrefix, mappingFile.getAbsolutePath());
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
     * Checks whether a time series exists by checking whether any of the IRIs that should be attached to
     * the time series is not initialised in the central RDB lookup table using the time series client.
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
     * @param weatherReadings The readings received from the weather station API
     */

        public void updateData(JSONObject weatherReadings) throws IllegalArgumentException {
        // Transform readings in hashmap containing a list of objects for each JSON key,
        // will be empty if the JSON Array is empty
            Map<String, List<?>> weatherReadingsMap = new HashMap<>();
        try{
            weatherReadingsMap = jsonObjectToMap(weatherReadings);
        }
        catch(Exception e){
            throw new JPSRuntimeException("Readings can not be empty!",e);
        }

        // Only do something if readings contain data
        if(!weatherReadingsMap.isEmpty()) {
            List<TimeSeries<OffsetDateTime>> timeSeries;
            try {
                timeSeries = convertReadingsToTimeSeries(weatherReadingsMap);
            }
            // Is a problem as time series objects must be the same every time to ensure proper insert into the database
            catch (NoSuchElementException e) {
                throw new IllegalArgumentException("Readings can not be converted to proper time series!", e);
            }
            // Update each time series
            for (TimeSeries<OffsetDateTime> ts : timeSeries) {
                // Retrieve current maximum time to avoid duplicate entries (can be null if no data is in the database yet)
                OffsetDateTime endDataTime;
                try {
                	endDataTime= tsClient.getMaxTime(ts.getDataIRIs().get(0));
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
                	throw new JPSRuntimeException("Could not add timeseries!");
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
     * Transform a JSON Object into a Map, where values per key are gathered into a list.
     * The JSON Object has key-value pairs.
     * The JSON Object takes the form {"observations":[{<key>:<value>},...,"metric_si":{{<key>:<value>},...{<key>:<value>}},...]}
     * @param readings The JSON Object to convert
     * @return The readings in form of a Map
     */

    private Map<String, List<?>> jsonObjectToMap(JSONObject readings) {

        // First save the values as Object //
        Map<String, List<Object>> readingsMap = new HashMap<>();
        JSONArray jsArr;
        try {
            jsArr = readings.getJSONArray("observations");
            for(int i=0; i<jsArr.length();i++) {
                JSONObject currentEntry = jsArr.getJSONObject(i);
                Iterator<String> it = currentEntry.keys();
                while(it.hasNext()) {
                    String key = it.next();
                    Object value = currentEntry.get(key);
                    if (value.getClass() != JSONObject.class) {
                        // Get the value and add it to the corresponding list
                        // Handle cases where the API returned null
                        if (value == JSONObject.NULL) {
                            // Handling depends on the datatype of the current key
                            String datatype = getClassFromJSONKey(key).getSimpleName();
                            // If it is a number use NaN (not a number)
                            if (datatype.equals(Integer.class.getSimpleName()) | datatype.equals(Double.class.getSimpleName()) | datatype.equals(Long.class.getSimpleName())) {
                                value = Double.NaN;
                            }
                            // Otherwise, use the string NA (not available)
                            else {
                                value = "NA";
                            }
                        }
                        // If the key is not present yet initialize the list
                        if (!readingsMap.containsKey(key)) {
                            readingsMap.put(key, new ArrayList<>());
                        }
                        if (key=="metric_si")
                            LOGGER.info(String.format("Reading %s key now", key));
                        readingsMap.get(key).add(value);
                    } else {
                        JSONObject obj = currentEntry.getJSONObject("metric_si");
                        for (Iterator<String> it1 = obj.keys(); it1.hasNext(); ) {
                            String key1 = it1.next();
                            Object value1 = obj.get(key1);
                            // Handle cases where the API returned null
                            if (value1 == JSONObject.NULL) {
                                value1 = Double.NaN;
                            }
                            // If the key is not present yet initialize the list
                            if (!readingsMap.containsKey(key1)) {
                                readingsMap.put(key1, new ArrayList<>());
                            }
                            readingsMap.get(key1).add(value1);
                        }
                    }
                }
            }
        } catch (Exception e) {
            throw new JPSRuntimeException("Readings can not be empty!", e);
        }

        // Convert the values to the proper datatype //
        Map<String, List<?>> readingsMapTyped = new HashMap<>();
        for (String key : readingsMap.keySet()) {
            // Get the class (datatype) corresponding to the key
            String datatype = getClassFromJSONKey(key).getSimpleName();
            // Get current list with object type
            List<Object> valuesUntyped = readingsMap.get(key);
            List<?> valuesTyped;
            // Use mapping to cast the values into integer, double, long or string
            // The Number cast is required for org.json datatypes
            if (datatype.equals(Integer.class.getSimpleName())) {
                valuesTyped = valuesUntyped.stream().map(x -> ((Number) x).intValue()).collect(Collectors.toList());
            } else if (datatype.equals(Double.class.getSimpleName())) {
                valuesTyped = valuesUntyped.stream().map(x -> ((Number) x).doubleValue()).collect(Collectors.toList());
            } else if (datatype.equals(Long.class.getSimpleName())) {
                valuesTyped = valuesUntyped.stream().map(x -> ((Number) x).longValue()).collect(Collectors.toList());
            } else {
                valuesTyped = valuesUntyped.stream().map(Object::toString).collect(Collectors.toList());
            }
            readingsMapTyped.put(key, valuesTyped);
        }
        return readingsMapTyped;

    }


    /**
     * Converts the readings in form of maps to time series' using the mappings from JSON key to IRI.
     * @param weatherReadings The weather readings as map.
     * @return A list of time series objects (one per mapping) that can be used with the time series client.
     */
     private List<TimeSeries<OffsetDateTime>> convertReadingsToTimeSeries(Map<String, List<?>> weatherReadings)
          throws  NoSuchElementException {
        // Extract the timestamps by mapping the private conversion method on the list items
        // that are supposed to be string (toString() is necessary as the map contains lists of different types)

        List<OffsetDateTime> weatherTimestamps = weatherReadings.get(CARESWeatherStationInputAgent.timestampKey).stream()
                .map(timestamp -> (convertStringToOffsetDateTime(timestamp.toString()))).collect(Collectors.toList());

        // Construct a time series object for each mapping
        List<TimeSeries<OffsetDateTime>> timeSeries = new ArrayList<>();
        for (JSONKeyToIRIMapper mapping: mappings) {
            // Initialize the list of IRIs
            List<String> iris = new ArrayList<>();
            // Initialize the list of list of values
            List<List<?>> values = new ArrayList<>();
            for(String key: mapping.getAllJSONKeys()) {
                // Add IRI
                iris.add(mapping.getIRI(key));
                if (weatherReadings.containsKey(key)) {
                    values.add(weatherReadings.get(key));
                }
                else {
                    throw new NoSuchElementException("The key " + key + " is not contained in the readings!");
                }
            }

            List<OffsetDateTime> times = weatherTimestamps;
            // Create the time series object and add it to the list

            TimeSeries<OffsetDateTime> currentTimeSeries = new TimeSeries<>(times, iris, values);
            timeSeries.add(currentTimeSeries);
        }

        return timeSeries;
    }



    /**
     * Converts a string into a datetime object with zone information using the zone globally define for the agent.
     * @param timestamp The timestamp as string, the format should be equal to 2007-12-03T10:15:30Z.
     * @return The resulting datetime object.
     */
    private OffsetDateTime convertStringToOffsetDateTime(String timestamp)  {

        timestamp=timestamp.replace("Z","");

        DateTimeFormatter dtf=DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss");
        LocalDateTime localTime=LocalDateTime.parse(timestamp,dtf);


        // Then add the zone id
        return OffsetDateTime.of(localTime, CARESWeatherStationInputAgent.ZONE_OFFSET);
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
        return new TimeSeries<>(newTimes, timeSeries.getDataIRIs(), newValues);
    }

    /**
     * Returns the class (datatype) corresponding to a JSON key. Note: rules for the mapping are hardcoded in the method.
     * @param jsonKey The JSON key as string.
     * @return The corresponding class as Class<?> object.
     */
    private Class<?> getClassFromJSONKey(String jsonKey) {
        if (jsonKey.contains(timestampKey) || jsonKey.contains("stationID") || jsonKey.contains("obsTimeLocal") || jsonKey.contains("tz")){
            return String.class;
        }
        else if( jsonKey.contains(status) || jsonKey.contains("winddirAvg")){
            return Integer.class;
        }
        else if(jsonKey=="epoch"){
            return Long.class;
        }
        else{
            return Double.class;
        }
    }

}