package uk.ac.cam.cares.jps.agent.aqmesh;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.agent.utils.JSONKeyToIRIMapper;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Class to retrieve data from the AQMesh API and storing it with connection to The World Avatar (Knowledge Base).
 * @author Niklas Kasenburg
 */
public class AQMeshInputAgent {

    // The time series client to interact with the knowledge graph and data storage
    private TimeSeriesClient<ZonedDateTime> tsClient;
    // A list of mappings between JSON keys and the corresponding IRI, contains one mapping per time series
    private List<JSONKeyToIRIMapper> mappings;
    // The prefix to use when no IRI exists for a JSON key originally
    public static final String generatedIRIPrefix = TimeSeriesSparql.ns_kb + "aqmesh";
    // The time unit used for all time series maintained by the AQMesh input agent
    public static final String timeUnit = ZonedDateTime.class.getSimpleName();
    // The JSON key for the timestamp
    public static final String timestampKey = "reading_datestamp";
    // The Zone ID of the timestamp (see https://docs.oracle.com/javase/8/docs/api/java/time/ZoneId.html)
    public static final ZoneId zoneID = ZoneId.of("UTC+00:00");

    /**
     * Standard constructor which reads in JSON key to IRI mappings from the config folder
     * defined in the provided properties file.
     * @param propertiesFile The properties file from which to read the path of the mapping folder.
     */
    public AQMeshInputAgent(String propertiesFile) throws IOException {
        // Set the mapping between JSON keys and IRIs
        try (InputStream input = new FileInputStream(propertiesFile)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            // Read the mappings folder from the properties file
            String mappingFolder = prop.getProperty("aqmesh.mappingfolder");
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
    public void setTsClient(TimeSeriesClient<ZonedDateTime> tsClient) {
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
                JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(AQMeshInputAgent.generatedIRIPrefix, mappingFile.getAbsolutePath());
                mappings.add(mapper);
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
                tsClient.initTimeSeries(iris, classes, timeUnit);
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
            if (!tsClient.checkDataHasTimeSeries(iri)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Updates the database with possible new data requested from the API.
     * @param connector The connector to communicate with the AQMesh API
     */
    public void updateDate(AQMeshAPIConnector connector) {
        // Retrieve readings from the connector
        JSONArray particleReadings = connector.getParticleReadings();
        JSONArray gasReadings = connector.getGasReadings();
        // Transform readings in hashmap containing a list of objects for each JSON key
        Map<String, List<?>> particleReadingsMap = new HashMap<>();
        Map<String, List<?>> gasReadingsMap = new HashMap<>();
        if (particleReadings.length() > 0) {
            particleReadingsMap = jsonArrayToMap(particleReadings);
        }
        if (gasReadings.length() > 0) {
            gasReadingsMap = jsonArrayToMap(gasReadings);
        }
        List<TimeSeries<ZonedDateTime>> timeSeries = convertReadingsToTimeSeries(particleReadingsMap, gasReadingsMap);
        // Update each time series
        for (TimeSeries<ZonedDateTime> ts: timeSeries) {
            // Retrieve current maximum time to avoid duplicate entries
            ZonedDateTime maxDataTime = tsClient.getMaxTime(ts.getDataIRIs().get(0));
            ZonedDateTime minCurrentTime = ts.getTimes().get(ts.getTimes().size()-1);
            // If the new data overlaps with existing timestamps, prune the new ones
            if (minCurrentTime.isBefore(maxDataTime)) {
                ts = pruneTimeSeries(ts, maxDataTime);
            }
            // Only update if there actually is data
            if (!ts.getTimes().isEmpty()) {
                tsClient.addTimeSeriesData(ts);
            }
        }
    }

    /**
     * Transform a JSON Array where each element is a single timestamp with all readings
     * into a Map, where values per key are gathered into a list.
     * @param readings The JSON Array to convert
     * @return The same readings in form of a Map
     */
    private Map<String, List<?>> jsonArrayToMap(JSONArray readings) {
        // First save the values as Object //
        Map<String, List<Object>> readingsMap = new HashMap<>();
        // Go through the readings in the array one by one
        for (int i = 0; i < readings.length(); i++) {
            JSONObject currentEntry = readings.getJSONObject(i);
            // Iterate through the keys of the entry
            for (Iterator<String> it = currentEntry.keys(); it.hasNext();) {
                String key = it.next();
                // Get the value and add it to the corresponding list
                Object value = currentEntry.get(key);
                // Handle cases where the API returned null
                if (value == JSONObject.NULL) {
                    // Handling depends on the datatype of the current key
                    String datatype = getClassFromJSONKey(key).getSimpleName();
                    // If it is a number use NaN (not a number)
                    if (datatype.equals(Integer.class.getSimpleName()) | datatype.equals(Double.class.getSimpleName())) {
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
            if (datatype.equals(Integer.class.getSimpleName())) {
                valuesTyped = valuesUntyped.stream().map(value -> ((Number) value).intValue()).collect(Collectors.toList());
            }
            else if (datatype.equals(Double.class.getSimpleName())) {
                valuesTyped = valuesUntyped.stream().map(value -> ((Number) value).doubleValue()).collect(Collectors.toList());
            }
            else if (datatype.equals(Boolean.class.getSimpleName())) {
                valuesTyped = valuesUntyped.stream().map(value -> ((Boolean) value)).collect(Collectors.toList());
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
     * @param particleReadings The particle readings as map.
     * @param gasReadings The gas readings as map.
     * @return A list of time series object (one per mapping) that can be used with the time series client.
     */
    private List<TimeSeries<ZonedDateTime>> convertReadingsToTimeSeries(Map<String, List<?>> particleReadings,
                                                                        Map<String, List<?>> gasReadings)
            throws  NoSuchElementException {
        // Extract the timestamps by mapping the convertStringToZonedDateTime on the list items
        // that are supposed to be string (toString() is necessary as the map contains lists of different types)
        List<ZonedDateTime> particleTimestamps = particleReadings.get(AQMeshInputAgent.timestampKey).stream()
                .map(timestamp -> (convertStringToZonedDateTime(timestamp.toString()))).collect(Collectors.toList());
        List<ZonedDateTime> gasTimestamps = gasReadings.get(AQMeshInputAgent.timestampKey).stream()
                .map(timestamp -> (convertStringToZonedDateTime(timestamp.toString()))).collect(Collectors.toList());
        // Construct a time series object for each mapping
        List<TimeSeries<ZonedDateTime>> timeSeries = new ArrayList<>();
        for (JSONKeyToIRIMapper mapping: mappings) {
            // Initialize the list of IRIs
            List<String> iris = new ArrayList<>();
            // Initialize the list of list of values
            List<List<?>> values = new ArrayList<>();
            // Go through all keys in the mapping
            boolean useParticleReadings = true;
            for(String key: mapping.getAllJSONKeys()) {
                // Add IRI
                iris.add(mapping.getIRI(key));
                // Always try the particle readings first (all general information are contained there)
                if (particleReadings.containsKey(key)) {
                    values.add(particleReadings.get(key));
                    useParticleReadings = true;
                }
                else if (gasReadings.containsKey(key)) {
                    values.add(gasReadings.get(key));
                    useParticleReadings = false;
                }
                else {
                    throw new NoSuchElementException("The key " + key + " is not contained in the readings!");
                }
            }
            // Timestamps depend on which readings are used for the mapping
            List<ZonedDateTime> times = (useParticleReadings) ? particleTimestamps : gasTimestamps;
            // Creat the time series object and add it to the list
            TimeSeries<ZonedDateTime> currentTimeSeries = new TimeSeries<>(times, iris, values);
            timeSeries.add(currentTimeSeries);
        }

        return timeSeries;
    }

    /**
     * Converts a string into a datetime object with zone information using the zone globally define for the agent.
     * @param timestamp The timestamp as string, the format should be equal to 2007-12-03T10:15:30.
     * @return The resulting datetime object.
     */
    private ZonedDateTime convertStringToZonedDateTime(String timestamp) {
        // Convert first to a local time
        LocalDateTime localTime = LocalDateTime.parse(timestamp);
        // Then add the zone id
        return ZonedDateTime.of(localTime, AQMeshInputAgent.zoneID);
    }

    /**
     * Prunes a times series so that all timestamps and corresponding values start after the threshold.
     * @param timeSeries The times series tp prune
     * @param timeThreshold The treshold before which no data should occur
     * @return The resulting datetime object.
     */
    private TimeSeries<ZonedDateTime> pruneTimeSeries(TimeSeries<ZonedDateTime> timeSeries, ZonedDateTime timeThreshold) {
        // Find the index from which to start
        List<ZonedDateTime> times = timeSeries.getTimes();
        int index = 0;
        while(index < times.size()) {
            if (times.get(index).isAfter(timeThreshold)) {
                break;
            }
            index++;
        }
        List<ZonedDateTime> newTimes = new ArrayList<>();
        List<List<?>> newValues = new ArrayList<>();
        // There are timestamps above the threshold
        if (index != times.size()) {
            // Prune the times
            newTimes = new ArrayList<>(times.subList(index, times.size()));
            // Prune the values
            for (String iri: timeSeries.getDataIRIs()) {
                newValues.add(timeSeries.getValues(iri).subList(index, times.size()));
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
        // JSON keys for reading and sending intervals end in _p1, _p2 or _p3
        Pattern intervalPattern = Pattern.compile(".*_p[123]$");
        // Intervals are integers representing the seconds
        if (intervalPattern.matcher(jsonKey).matches()) {
            return Integer.class;
        }
        // Battery voltage is a floating point
        else if (jsonKey.contains("_voltage")) {
            return Double.class;
        }
        // Environment conditions are floating point measures
        else if (jsonKey.contains("temperature") | jsonKey.contains("pressure") | jsonKey.contains("humidity")) {
            return Double.class;
        }
        // Noise information for gas readings are floating point numbers
        else if (jsonKey.contains("noise")) {
            return Double.class;
        }
        // Sensor readings and corresponding offset and slope are floating point numbers
        else if (jsonKey.contains("prescale") | jsonKey.contains("slope") | jsonKey.contains("offset")) {
            return Double.class;
        }
        // Battery low warning and particle modem overlap are boolean
        else if (jsonKey.equals("battery_low") | jsonKey.equals("particle_modem_overlap")) {
            return Boolean.class;
        }
        // The default datatype is string
        else {
            return String.class;
        }
    }

}
