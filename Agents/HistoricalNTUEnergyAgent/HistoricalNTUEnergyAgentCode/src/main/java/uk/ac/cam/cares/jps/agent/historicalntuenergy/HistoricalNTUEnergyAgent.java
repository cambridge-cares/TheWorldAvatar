package uk.ac.cam.cares.jps.agent.historicalntuenergy;

import org.json.JSONArray;
import org.json.JSONObject;
import org.jooq.exception.DataAccessException;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient.Type;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.*;
import java.util.*;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


public class HistoricalNTUEnergyAgent {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(HistoricalNTUEnergyAgentLauncher.class);

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
    public static final String OmPrefix = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String generatedIRIPrefix = OmPrefix + "measure";
    /**
     * The time unit used for all time series maintained by the agent
     */
    public static final String timeUnit = OffsetDateTime.class.getSimpleName();
    /**
     * The JSON key for the timestamp
     */
    public static final String timestampKey = "TIME";
    /**
     * The Zone offset of the timestamp (https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/ZoneOffset.html)
     */
    public static final ZoneOffset ZONE_OFFSET = ZoneOffset.UTC;

    /**
     * Standard constructor which reads in JSON key to IRI mappings from the config folder
     * defined in the provided properties file.
     *
     * @param propertiesFile The properties file from which to read the path of the mapping folder.
     */
    public HistoricalNTUEnergyAgent(String propertiesFile) throws IOException {
        // Set the mapping between JSON keys and IRIs
        try (InputStream input = new FileInputStream(propertiesFile)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            String mappingFolder;
            // Read the mappings folder from the properties file
            try {
                // Read the mappings folder from the properties file
                mappingFolder = System.getenv(prop.getProperty("ntuenergy.mappingfolder"));
            } catch (NullPointerException e) {
                throw new IOException("The key ntuenergy.mappingfolder cannot be found in the properties file.");
            }
            if (mappingFolder == null) {
                throw new InvalidPropertiesFormatException("The properties file does not contain the key ntuenergy.mappingfolder " +
                        "with a path to the folder containing the required JSON key to IRI mappings.");
            }
            // Read the JSON key to IRI mappings from the mapping folder
            readMappings(mappingFolder);
        }

    }

    /**
     * Retrieves the number of time series the input agent is handling.
     *
     * @return The number of time series maintained by the agent.
     */
    public int getNumberOfTimeSeries() {
        return mappings.size();
    }

    /**
     * Setter for the time series client.
     *
     * @param tsClient The time series client to use.
     */
    public void setTsClient(TimeSeriesClient<OffsetDateTime> tsClient) {
        this.tsClient = tsClient;
    }

    /**
     * Reads the JSON key to IRI mappings from files in the provided folder.
     *
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
            for (File mappingFile : mappingFiles) {
                JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(HistoricalNTUEnergyAgent.generatedIRIPrefix, mappingFile.getAbsolutePath());
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
        for (JSONKeyToIRIMapper mapping : mappings) {
            // The IRIs used by the current mapping
            List<String> iris = mapping.getAllIRIs();
            // Check whether IRIs have a time series linked and if not initialize the corresponding time series
            if (!timeSeriesExist(iris)) {
                // Get the classes (datatype) corresponding to each JSON key needed for initialization
                List<Class<?>> classes = iris.stream().map(this::getClassFromJSONKey).collect(Collectors.toList());
                // Initialize the time series

                tsClient.initTimeSeries(iris, classes, timeUnit, Type.INSTANTANEOUS, null, null);
                LOGGER.info(String.format("Initialized time series with the following IRIs: %s", String.join(", ", iris)));

            }
        }
    }

    /**
     * Checks whether a time series exists by checking whether any of the IRIs that should be attached to
     * the time series is not initialised in the central RDB lookup table using the time series client.
     *
     * @param iris The IRIs that should be attached to the same time series provided as list of strings.
     * @return True if all IRIs have a time series attached, false otherwise.
     */
    private boolean timeSeriesExist(List<String> iris) {
        // If any of the IRIs does not have a time series the time series does not exist
        for (String iri : iris) {
            try {
                if (!tsClient.checkDataHasTimeSeries(iri)) {
                    return false;
                }
                // If central RDB lookup table ("dbTable") has not been initialised, the time series does not exist
            } catch (DataAccessException e) {
                if (e.getMessage().contains("ERROR: relation \"dbTable\" does not exist")) {
                    return false;
                } else {
                    throw e;
                }
            }
        }
        return true;
    }

    /**
     * Updates the database with new readings.
     *
     * @param energyReadings The energy readings retrieved from the Excel file
     */
    public void updateData(JSONArray energyReadings) {
        if (energyReadings.isEmpty()) {
            throw new IllegalArgumentException("Readings can not be empty!");
        }
        // Check for null parameters
        Objects.requireNonNull(energyReadings, "Energy readings cannot be null");
        // Convert readings to hash maps
        Map<String, List<?>> energyReadingsMap = jsonArrayToMap(energyReadings);
        // Check for empty readings
        if (energyReadingsMap.isEmpty()) {
            throw new IllegalArgumentException("Readings cannot be empty");
        }

        /**
         * Create a hashmap for time-series that are not in the Excel energy readings
         * Note: these time-series needs to be instantiated for the OPF (optimal power flow) Agent
         * to later fill up their values.
         */
        Map<String, List<?>> calculatedOptimEnergyMap = new HashMap<>();
        ArrayList<String> entries = new ArrayList<String>();
        entries.add("TIME");
        entries.add("GENERATOR_NODE_VA_DEGREE");
        entries.add("GENERATOR_NODE_VM_KV");
        entries.add("CONNECTION_NODE_VA_DEGREE");
        entries.add("CONNECTION_NODE_VM_KV");
        entries.add("NEC_VA_DEGREE");
        entries.add("NEC_VM_KV");
        entries.add("CANTEEN_2_VA_DEGREE");
        entries.add("CANTEEN_2_VM_KV");
        entries.add("SPMS_VA_DEGREE");
        entries.add("SPMS_VM_KV");
        entries.add("RTP_VA_DEGREE");
        entries.add("RTP_VM_KV");
        entries.add("N1_3_VA_DEGREE");
        entries.add("N1_3_VM_KV");
        entries.add("N_2_VA_DEGREE");
        entries.add("N_2_VM_KV");
        entries.add("N_2_1_VA_DEGREE");
        entries.add("N_2_1_VM_KV");
        entries.add("SBS_VA_DEGREE");
        entries.add("SBS_VM_KV");
        entries.add("PIONEER_HALL_VA_DEGREE");
        entries.add("PIONEER_HALL_VM_KV");
        entries.add("THE_WAVE_VA_DEGREE");
        entries.add("THE_WAVE_VM_KV");
        entries.add("HALL_4_VA_DEGREE");
        entries.add("HALL_4_VM_KV");
        entries.add("EMB_VA_DEGREE");
        entries.add("EMB_VM_KV");
        entries.add("NYA_VA_DEGREE");
        entries.add("NYA_VM_KV");

        entries.add("NEC_GQ_KVAR");
        entries.add("NEC_GP_KW");
        entries.add("CANTEEN_2_GQ_KVAR");
        entries.add("CANTEEN_2_GP_KW");
        entries.add("SPMS_GQ_KVAR");
        entries.add("SPMS_GP_KW");
        entries.add("RTP_GQ_KVAR");
        entries.add("RTP_GP_KW");
        entries.add("N1_3_GQ_KVAR");
        entries.add("N1_3_GP_KW");
        entries.add("N_2_GQ_KVAR");
        entries.add("N_2_GP_KW");
        entries.add("N_2_1_GQ_KVAR");
        entries.add("N_2_1_GP_KW");
        entries.add("SBS_GQ_KVAR");
        entries.add("SBS_GP_KW");
        entries.add("PIONEER_HALL_GQ_KVAR");
        entries.add("PIONEER_HALL_GP_KW");
        entries.add("THE_WAVE_GQ_KVAR");
        entries.add("THE_WAVE_GP_KW");
        entries.add("HALL_4_GQ_KVAR");
        entries.add("HALL_4_GP_KW");
        entries.add("EMB_GQ_KVAR");
        entries.add("EMB_GP_KW");
        entries.add("NYA_GQ_KVAR");
        entries.add("NYA_GP_KW");

        // Add all entries to the hashmap (for later timeseries instantiation)
        for (String value : entries) {
            calculatedOptimEnergyMap.put(value, new ArrayList<>());
        }

        // Convert hash maps to time series
        List<TimeSeries<OffsetDateTime>> inputTimeSeries;
        List<TimeSeries<OffsetDateTime>> outputTimeSeries;
        try {
            inputTimeSeries = convertReadingsToTimeSeries(energyReadingsMap);
            outputTimeSeries = convertReadingsToTimeSeries(calculatedOptimEnergyMap);
        } catch (NoSuchElementException e) {
            throw new IllegalArgumentException("Readings can not be converted to proper time series!", e);
        }

        // Update each time series in the database
        for (TimeSeries<OffsetDateTime> ts : inputTimeSeries) {
            // Only update if there actually is data
            if (!ts.getTimes().isEmpty()) {
                try {
                    tsClient.addTimeSeriesData(ts);
                    LOGGER.debug("Time series updated for the following IRIs: {}", String.join(", ", ts.getDataIRIs()));
                } catch (Exception e) {
                    LOGGER.error("Error updating time series", e);
                    throw new JPSRuntimeException("Could not add time series data", e);
                }
            }
        }
        for (TimeSeries<OffsetDateTime> ts : outputTimeSeries) {
            // Only update if there actually is data
            if (!ts.getTimes().isEmpty()) {
                try {
                    tsClient.addTimeSeriesData(ts);
                    LOGGER.debug("Time series updated for the following IRIs: {}", String.join(", ", ts.getDataIRIs()));
                } catch (Exception e) {
                    LOGGER.error("Error updating time series", e);
                    throw new JPSRuntimeException("Could not add time series data", e);
                }
            }
        }

    }


    /**
     * Transform a JSON Array where each element is a single timestamp with all readings
     * into a Map, where values per key are gathered into a list.
     *
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
            for (Iterator<String> it = currentEntry.keys(); it.hasNext(); ) {
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

        // Convert the values to the pFVroper datatype //
        Map<String, List<?>> readingsMapTyped = new HashMap<>();
        for (String key : readingsMap.keySet()) {
            // Get the class (datatype) corresponding to the key
            String datatype = getClassFromJSONKey(key).getSimpleName();
            // Get current list with object type
            List<Object> valuesUntyped = readingsMap.get(key);
            List<?> valuesTyped;
            // Use mapping to cast the values into integer, double, boolean or string
            // The Number cast is required for org.json datatypes
            if (datatype.equals(Double.class.getSimpleName())) {
                valuesTyped = valuesUntyped.stream().map(value -> ((Number) value).doubleValue()).collect(Collectors.toList());
            } else {
                valuesTyped = valuesUntyped.stream().map(Object::toString).collect(Collectors.toList());
            }
            readingsMapTyped.put(key, valuesTyped);
        }
        return readingsMapTyped;
    }

    /**
     * Converts the readings in form of maps to time series' using the mappings from JSON key to IRI.
     *
     * @param energyReadings The energy readings as map.
     * @return A list of time series objects (one per mapping) that can be used with the time series client.
     */
    private List<TimeSeries<OffsetDateTime>> convertReadingsToTimeSeries(Map<String, List<?>> energyReadings)
            throws NoSuchElementException {
        // Extract the timestamps by mapping the private conversion method on the list items
        // that are supposed to be string (toString() is necessary as the map contains lists of different types)
        List<OffsetDateTime> energyTimestamps = energyReadings.get(HistoricalNTUEnergyAgent.timestampKey).stream()
                .map(timestamp -> (convertStringToOffsetDateTime(timestamp.toString()))).collect(Collectors.toList());
        // Construct a time series object for each mapping
        List<TimeSeries<OffsetDateTime>> timeSeries = new ArrayList<>();
        int addedTSCounter = 0;
        for (JSONKeyToIRIMapper mapping : mappings) {
            // Initialize the list of IRIs
            List<String> iris = new ArrayList<>();
            // Initialize the list of list of values
            List<List<?>> values = new ArrayList<>();
            // Go through all keys in the mapping
            for (String key : mapping.getAllJSONKeys()) {

                if (energyReadings.containsKey(key)) {
                    // Add IRI
                    iris.add(mapping.getIRI(key));
                    values.add(energyReadings.get(key));
                    LOGGER.info("Added " + key);
                    addedTSCounter += 1;
                }
            }
            // Timestamps depend on which readings are used for the mapping
            List<OffsetDateTime> times = energyTimestamps;
            // Create the time series object and add it to the list
            TimeSeries<OffsetDateTime> currentTimeSeries = new TimeSeries<>(times, iris, values);
            timeSeries.add(currentTimeSeries);
            LOGGER.info("Added " + String.valueOf(addedTSCounter) + " timeseries!");
        }

        return timeSeries;
    }

    /**
     * Converts a string into a datetime object with zone information using the zone globally define for the agent.
     *
     * @param timestamp The timestamp as string, the format should be equal to 2007-12-03T10:15:30.
     * @return The resulting datetime object.
     */
    private OffsetDateTime convertStringToOffsetDateTime(String timestamp) {
        // Convert first to a local time
        LocalDateTime localTime = LocalDateTime.parse(timestamp);
        return OffsetDateTime.of(localTime, HistoricalNTUEnergyAgent.ZONE_OFFSET);
    }

    /**
     * Returns the class (datatype) corresponding to a JSON key. Note: rules for the mapping are hardcoded in the method.
     *
     * @param jsonKey The JSON key as string.
     * @return The corresponding class as Class<?> object.
     */
    private Class<?> getClassFromJSONKey(String jsonKey) {
        if (jsonKey.equals("TIME")) {
            return String.class;
        } else {
            return Double.class;
        }
    }
}
