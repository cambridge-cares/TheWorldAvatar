package uk.ac.cam.cares.jps.agent;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;
import java.util.*;

/**
 * Handles the properties file for the time series client.
 *
 * @author qhouyee
 */
class TSPropertiesHandler {
    // Logger for reporting info/errors.
    private static final Logger LOGGER = LogManager.getLogger(HistoricalPumpDataInstantiationAgent.class);
    private String propertiesFile; // file path to properties file
    private boolean isMultiTimeSeries = false;
    private Map<String, Map<String, String>> iriMappings;
    private final List<String> readingsKey;
    private final String UNDERSCORE = "_";

    /**
     * Standard constructor
     *
     * @param readings A Hashmap containing the readings.
     */
    protected TSPropertiesHandler(Map<String, Map<String, List<?>>> readings, String timeHeader) throws IOException {
        if (!readings.isEmpty()) {
            // Get any entry's values in the reading Hashmap
            // This will contain the column headers
            Map<String, List<?>> values = readings.values().iterator().next();
            List<String> colHeaders = new ArrayList<>(values.keySet());
            // Remove time column as it should not be passed into the time series as an IRI
            colHeaders.remove(timeHeader);

            // When there is only one time series
            if (readings.size() == 1) {
                this.readingsKey = colHeaders;
            } else {
                // When there are multiple time series
                isMultiTimeSeries = true;
                this.readingsKey = new ArrayList<>();
                for (String grouping : readings.keySet()) {
                    for (String header : colHeaders) {
                        // Append grouping to header to form the key
                        String key = grouping + this.UNDERSCORE + header;
                        this.readingsKey.add(key);
                    }
                }
            }
        } else {
            LOGGER.error("Readings can not be empty!");
            throw new IllegalArgumentException("Readings can not be empty!");
        }
    }

    /**
     * Generates a Hashmap that maps the data IRI to their group and column header. This mapping information is stored in an
     * external properties file on the first build process. Subsequent builds can read the same properties file to
     * ensure newer readings can be uploaded to the correct or existing time series database with the same dataIRI.
     *
     * @param propertiesFile The properties file path.
     * @return the iriMappings as a Hashmap of nested Hashmap in the format {group: {dataIRI: excelHeader}}.
     */
    protected Map<String, Map<String, String>> generateIRIMappings(String propertiesFile) throws IOException {
        this.propertiesFile = propertiesFile;
        Properties prop = new Properties();

        // Only create a new property file if it doesn't exist
        File file = new File(this.propertiesFile);
        if (!file.exists()) {
            LOGGER.debug("No properties file found! Generating a new file...");
            setIRIMappings(prop);
            saveToFile(prop);
        } else {
            LOGGER.debug("Properties file found! Reading the file...");
            readMappings(prop);
        }
        return this.iriMappings;
    }

    /**
     * Reads the measures names (usually in Excel headers) to IRI mappings from the provided file, and store them in a HashMap.
     * Regenerates a new file if there are missing headers.
     *
     * @param prop A Properties object to save.
     */
    private void readMappings(Properties prop) throws IOException {
        // try-with-resource to ensure input stream is always closed
        try (InputStream input = new FileInputStream(this.propertiesFile)) {
            prop.load(input);

            // If there are missing headers in the IRI, regenerate the property file from scratch
            if (checkMissingHeaders(prop)) {
                Properties newProp = new Properties();
                setIRIMappings(newProp);
                saveToFile(newProp);
                LOGGER.info("Keys do not correspond to Excel. The property file was automatically regenerated.");
            } else {
                this.iriMappings = new HashMap<>();
                // Extract the key value pairs from the properties to the HashMap
                for (Map.Entry<Object, Object> entry : prop.entrySet()) {
                    String key = String.valueOf(entry.getKey());
                    String iri = String.valueOf(entry.getValue());
                    // Create an IRI if it does not exist for the key
                    if (iri.isEmpty()) {
                        iri = generateIRI(HistoricalPumpDataInstantiationAgent.iriPrefix, key);
                        LOGGER.info("No IRI provided for key " + key + ". The IRI was automatically generated: " + iri);
                        prop.setProperty(key, iri);
                    }
                    // Stores their key and (new) value in the iriMapping hashmap
                    if (isMultiTimeSeries) {
                        int underscore_Index = key.indexOf(this.UNDERSCORE);
                        String group = key.substring(0, underscore_Index);
                        String colHeader = key.substring(underscore_Index + 1);
                        addNestedKeyValue(group, colHeader, iri);
                    } else {
                        addNestedKeyValue(ExcelParserHelper.BASE_KEY_FOR_SINGLE_TIMESERIES, key, iri);
                    }
                }
                saveToFile(prop);
            }
        }
    }

    /**
     * Checks for missing Excel headers in an existing property file.
     * Returns false if there is no missing header, return true otherwise.
     *
     * @param prop A Properties object to save.
     * @return Boolean true or false.
     */
    private boolean checkMissingHeaders(Properties prop) {
        // For Set.equals(), element order do not matter. Returns true if they have equal size and identical elements,
        // ie no missing header, and should return false in this method
        return !new HashSet<>(this.readingsKey).equals(prop.keySet());
    }

    /**
     * Generate and store group and IRI mapping to the Excel column header as a HashMap.
     * Set their properties based on `group_header = iri` format.
     *
     * @param prop A Properties object to save.
     */
    private void setIRIMappings(Properties prop) {
        this.iriMappings = new HashMap<>();
        for (String header : this.readingsKey) {
            LOGGER.info("Generating IRI for " + header + "...");
            String iri = generateIRI(HistoricalPumpDataInstantiationAgent.iriPrefix, header);
            prop.setProperty(header, iri);
            if (isMultiTimeSeries) {
                int underscore_Index = header.indexOf(this.UNDERSCORE);
                String group = header.substring(0, underscore_Index);
                String colHeader = header.substring(underscore_Index + 1);
                addNestedKeyValue(group, colHeader, iri);
            } else {
                addNestedKeyValue(ExcelParserHelper.BASE_KEY_FOR_SINGLE_TIMESERIES, header, iri);
            }
        }
    }

    /**
     * Add key value pair to the nested hash map in iriMappings.
     *
     * @param group  The group name, usually base for single time series.
     * @param header The Excel column header name.
     * @param iri    The IRI generated.
     * @return The generated IRI as string.
     */
    private void addNestedKeyValue(String group, String header, String iri) {
        Map<String, String> nestedMap = new HashMap<>();
        // If there is a pre-existing nested map, add key value directly
        if (this.iriMappings.containsKey(group)) {
            this.iriMappings.get(group).put(header, iri);
        } else {
            // Otherwise, create a new nested map and add the key to iri mappings
            nestedMap.put(header, iri);
            this.iriMappings.put(group, nestedMap);
        }
    }

    /**
     * Generates a random IRI based on the format <Prefix>_<MeasuresName>_<UUID>.
     *
     * @param prefix         The prefix, usually a combination of namespace and an identifier.
     * @param columnHeadings The measures name specified as Excel column headings.
     * @return The generated IRI as string.
     */
    private String generateIRI(String prefix, String columnHeadings) {
        return prefix + String.join("_", columnHeadings, UUID.randomUUID().toString());
    }


    /**
     * Save changes to a property file located in the file path input.
     *
     * @param prop A Properties object to save.
     */
    private void saveToFile(Properties prop) {
        // try-with-resource to ensure output stream is always closed
        try (OutputStream output = new FileOutputStream(this.propertiesFile)) {
            prop.store(output, null);
        } catch (IOException io) {
            LOGGER.error(io);
            throw new JPSRuntimeException(io);
        }
    }
}