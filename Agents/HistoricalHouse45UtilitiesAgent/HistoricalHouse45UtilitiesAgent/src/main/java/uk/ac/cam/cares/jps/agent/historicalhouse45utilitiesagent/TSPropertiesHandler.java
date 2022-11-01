package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Handles the properties file for the time series client.
 *
 * @author qhouyee
 */
class TSPropertiesHandler {
    // Logger for reporting info/errors.
    private static final Logger LOGGER = LogManager.getLogger(HistoricalHouse45UtilitiesAgent.class);
    private String propertiesFile; // file path to properties file
    private final String dateKey;
    private final List<String> readingsKey;
    private Map<String, String> iriMappings;

    /**
     * Standard constructor
     *
     * @param readings A Hashmap containing the readings.
     */
    protected TSPropertiesHandler(Map<String, List<?>> readings, String dateKey) throws IOException {
        if (!readings.isEmpty()) {
            this.readingsKey = new ArrayList<>(readings.keySet());
            // Remove the date or time data as it is not required to be pass in time series
            this.dateKey = dateKey;
            this.readingsKey.remove(this.dateKey);
        } else {
            throw new IllegalArgumentException("Readings can not be empty!");
        }
    }

    /**
     * Generates a Hashmap that maps the data IRI to their measures name. This mapping information is stored in an
     * external properties file on the first build process. Subsequent builds can read the same properties file to
     * ensure newer readings can be uploaded to the correct or existing time series database with the same dataIRI.
     *
     * @param propertiesFile The properties file path.
     * @return the iriMappings as a Hashmap in the format {dataIRI: excelHeader}
     */
    protected Map<String, String> generateIRIMappings(String propertiesFile) throws IOException {
        this.propertiesFile = propertiesFile;
        Properties prop = new Properties();

        // Only create a new property file if it doesn't exist
        File file = new File(this.propertiesFile);
        if (!file.exists()) {
            setIRIMappings(prop);
            saveToFile(prop);
        } else {
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
                LOGGER.info("Keys do not correspond to excel file. The property file was automatically regenerated.");
            } else {
                this.iriMappings = new HashMap<>();
                // Extract the key value pairs from the properties to the HashMap
                for (Map.Entry<Object, Object> entry : prop.entrySet()) {
                    String key = String.valueOf(entry.getKey());
                    String value = String.valueOf(entry.getValue());
                    // Create an IRI if it does not exist for the key
                    if (value.isEmpty()) {
                        value = generateIRI(HistoricalHouse45UtilitiesAgent.iriPrefix, key);
                        LOGGER.info("No IRI provided for key " + key + ". The IRI was automatically generated: " + value);
                        prop.setProperty(key, value);
                    }
                    // Stores their key and (new) value in the iriMapping hashmap
                    this.iriMappings.put(key, value);
                }
                saveToFile(prop);
            }
        }
    }

    /**
     * Checks for missing Excel headers in an existing property file. Returns false if there is no missing header,
     * return true otherwise.
     *
     * @param prop A Properties object to save.
     * @return Boolean
     */
    private boolean checkMissingHeaders(Properties prop) {
        // For Set.equals(), element order do not matter. Returns true if they have equal size and identical elements,
        // ie no missing header, and should return false in this method
        return !new HashSet<>(this.readingsKey).equals(prop.keySet());
    }

    /**
     * Generate and store a IRI mapping to the Excel column header as a HashMap. Set their properties in the object.
     *
     * @param prop A Properties object to save.
     */
    private void setIRIMappings(Properties prop) {
        this.iriMappings = new HashMap<>();

        for (String header : this.readingsKey) {
            String value = generateIRI(HistoricalHouse45UtilitiesAgent.iriPrefix, header);
            prop.setProperty(header, value);
            this.iriMappings.put(header, value);
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
            io.printStackTrace();
        }
    }
}