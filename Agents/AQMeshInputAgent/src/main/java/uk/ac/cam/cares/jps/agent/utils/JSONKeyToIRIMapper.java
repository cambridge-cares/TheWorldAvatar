package uk.ac.cam.cares.jps.agent.utils;

import java.io.*;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.UUID;
import java.util.regex.Pattern;

/**
 * Class to retrieve store a mapping from JSON key to knowledge graph IRI which extends a HashMap with fixed types.
 * @author Niklas Kasenburg
 */
public class JSONKeyToIRIMapper {

    private String iriPrefix;
    private final HashMap<String, String> mapping  = new HashMap<>();


    /**
     * Standard constructor can be used if the mapping to use does not contain keys without value.
     */
    public JSONKeyToIRIMapper()  {}

    /**
     * Constructor setting the prefix.
     * @param iriPrefix The prefix used when generating an IRI.
     */
    public JSONKeyToIRIMapper(String iriPrefix) {
        setIRIPrefix(iriPrefix);
    }

    /**
     * Constructor using directly a mapping file and setting the prefix.
     * @param iriPrefix The prefix used when generating an IRI.
     * @param filepath The path to the mapping file.
     */
    public JSONKeyToIRIMapper(String iriPrefix, String filepath) throws IOException {
        setIRIPrefix(iriPrefix);
        readMappingFromFile(filepath);
    }

    /**
     * Reads the JSON key to IRI from a properties files.
     * @param filepath The path to the properties file.
     */
    public void readMappingFromFile(String filepath) throws IOException {

        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }
        // Read the mapping from the properties file
        // Try-with-resource to ensure closure of input stream
        try (InputStream input = new FileInputStream(file)) {

            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            // Set the key value pairs from the properties to the HashMap
            for (Map.Entry<Object, Object> entry : prop.entrySet()) {
                String key = String.valueOf(entry.getKey());
                String value = String.valueOf(entry.getValue());
                // Create an IRI if no one exists for the key
                if (value.isEmpty()) {
                    // Create a random IRI in the format: <Prefix>_<JSONKey>_<UUID>
                    value = generateIRI(this.iriPrefix, key);
                }
                // Check that the IRI is valid
                if (checkIRI(value)) {
                    mapping.put(key, value);
                }
                else {
                    throw new IOException("The value for key "+ key + " is not a valid URI: " + value);
                }
            }
        }
    }

    /**
     * Generates a IRI based on the prefix, JSON key and a random UUID.
     * @param prefix The prefix, usually a combination of namespace and an API identifier.
     * @param jsonKey The JSON key.
     * @return The generated IRI as string.
     */
    public String generateIRI(String prefix, String jsonKey) {
        return String.join("_", prefix, jsonKey, UUID.randomUUID().toString());
    }

    /**
     * Retrieves the IRI for a JSON key. Wraps the get method of the HashMap field.
     * @param jsonKey The JSON key for which to retrieve the IRI.
     * @return The IRI that is mapped to the JSON key or null if there is no mapping.
     */
    public String getIRI(String jsonKey) {
        return mapping.get(jsonKey);
    }

    /**
     * Getter for the IRI prefix.
     * @return The currently set IRI prefix.
     */
    public String getIRIPrefix() {
        return iriPrefix;
    }

    /**
     * Setter for the IRI prefix.
     * @param iriPrefix The new IRI prefix.
     */
    public void setIRIPrefix(String iriPrefix) {
        // Checks whether the prefix is a valid URI before setting it
        if (checkIRI(iriPrefix)) {
            this.iriPrefix = iriPrefix;
        }
        else throw new IllegalArgumentException(iriPrefix + " is not a valid IRI.");
    }

    /**
     * Checks whether an IRI is valid.
     * @param iri The IRI to test provided as string.
     */
    private boolean checkIRI(String iri) {
        // Every legal (full) IRI contains at least one ':' character to separate the scheme from the rest of the IRI
        return Pattern.compile("\\w+\\S+:\\S+\\w+").matcher(iri).matches();
    }

}
