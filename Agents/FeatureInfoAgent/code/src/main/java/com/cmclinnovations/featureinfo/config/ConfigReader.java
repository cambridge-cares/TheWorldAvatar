package com.cmclinnovations.featureinfo.config;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.cmclinnovations.featureinfo.config.ConfigEntry.ConfigEntryBuilder;

/**
 * This class handles parsing the configuration file and generating
 * instances of the ConfigEntry class for storage.
 */
public class ConfigReader {

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ConfigReader.class);
    
    /**
     * Pool of parsed configuration entries.
     */
    private final Collection<ConfigEntry> entries;
    
    /**
     * Initialise a new ConfigReader instance.
     * 
     * @param entries Collection in which to store valid ConfigEntry instances.
     */
    public ConfigReader(Collection<ConfigEntry> entries) {
        this.entries = entries;
    }

    /**
     * Parse the config file at the input location.
     * 
     * @param configFile Absolute location of configuration file.
     * 
     * @throws IOException if configuration file cannot be read.
     * @throws IllegalArgumentExeption if configuration file has invalid syntax.
     */
    public void parseConfig(Path configFile) throws IOException, IllegalArgumentException {
        // File checks
        if(!Files.exists(configFile)) {
            LOGGER.error("OS reports that file does not exist: {}", configFile);
            throw new IOException("OS reports that config file does not exist.");
        }
        if(!Files.isRegularFile(configFile)) {
            LOGGER.error("OS reports that config file is not a regular file: {}", configFile);
            throw new IOException("OS reports that config file is not a regular file.");
        }
        if(!Files.isReadable(configFile)) {
            LOGGER.error("OS reports that config file exists but is not readable: {}", configFile);
            throw new IOException("OS reports that config file exists but is not readable.");
        }

        String configContent = Files.readString(configFile);
        LOGGER.info("Have read contents of config file at: {}", configFile);
        
        // Parse as JSON
        JSONObject configJSON = new JSONObject(configContent);

        // Get the main array
        JSONArray array = null;
        if(configJSON.has("entries")) {
            array = configJSON.getJSONArray("entries");
        } else if(configJSON.has("queries")) {
            array = configJSON.getJSONArray("queries");
        } else {
            throw new IllegalArgumentException("Configuration file has no main array under 'entries' or 'queries' key!");
        }

        // For each object within the array
        for(int i = 0; i < array.length(); i++) {
            JSONObject jsonEntry = array.getJSONObject(i);
            try {
                ConfigEntry configEntry = parseEntry(configFile.getParent(), jsonEntry);
                if(!entries.contains(configEntry)) entries.add(configEntry);
            } catch(JSONException jsonException) {
                LOGGER.error("Could not parse configuration entry #{}.", i);
                LOGGER.error(jsonException);
            }
        }
        LOGGER.debug("Parsed {} configuration entries.", this.entries.size());
    }

    /**
     * Given a JSON object from the configuration file, this method parses the parameters
     * and constructs a matching ConfigEntry instance.
     * 
     * @param configDir directory containing configuration file and queries.
     * @param entry JSON entry from configuration file.
     * @return Built ConfigEntry instance.
     * 
     * @throws JSONException if JSON is missing required entries.
     * @throws IllegalArgumentException if JSON values are invalid.
     * @throws IOException if query files cannot be read.
     */
    private ConfigEntry parseEntry(Path configDir, JSONObject jsonEntry) throws JSONException, IllegalArgumentException, IOException {
        // Initialise a new builder
        ConfigEntryBuilder builder = new ConfigEntryBuilder(configDir);

        if(jsonEntry.has("meta") || jsonEntry.has("time")) {
            // New format entry
            String id = jsonEntry.getString("id");
            String clazz = jsonEntry.getString("class");

            // Meta details
            JSONObject metaEntry = jsonEntry.optJSONObject("meta");
            String metaFile = null;
            if(metaEntry != null) {
                metaFile = metaEntry.optString("queryFile");
            }

            // Time details
            JSONObject timeEntry = jsonEntry.optJSONObject("time");
            String timeFile = null;
            int timeLimit = 0;
            String timeUnit = null;
            String timeReference = null;
            String timeDatabase = null;

            if(timeEntry != null) {
                timeFile = timeEntry.optString("queryFile");
                timeLimit = timeEntry.optInt("limit");
                timeUnit = timeEntry.optString("unit");
                timeReference = timeEntry.optString("reference");
                timeDatabase = timeEntry.optString("database");
            }

            // Build
            return builder.build(id, clazz, metaFile, timeFile, timeReference, timeLimit, timeUnit, timeDatabase);

        } else {
            // Assume old format entry
            String id = "entry-" + (entries.size() + 1);
            String clazz = jsonEntry.getString("class");

            // Meta details
            String metaFile = jsonEntry.optString("metaFile");

            // Time details
            String timeFile = jsonEntry.optString("timeFile");
            int timeLimit = jsonEntry.has("timeLimit") ? jsonEntry.getInt("timeLimit") : 24;
            String timeUnit = "hours";
            String timeReference = "NOW";
            String timeDatabase = jsonEntry.optString("databaseName");

            // Build
            return builder.build(id, clazz, metaFile, timeFile, timeReference, timeLimit, timeUnit, timeDatabase);
        }
            
    }

}
// End of class.