package com.cmclinnovations.featureinfo.config;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.google.common.base.Objects;

/**
 * This class represents a single entry within the agent's configuration
 * file. One entry is used to map a single T-Box IRI to an associated
 * meta/time query.
 */
public class ConfigEntry {
    
    /**
     * Unique ID for this entry (may be auto-generated).
     */
    private final String id;

    /**
     * Class IRI.
     */
    private String classIRI;

    /**
     * Location of the linked metadata query file (may be null).
     */
    private String metaQueryFile;

    /**
     * Contents of the linked metadata query (may be null).
     */
    private String metaQueryContent;

    /**
     * Location of the linked time series query file (may be null).
     */
    private String timeQueryFile;

    /**
     * Contents of the linked time series query (may be null).
     */
    private String timeQueryContent;

    /**
     * Time reference type (may be null).
     */
    private TimeReference timeReference;

    /**
     * Time limit value (may be null).
     */
    private int timeLimitValue;

    /**
     * Unit for time limit value (may be null).
     */
    private TimeUnit timeLimitUnit;

    /**
     * Name of Postgres database containing time series data.
     */
    private String timeDatabase;

    /**
     * Initialise a new ConfigEntry instance.
     * 
     * @param id unique ID for entry.
     */
    private ConfigEntry(String id) {
        this.id = id;
    }

    /**
     * Unique ID for entry.
     * 
     * @return unique ID.
     */
    public String getID() {
        return this.id;
    }

    /**
     * IRI for registered class.
     * 
     * @return IRI for registered class.
     */
    public String getClassIRI() {
        return this.classIRI;
    }

    /**
     * Meta query content.
     * 
     * @return meta query content.
     */
    public String getMetaQueryContent() {
        return this.metaQueryContent;
    }

    /**
     * Time query content.
     * 
     * @return time query content.
     */
    public String getTimeQueryContent() {
        return this.timeQueryContent;
    }

    /**
     * Time reference type.
     * 
     * @return Time reference type.
     */
    public TimeReference getTimeReference() {
        return this.timeReference;
    }

    /**
     * Time limit value.
     * 
     * @return time limit value.
     */
    public int getTimeLimitValue() {
        return this.timeLimitValue;
    }

    /**
     * Time limit unit.
     * 
     * @return time limit unit.
     */
    public TimeUnit getTimeLimitUnit() {
        return this.timeLimitUnit;
    }

    /**
     * Name of time series database.
     * 
     * @return name of time series database.
     */
    public String getTimeDatabase() {
        return this.timeDatabase;
    }

    /**
     * Generates hash code for this instance.
     * 
     * @returns hashcode.
     */
    @Override
    public int hashCode() {
        int hash = 5;
        int prime = 31;
        hash = prime * hash + (this.id != null ? this.id.hashCode() : 0);
        hash = prime * hash + (this.metaQueryFile != null ? this.metaQueryFile.hashCode() : 0);
        hash = prime * hash + (this.timeQueryFile != null ? this.timeQueryFile.hashCode() : 0);
        hash = prime * hash + (this.timeReference != null ? this.timeReference.hashCode() : 0);
        hash = prime * hash + (this.timeLimitValue);
        hash = prime * hash + (this.timeLimitUnit != null ? this.timeLimitUnit.hashCode() : 0);
        hash = prime * hash + (this.timeDatabase != null ? this.timeDatabase.hashCode() : 0);
        return hash;
    }

    /**
     * Checks for equality with input object.
     * 
     * @param object instance for comparison.
     * 
     * @returns equality state.
     */
    @Override
    public boolean equals(Object object) {
        if(object == this) return true;
        if(!(object instanceof ConfigEntry)) return false;

        ConfigEntry that = (ConfigEntry) object;

        if(!Objects.equal(this.id, that.id)) return false;
        if(!Objects.equal(this.metaQueryFile, that.metaQueryFile)) return false;
        if(!Objects.equal(this.timeQueryFile, that.timeQueryFile)) return false;
        if(!Objects.equal(this.timeReference, that.timeReference)) return false;
        if(!Objects.equal(this.timeLimitValue, that.timeLimitValue)) return false;
        if(!Objects.equal(this.timeLimitUnit, that.timeLimitUnit)) return false;
        if(!Objects.equal(this.timeDatabase, that.timeDatabase)) return false;

        return true;
    }

    /**
     * Builder class for ConfigEntry instances.
     */
    public static class ConfigEntryBuilder {

        /**
         * Logger for reporting info/errors.
         */
        private static final Logger LOGGER = LogManager.getLogger(ConfigEntryBuilder.class);

        /**
         * Directory containing configuration and query files.
         */
        private final Path configDirectory;

        /**
         * Constructor.
         * 
         * @param configDirectory Directory containing configuration and query files.
         */
        public ConfigEntryBuilder(Path configDirectory) {
            this.configDirectory = configDirectory;
        }

        /**
         * Build a new ConfigEntry with metadata information only.
         * 
         * @param id unique ID for entry.
         * @param clazz matching class IRI.
         * @param metaQueryFile relative location of metadata query file.
         * 
         * @return new ConfigEntry instance.
         * @throws IllegalArgumentException If values for enumerators are invalid.
         * @throws IOException If query files are present, but cannot be read.
         */
        public ConfigEntry build(
            String id,
            String classIRI,
            String metaQueryFile) throws IllegalArgumentException, IOException {

           return build(id, classIRI, metaQueryFile, null, null, 0, null, null);
        }

        /**
         * Build a new ConfigEntry with time series information only.
         * 
         * @param id unique ID for entry.
         * @param classIRI matching class IRI.
         * @param timeQueryFile relative location of time series query file.
         * @param timeReference reference for start of time limit.
         * @param timeLimitValue time limit value.
         * @param timeLimitUnit time limit unit.
         * @param timeDatabase name of Postgres database with time data.
         * 
         * @return new ConfigEntry instance.
         * @throws IllegalArgumentException If values for enumerators are invalid.
         * @throws IOException If query files are present, but cannot be read.
         */
        public ConfigEntry build(
            String id,
            String classIRI,
            String timeQueryFile,
            String timeReference,
            int timeLimitValue,
            String timeLimitUnit,
            String timeDatabase) throws IllegalArgumentException, IOException {

            return build(id, classIRI, null, timeQueryFile, timeReference, timeLimitValue, timeLimitUnit, timeDatabase);
        }

        /**
         * Build a new ConfigEntry.
         * 
         * @param id unique ID for entry.
         * @param classIRI matching class IRI.
         * @param metaQueryFile relative location of metadata query file.
         * @param timeQueryFile relative location of time series query file.
         * @param timeReference reference for start of time limit.
         * @param timeLimitValue time limit value.
         * @param timeLimitUnit time limit unit.
         * @param timeDatabase name of Postgres database with time data.
         * 
         * @return new ConfigEntry instance.
         * @throws IllegalArgumentException If values for enumerators are invalid.
         * @throws IOException If query files are present, but cannot be read.
         */
        public ConfigEntry build(
            String id,
            String classIRI,
            String metaQueryFile,
            String timeQueryFile,
            String timeReference,
            int timeLimitValue,
            String timeLimitUnit,
            String timeDatabase) throws IllegalArgumentException, IOException {

            // Check for valid parameters
            if(timeQueryFile != null && !timeQueryFile.isEmpty() && (timeDatabase == null || timeDatabase.isEmpty())) {
                throw new IllegalArgumentException("Invalid value for 'timeDatabase' parameter, please see documentation for allowed values.");
            }

            // Create and return ConfigEntry instance.
            ConfigEntry entry = new ConfigEntry(id);
            entry.classIRI = classIRI;
            entry.metaQueryFile = metaQueryFile;
            entry.timeQueryFile = timeQueryFile;

            if(timeReference == null) {
                entry.timeReference = TimeReference.NOW;
            } else {
                entry.timeReference = TimeReference.valueOf(timeReference.toUpperCase());
            }

            if(timeLimitUnit == null) {
                entry.timeLimitUnit = TimeUnit.HOURS;
            } else {
                entry.timeLimitUnit = TimeUnit.valueOf(timeLimitUnit.toUpperCase());
            }

            if(timeLimitValue == 0) {
                entry.timeLimitValue = 24;
            } else {
                entry.timeLimitValue = timeLimitValue;
            }
           
            entry.timeDatabase = timeDatabase;

            // Populate query contents
            readQueryContent(entry);
            return entry;
        }

        /**
         * Reads and caches the raw content of SPARQL query files.
         * 
         * @param entry
         */
        private void readQueryContent(ConfigEntry entry) throws IOException {
            // Parse metadata query
            if(entry.metaQueryFile != null && !entry.metaQueryFile.isEmpty()) {
                Path file = this.configDirectory.resolve(Paths.get(entry.metaQueryFile));
                entry.metaQueryContent = Files.readString(file);
            } else {
                LOGGER.info("No valid entry for metadata query file in entry '{}', skipping.", entry.id);
            }

            // Parse time series query
            LOGGER.debug("ON CLASS ENTRY: {}", entry.classIRI);
            LOGGER.debug("LOADING TIME FILE" + entry.timeQueryFile);

            if(entry.timeQueryFile != null && !entry.timeQueryFile.isEmpty()) {
                Path file = this.configDirectory.resolve(Paths.get(entry.timeQueryFile));
                entry.timeQueryContent = Files.readString(file);
                LOGGER.debug("QUERY CONTENT");
                LOGGER.debug(entry.timeQueryContent);
            } else {
                LOGGER.info("No valid entry for time series query file in entry '{}', skipping.", entry.id);
            }
        }
    }

}
// End of class.