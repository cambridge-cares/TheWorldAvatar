package com.cmclinnovations.stack.clients.blazegraph;

import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;

import com.fasterxml.jackson.annotation.JacksonInject;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Namespace {

    public static final String PROPERTIES_FILE_DIRECTORY_KEY = "propertiesFileDirectory";

    private final String name;
    private final Properties properties = new Properties();

    /**
     * Constructor for the short form (just the name as a string)
     */
    @JsonCreator
    public Namespace(String name) {
        this.name = name;
    }

    /**
     * Constructor for the long form (full JSON object)
     */
    @JsonCreator
    public Namespace(@JsonProperty(value = "name") String name,
            @JsonProperty(value = "properties") Properties properties,
            @JsonProperty(value = "propertiesFile") Path propertiesFile,
            @JsonProperty(value = PROPERTIES_FILE_DIRECTORY_KEY) @JacksonInject(PROPERTIES_FILE_DIRECTORY_KEY) Path propertyFileDirectory) {
        this.name = name;
        if (null != propertiesFile) {
            if (null != propertyFileDirectory) {
                propertiesFile = propertyFileDirectory.resolve(propertiesFile);
            }
            try (Reader reader = Files.newBufferedReader(propertiesFile)) {
                // Read in properties from named properties file
                this.properties.load(reader);
            } catch (IOException ex) {
                throw new RuntimeException(
                        "Failed to load Blazegraph namespace properties from file '" + propertiesFile + "'.", ex);
            }
        }
        if (null != properties) {
            // Add any properties listed in the .json config file
            this.properties.putAll(properties);
        }
    }

    public String getName() {
        return name;
    }

    public Properties getProperties() {
        return properties;
    }

}
