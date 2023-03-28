package uk.ac.cam.cares.jps;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * A client that retrieve configuration files.
 *
 * @author qhouyee
 */
public class ConfigStore {
    private static final Logger LOGGER = LogManager.getLogger(DataBridgeAgent.class);
    private static final String NO_PROPERTIES_MSG = "No endpoint.properties file detected! Please place the file in the config directory.";
    private static final String INACCESSIBLE_CLIENT_PROPERTIES_MSG = "File could not be accessed! See error message for more details: ";
    private static final String PROPERTIES_FILEPATH = System.getProperty("user.dir") + "/config/endpoint.properties";
    private static final String ORIGIN_SPARQL_ENDPOINT = "sparql.origin.endpoint";
    private static final String DESTINATION_SPARQL_ENDPOINT = "sparql.destination.endpoint";

    /**
     * Retrieves SPARQL endpoints stored in the properties file.
     *
     * @return An array of these endpoints.
     */
    protected static String[] retrieveSPARQLConfig() {
        StringBuilder missingPropertiesErrorMessage = new StringBuilder();
        try (InputStream input = new FileInputStream(PROPERTIES_FILEPATH)) {
            Properties prop = new Properties();
            String[] config = new String[2];
            LOGGER.debug("Retrieving configuration from " + PROPERTIES_FILEPATH + "...");
            prop.load(input);
            config[0] = validateProperties(prop, ORIGIN_SPARQL_ENDPOINT, missingPropertiesErrorMessage);
            config[1] = validateProperties(prop, DESTINATION_SPARQL_ENDPOINT, missingPropertiesErrorMessage);
            String missingMessage = missingPropertiesErrorMessage.toString();
            if (!missingMessage.isEmpty()) {
                LOGGER.error("Missing Properties:\n" + missingMessage);
                throw new JPSRuntimeException("Missing Properties:\n" + missingMessage);
            }
            LOGGER.info("All required configurations have been retrieved!");
            return config;
        } catch (FileNotFoundException e) {
            LOGGER.error(NO_PROPERTIES_MSG);
            throw new JPSRuntimeException(NO_PROPERTIES_MSG);
        } catch (IOException e) {
            LOGGER.error(INACCESSIBLE_CLIENT_PROPERTIES_MSG + e);
            throw new JPSRuntimeException(INACCESSIBLE_CLIENT_PROPERTIES_MSG + e);
        }
    }

    /**
     * Validates the client properties, and return their value if it exists.
     *
     * @param prop                          A Properties object containing the required properties.
     * @param propertyKey                   The property key associated with the value.
     * @param missingPropertiesErrorMessage An error message that will be written if there is no property.
     * @return The value of the endpoints.
     */
    private static String validateProperties(Properties prop, String propertyKey, StringBuilder missingPropertiesErrorMessage) {
        if (prop.getProperty(propertyKey) == null) {
            missingPropertiesErrorMessage.append(propertyKey + " is missing! Please add the input to endpoint.properties.\n");
            LOGGER.error(propertyKey + " is missing! Please add the input to endpoint.properties.");
        } else {
            return prop.getProperty(propertyKey);
        }
        return "";
    }
}
