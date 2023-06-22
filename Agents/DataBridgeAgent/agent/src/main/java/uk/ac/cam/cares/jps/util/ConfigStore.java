package uk.ac.cam.cares.jps.util;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.DataBridgeAgent;
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
    private static final String VAL_TRANSFER_IN = "in";
    private static final String VAL_TRANSFER_OUT = "out";
    private static final String SRC_DB_URL = "src.db.url";
    private static final String SRC_DB_USER = "src.db.user";
    private static final String SRC_DB_PASSWORD = "src.db.password";
    private static final String TARGET_DB_URL = "target.db.url";
    private static final String TARGET_DB_USER = "target.db.user";
    private static final String TARGET_DB_PASSWORD = "target.db.password";


    /**
     * An overloaded method for non-stack transfers to retrieve the SQL database properties stored in the properties file.
     *
     * @return An array of these endpoints.
     */
    public static String[] retrieveSQLConfig() {
        return retrieveSQLConfig(null, "");
    }

    /**
     * Retrieves SQL database properties stored in the properties file or as a stack database.
     *
     * @param stackDatabase The stack database name passed as a parameter to the GET request.
     * @return An array of these endpoints.
     */
    public static String[] retrieveSQLConfig(String stackDatabase, String transferKey) {
        StringBuilder missingPropertiesErrorMessage = new StringBuilder();
        try (InputStream input = new FileInputStream(PROPERTIES_FILEPATH)) {
            Properties prop = new Properties();
            String[] config = new String[6];
            LOGGER.debug("Retrieving configuration from " + PROPERTIES_FILEPATH + "...");
            prop.load(input);
            if (transferKey.isEmpty()) {
                LOGGER.info("Retrieving the details for source and target databases from file...");
                retrieveDatabase(true, config, prop, missingPropertiesErrorMessage, "");
                retrieveDatabase(false, config, prop, missingPropertiesErrorMessage, "");
            } else if (transferKey.equals(VAL_TRANSFER_IN)) {
                LOGGER.info("Retrieving the details for source database from file...");
                retrieveDatabase(true, config, prop, missingPropertiesErrorMessage, "");
                LOGGER.info("Retrieving the details for the stack database to be targeted...");
                retrieveDatabase(false, config, prop, missingPropertiesErrorMessage, stackDatabase);
            } else if (transferKey.equals(VAL_TRANSFER_OUT)) {
                LOGGER.info("Retrieving the details for the stack database as a source...");
                retrieveDatabase(true, config, prop, missingPropertiesErrorMessage, stackDatabase);
                LOGGER.info("Retrieving the details for target database from file...");
                retrieveDatabase(false, config, prop, missingPropertiesErrorMessage, "");
            }
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
     * Retrieves the Database url, username and password, which will be stored in the config input.
     *
     * @param isSource                      A boolean indicating whether to retrieve the source or target endpoint.
     * @param config                        An array to hold the database details.
     * @param prop                          A Properties object containing the required properties.
     * @param missingPropertiesErrorMessage An error message that will be written if there is no property.
     * @param stackDatabase                 The stack namespace endpoint passed as a parameter to the GET request.
     */
    private static void retrieveDatabase(boolean isSource, String[] config, Properties prop, StringBuilder missingPropertiesErrorMessage, String stackDatabase) {
        // If the code does not need to be executed on a stack, retrieve the configs for source or target database from the properties file
        if (stackDatabase.isEmpty()) {
            if (isSource) {
                config[0] = validateProperties(prop, SRC_DB_URL, missingPropertiesErrorMessage);
                config[1] = validateProperties(prop, SRC_DB_USER, missingPropertiesErrorMessage);
                config[2] = validateProperties(prop, SRC_DB_PASSWORD, missingPropertiesErrorMessage);
            } else {
                config[3] = validateProperties(prop, TARGET_DB_URL, missingPropertiesErrorMessage);
                config[4] = validateProperties(prop, TARGET_DB_USER, missingPropertiesErrorMessage);
                config[5] = validateProperties(prop, TARGET_DB_PASSWORD, missingPropertiesErrorMessage);
            }
        } else {
            retrieveStackDatabase(stackDatabase, config, isSource);
        }
    }

    /**
     * Retrieves the SQL database within this stack as either the source or target.
     *
     * @param stackDatabase The stack database name passed as a parameter to the GET request.
     * @param config        The configuration array to store the results.
     * @param isSource      Indicates if the stack database is the source or target.
     */
    private static void retrieveStackDatabase(String stackDatabase, String[] config, boolean isSource) {
        ContainerClient client = new ContainerClient();
        PostGISEndpointConfig postConfig = client.readEndpointConfig("postgis", PostGISEndpointConfig.class);
        if (isSource) {
            config[0] = postConfig.getJdbcURL(stackDatabase);
            config[1] = postConfig.getUsername();
            config[2] = postConfig.getPassword();
        } else {
            config[3] = postConfig.getJdbcURL(stackDatabase);
            config[4] = postConfig.getUsername();
            config[5] = postConfig.getPassword();
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
        if (prop.getProperty(propertyKey) == null || prop.getProperty(propertyKey).isEmpty()) {
            missingPropertiesErrorMessage.append(propertyKey + " is missing! Please add the input to endpoint.properties.\n");
            LOGGER.error(propertyKey + " is missing! Please add the input to endpoint.properties.");
        } else {
            return prop.getProperty(propertyKey);
        }
        return "";
    }

    /**
     * Retrieves the TSClient configuration as an array. If the stack parameters are available, the stack database will be retrieved.
     * Otherwise, the configuration will be retrieved based on the source database of the properties file.
     *
     * @param stackDatabase The stack database name passed as a parameter to the POST request.
     * @param namespace     The namespace endpoint passed as a parameter to the POST request.
     * @return An array containing the database url, username, password, and the sparql endpoint.
     */
    public static String[] retrieveTSClientConfig(String namespace, String stackDatabase) {
        try (InputStream input = new FileInputStream(PROPERTIES_FILEPATH)) {
            String[] config = new String[4];
            StringBuilder missingPropertiesErrorMessage = new StringBuilder();
            Properties prop = new Properties();
            LOGGER.debug("Retrieving configuration from " + PROPERTIES_FILEPATH + "...");
            prop.load(input);
            LOGGER.info("Retrieving rdb database credentials...");
            retrieveDatabase(true, config, prop, missingPropertiesErrorMessage, stackDatabase);
            LOGGER.info("Retrieving sparql endpoint...");
            config[3] = namespace;
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
}
