package uk.ac.cam.cares.jps;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
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
    private static final String SRC_SPARQL_ENDPOINT = "sparql.src.endpoint";
    private static final String SRC_DB_URL = "src.db.url";
    private static final String SRC_DB_USER = "src.db.user";
    private static final String SRC_DB_PASSWORD = "src.db.password";
    private static final String TARGET_SPARQL_ENDPOINT = "sparql.target.endpoint";
    private static final String TARGET_DB_URL = "target.db.url";
    private static final String TARGET_DB_USER = "target.db.user";
    private static final String TARGET_DB_PASSWORD = "target.db.password";


    /**
     * An overloaded method to retrieve the SQL database properties stored in the properties file.
     *
     * @return An array of these endpoints.
     */
    protected static String[] retrieveSQLConfig() {
        return retrieveSQLConfig(null);
    }

    /**
     * Retrieves SQL database properties stored in the properties file or construct it from the stack database url.
     *
     * @param stackDatabase The stack database name passed as a parameter to the GET request.
     * @return An array of these endpoints.
     */
    protected static String[] retrieveSQLConfig(String stackDatabase) {
        StringBuilder missingPropertiesErrorMessage = new StringBuilder();
        try (InputStream input = new FileInputStream(PROPERTIES_FILEPATH)) {
            Properties prop = new Properties();
            String[] config = new String[6];
            LOGGER.debug("Retrieving configuration from " + PROPERTIES_FILEPATH + "...");
            prop.load(input);
            config[0] = validateProperties(prop, SRC_DB_URL, missingPropertiesErrorMessage);
            config[1] = validateProperties(prop, SRC_DB_USER, missingPropertiesErrorMessage);
            config[2] = validateProperties(prop, SRC_DB_PASSWORD, missingPropertiesErrorMessage);
            retrieveTargetDatabase(prop, stackDatabase, config);
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
     * Retrieves the target database from either the endpoint.properties if available, or defaults to the stack database otherwise.
     *
     * @param prop          A Properties object containing the required properties.
     * @param stackDatabase The stack database name passed as a parameter to the GET request.
     * @param config        The configuration array to store the results.
     */
    private static void retrieveTargetDatabase(Properties prop, String stackDatabase, String[] config) {
        ContainerClient client = new ContainerClient();
        if (stackDatabase != null) {
            PostGISEndpointConfig postConfig = client.readEndpointConfig("postgis", PostGISEndpointConfig.class);
            config[3] = postConfig.getJdbcURL(stackDatabase);
            config[4] = postConfig.getUsername();
            config[5] = postConfig.getPassword();
            LOGGER.info("Detected database parameter. Target stack database is configured for: " + config[3]);
        } else {
            LOGGER.info("No database parameter is available. Configuring target database based on endpoint.properties...");
            config[3] = prop.getProperty(TARGET_DB_URL);
            config[4] = prop.getProperty(TARGET_DB_USER);
            config[5] = prop.getProperty(TARGET_DB_PASSWORD);
            if (config[3].isEmpty()) {
                LOGGER.fatal("Target database url is empty in endpoint.properties. Please add an url or pass a database parameter to the GET request!");
                throw new JPSRuntimeException("Target database url is empty in endpoint.properties. Please add an url or pass a database parameter to the GET request!");
            } else if (config[4].isEmpty()) {
                LOGGER.fatal("Target database user is empty in endpoint.properties. Please add an user!");
                throw new JPSRuntimeException("Target database user is empty in endpoint.properties. Please add an user!");
            } else if (config[5].isEmpty()) {
                LOGGER.fatal("Target database password is empty in endpoint.properties. Please add the password!");
                throw new JPSRuntimeException("Target database password is empty in endpoint.properties. Please add the password!");
            }
            LOGGER.info("Target database has been configured to:" + config[3]);
        }
    }

    /**
     * An overloaded method to retrieve SPARQL endpoints stored in the properties file.
     *
     * @return An array of these endpoints.
     */
    protected static String[] retrieveSPARQLConfig() {
        return retrieveSPARQLConfig(null);
    }

    /**
     * Retrieves SPARQL endpoints stored in the properties file or construct the target from the stack endpoint.
     *
     * @param stackNamespace The stack namespace endpoint passed as a parameter to the GET request.
     * @return An array of these endpoints.
     */
    protected static String[] retrieveSPARQLConfig(String stackNamespace) {
        StringBuilder missingPropertiesErrorMessage = new StringBuilder();
        try (InputStream input = new FileInputStream(PROPERTIES_FILEPATH)) {
            Properties prop = new Properties();
            String[] config = new String[2];
            LOGGER.debug("Retrieving configuration from " + PROPERTIES_FILEPATH + "...");
            prop.load(input);
            config[0] = validateProperties(prop, SRC_SPARQL_ENDPOINT, missingPropertiesErrorMessage);
            configureTargetEndpoint(prop, stackNamespace, config);
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
     * Retrieves the target SPARQL endpoint from the endpoint.properties if available, and defaults to the stack endpoint otherwise.
     *
     * @param prop           A Properties object containing the required properties.
     * @param stackNamespace The stack namespace endpoint passed as a parameter to the GET request.
     * @param config         The configuration array to store the results.
     */
    private static void configureTargetEndpoint(Properties prop, String stackNamespace, String[] config) {
        ContainerClient client = new ContainerClient();
        if (stackNamespace != null) {
            BlazegraphEndpointConfig blazeConfig = client.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class);
            config[1] = blazeConfig.getServiceUrl() + "/namespace/" + stackNamespace + "/sparql";
            LOGGER.info("Detected namespace parameter. Target endpoint is configured within the stack at: " + config[1]);
        } else {
            LOGGER.info("No namespace parameter is available. Configuring target endpoint based on endpoint.properties...");
            config[1] = prop.getProperty(TARGET_SPARQL_ENDPOINT);
            if (config[1].isEmpty()) {
                LOGGER.fatal("Target SPARQL endpoint is empty in endpoint.properties. Please add an endpoint or pass a namespace parameter to the GET request!");
                throw new JPSRuntimeException("Target SPARQL endpoint is empty in endpoint.properties. Please add an endpoint or pass a namespace parameter to the GET request!");
            }
            LOGGER.info("Target endpoint has been configured to:" + config[1]);
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
}
