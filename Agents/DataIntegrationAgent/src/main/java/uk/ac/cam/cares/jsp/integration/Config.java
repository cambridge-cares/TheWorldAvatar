package uk.ac.cam.cares.jsp.integration;

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

public class Config extends ContainerClient {

    public static String DATABASE = System.getenv("DATABASE");
    private static final String PROPERTIES_FILEPATH = System.getProperty("user.dir") + "/src/resources/endpoint.properties";
    private static final String NO_PROPERTIES_MSG = "No endpoint.properties file detected! Please place the file in the config directory.";
    private static final String INACCESSIBLE_CLIENT_PROPERTIES_MSG = "File could not be accessed! See error message for more details: ";
    private static final String SRC_DB_URL = "src.db.url";
    private static final String SRC_DB_USER = "src.db.user";
    private static final String SRC_DB_PASSWORD = "src.db.password";
    private static final String SRC_DB_3D = "src.db.3d";
    private static final String SRC_DB_2D = "src.db.2d";
    private static final String SRC_TABLE_2D = "src.table.2d";
    private static final Logger LOGGER = LogManager.getLogger(Config.class);

    /**
     * An overloaded method for stack transfers to retrieve the SQL database properties stored in the properties file.
     *
     * @return An array of these endpoints.
     */
    public String[] retrieveSQLConfig() {
        StringBuilder missingPropertiesErrorMessage = new StringBuilder();
        try (InputStream input = this.getClass().getClassLoader().getResourceAsStream("endpoint.properties")) {
            Properties prop = new Properties();
            String[] config = new String[6];
            if(input != null){
                LOGGER.debug("Retrieving configuration from " + PROPERTIES_FILEPATH + "...");   
                prop.load(input);
            }

            ContainerClient client = new ContainerClient();
            PostGISEndpointConfig postConfig = client.readEndpointConfig("postgis", PostGISEndpointConfig.class);
            config[1] = postConfig.getUsername();
            config[2] = postConfig.getPassword();
            config[3] = postConfig.getJdbcURL(prop.getProperty(SRC_DB_3D));
            config[4] = postConfig.getJdbcURL(prop.getProperty(SRC_DB_2D));
            config[5] = prop.getProperty(SRC_TABLE_2D);
            LOGGER.info("Retrieving the details for databases from file...");
            // retrieveDatabase(config, prop, missingPropertiesErrorMessage);
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
     * @param config                        An array to hold the database details.
     * @param prop                          A Properties object containing the required properties.
     * @param missingPropertiesErrorMessage An error message that will be written if there is no property.
     */
    private static void retrieveDatabase(String[] config, Properties prop, StringBuilder missingPropertiesErrorMessage) {

        config[0] = validateProperties(prop, SRC_DB_URL, missingPropertiesErrorMessage);
        config[1] = validateProperties(prop, SRC_DB_USER, missingPropertiesErrorMessage);
        config[2] = validateProperties(prop, SRC_DB_PASSWORD, missingPropertiesErrorMessage);
        config[3] = validateProperties(prop, SRC_DB_3D, missingPropertiesErrorMessage);
        config[4] = validateProperties(prop, SRC_DB_2D, missingPropertiesErrorMessage);
        config[5] = validateProperties(prop, SRC_TABLE_2D, missingPropertiesErrorMessage);
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
