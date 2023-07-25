package uk.ac.cam.cares.jps.agent.dashboard.utils;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.dashboard.DashboardAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 * A class that provides method to retrieve configurations.
 *
 * @author qhouyee
 */
public class ConfigStore {
    private static final Logger LOGGER = LogManager.getLogger(DashboardAgent.class);
    private static final String NO_PROPERTIES_MSG = "No credentials.properties file detected! Please place the file in the config directory.";
    private static final String INACCESSIBLE_CLIENT_PROPERTIES_MSG = "File could not be accessed! See error message for more details: ";
    private static final String PROPERTIES_FILEPATH = System.getProperty("user.dir") + "/config/credentials.properties";
    private static final String DASHBOARD_USERNAME = "dashboard.user";
    private static final String DASHBOARD_PASSWORD = "dashboard.pass";

    /**
     * Retrieve the dashboard credentials stored in the properties file.
     *
     * @return An array of this information.
     */
    public static String[] retrieveCredentials() {
        StringBuilder missingPropertiesErrorMessage = new StringBuilder();
        try (InputStream input = new FileInputStream(PROPERTIES_FILEPATH)) {
            Properties prop = new Properties();
            String[] config = new String[2];
            prop.load(input);
            config[0] = validateProperties(prop, DASHBOARD_USERNAME, missingPropertiesErrorMessage);
            config[1] = validateProperties(prop, DASHBOARD_PASSWORD, missingPropertiesErrorMessage);
            String missingMessage = missingPropertiesErrorMessage.toString();
            if (!missingMessage.isEmpty()) {
                LOGGER.error("Missing Properties:\n" + missingMessage);
                throw new JPSRuntimeException("Missing Properties:\n" + missingMessage);
            }
            LOGGER.debug("All required credentials have been retrieved!");
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
     * Validates the properties, and return their value if it exists.
     *
     * @param prop                          A Properties object containing the required properties.
     * @param propertyKey                   The property key associated with the value.
     * @param missingPropertiesErrorMessage An error message that will be written if there is no property.
     * @return The value of the endpoints.
     */
    private static String validateProperties(Properties prop, String propertyKey, StringBuilder missingPropertiesErrorMessage) {
        if (prop.getProperty(propertyKey) == null || prop.getProperty(propertyKey).isEmpty()) {
            missingPropertiesErrorMessage.append(propertyKey + " is missing! Please add the input to credentials.properties.\n");
            LOGGER.error(propertyKey + " is missing! Please add the input to credentials.properties.");
        } else {
            return prop.getProperty(propertyKey);
        }
        return "";
    }
}
