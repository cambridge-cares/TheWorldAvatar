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

    private static final Logger LOGGER = LogManager.getLogger(Config.class);

    public String[] Config() {
        StringBuilder missingPropertiesErrorMessage = new StringBuilder();
        ContainerClient containerClient = new ContainerClient();
        PostGISEndpointConfig postGISEndpointConfig = containerClient.readEndpointConfig("postgis",
                PostGISEndpointConfig.class);

        String[] config = new String[5];
        config[0] = postGISEndpointConfig.getUsername();
        config[1] = postGISEndpointConfig.getPassword();
        config[2] = postGISEndpointConfig.getJdbcURL(EnvConfig.DB_3D);
        config[3] = postGISEndpointConfig.getJdbcURL(EnvConfig.DB_2D);
        config[4] = EnvConfig.DB_2D_TABLE;
        LOGGER.info("Retrieving the details for databases from file...");
        // retrieveDatabase(config, prop, missingPropertiesErrorMessage);
        String missingMessage = missingPropertiesErrorMessage.toString();
        if (!missingMessage.isEmpty()) {
            LOGGER.error("Missing Properties:\n" + missingMessage);
            throw new JPSRuntimeException("Missing Properties:\n" + missingMessage);
        }
        LOGGER.info("All required configurations have been retrieved!");
        return config;
    }


}
