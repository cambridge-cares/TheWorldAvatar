package uk.ac.cam.cares.jsp.linking;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Config extends ContainerClient {
    public static String DATABASE = System.getenv("DATABASE");
    private static final String PROPERTIES_FILEPATH = System.getProperty("user.dir") + "/inputs/config/endpoint.properties";
    private static final String NO_PROPERTIES_MSG = "No endpoint.properties file detected! Please place the file in the config directory.";
    private static final String INACCESSIBLE_CLIENT_PROPERTIES_MSG = "File could not be accessed! See error message for more details: ";
    private static final String SRC_DB_URL = "src.db.url";
    private static final String SRC_DB_USER = "src.db.user";
    private static final String SRC_DB_PASSWORD = "src.db.password";
    private static final String SRC_DB_3D = "src.db.3d";
     private static final String SRC_KG_URL = "src.kg.url";
    private static final String PREFIX1 = "onto.prefix1";
    private static final String PREFIX2 = "onto.prefix2";
    private static final String ISA = "onto.isA";
    private static final String HAS = "onto.has";
    private static final Logger LOGGER = LogManager.getLogger(Config.class);


    public String[] retrieveSQLConfig() {
        StringBuilder missingPropertiesErrorMessage = new StringBuilder();
        try (InputStream input = this.getClass().getClassLoader().getResourceAsStream("endpoint.proerties")) {
            Properties prop = new Properties();
            String[] config = new String[8];
            if(input != null){
                LOGGER.debug("Retrieving configuration from " + PROPERTIES_FILEPATH + "...");
                prop.load(input);
            }else{
                prop.setProperty(SRC_DB_URL, "jdbc:postgresql://dataintegrationagent-postgis:5432");
                prop.setProperty(SRC_DB_USER, "postgres");
                prop.setProperty(SRC_DB_PASSWORD, "postgis");
                prop.setProperty(SRC_DB_3D, "sg_ntu");
                prop.setProperty(SRC_KG_URL, "http://localhost:3838//blazegraph/namespace/ifc/sparql");
                prop.setProperty(PREFIX1, "https://w3id.org/bot#");
                prop.setProperty(PREFIX2, "https://www.theworldavatar.com/kg/ontobim/");
                prop.setProperty(ISA, "Building");
                prop.setProperty(HAS, "hasIfcRepresentation");
            }
            
            ContainerClient client = new ContainerClient();
            PostGISEndpointConfig postConfig = client.readEndpointConfig("postgis", PostGISEndpointConfig.class);
            config[0] = postConfig.getJdbcDriverURL();
            config[1] = postConfig.getUsername();
            config[2] = postConfig.getPassword();
            config[3] = postConfig.getJdbcURL(prop.getProperty(SRC_DB_3D));
            config[4] = prop.getProperty(SRC_KG_URL);
            config[5] = prop.getProperty(PREFIX1);
            config[6] = prop.getProperty(PREFIX1);
            config[7] = prop.getProperty(PREFIX2);

            LOGGER.info("Retrieving the details for source and target databases from file...");
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
    // private static void retrieveDatabase(String[] config, Properties prop, StringBuilder missingPropertiesErrorMessage) {

    //     ContainerClient client = new ContainerClient();
    //     PostGISEndpointConfig postConfig = client.readEndpointConfig("postgis", PostGISEndpointConfig.class);
    //     config[1] = postConfig.getUsername();
    //     config[2] = postConfig.getPassword();
    //     config[3] = postConfig.getJdbcURL(config[3]);

    //     config[0] = validateProperties(prop, SRC_DB_URL, missingPropertiesErrorMessage);
    //     config[1] = validateProperties(prop, SRC_DB_USER, missingPropertiesErrorMessage);
    //     config[2] = validateProperties(prop, SRC_DB_PASSWORD, missingPropertiesErrorMessage);
    //     config[3] = validateProperties(prop, SRC_DB_3D, missingPropertiesErrorMessage);
    //     config[4] = validateProperties(prop, PREFIX1, missingPropertiesErrorMessage);
    //     config[5] = validateProperties(prop, PREFIX2, missingPropertiesErrorMessage);
    //     config[4] = validateProperties(prop, ISA, missingPropertiesErrorMessage);
    //     config[5] = validateProperties(prop, HAS, missingPropertiesErrorMessage);
    // }

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
