package uk.ac.cam.cares.jps.agent.carpark.file;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

public class ConfigReader {
    private static final Logger LOGGER = LogManager.getLogger(ConfigReader.class);
    private static final String MISSING_FILE_ERROR = "No file was found from the path ";
    private static final String RDB_URL_KEY = "db.url";
    private static final String RDB_USERNAME_KEY = "db.user";
    private static final String RDB_PASSWORD_KEY = "db.password";
    private static final String SPARQL_QUERY_ENDPOINT_KEY = "sparql.query.endpoint";
    private static final String SPARQL_UPDATE_ENDPOINT_KEY = "sparql.update.endpoint";
    private static final String SPARQL_USERNAME_KEY = "sparql.username";
    private static final String SPARQL_PASSWORD_KEY = "sparql.password";
    private static final String API_AVAILABLE_LOT_ENDPOINT_KEY = "carpark.api.lot.endpoint";
    private static final String API_LOT_TOKEN_KEY = "carpark.api.lot.token";
    private static final String API_PRICING_ENDPOINT_KEY = "carpark.api.pricing.endpoint";
    private static final String CARPARK_MAPPING_FOLDER_KEY = "carpark.mapping.folder";
    private static final String BUILDING_IDENTIFICATION_AGENT_ENDPOINT_KEY = "building.identification.agent.endpoint";
    private static final String BUILDING_PREFIX_KEY = "building.prefix";

    // Private constructor that should not be instantiated
    private ConfigReader() {}

    /**
     * Retrieves the RDB configuration from the specified file path.
     *
     * @param filepath Path to the config file for retrieval.
     * @return The RDB url, username, and password as a queue in this sequence.
     */
    public static Queue<String> retrieveRDBConfig(String filepath) throws IOException {
        String[] requiredProperties = new String[]{RDB_URL_KEY, RDB_USERNAME_KEY, RDB_PASSWORD_KEY};
        return retrieveFileContents(filepath, requiredProperties, true);
    }

    /**
     * Retrieves the SPARQL configuration from the specified file path. Note that username and password is optional and can be excluded in the results.
     *
     * @param filepath Path to the config file for retrieval.
     * @return The SPARQL query and update endpoints, username, and password as a queue in this sequence.
     */
    public static Queue<String> retrieveSparqlConfig(String filepath) throws IOException {
        String[] requiredProperties = new String[]{SPARQL_QUERY_ENDPOINT_KEY, SPARQL_UPDATE_ENDPOINT_KEY};
        Queue<String> configs = retrieveFileContents(filepath, requiredProperties, true);
        String[] optionalProperties = new String[]{SPARQL_USERNAME_KEY, SPARQL_PASSWORD_KEY};
        Queue<String> optionalConfigs = retrieveFileContents(filepath, optionalProperties, false);
        if (!optionalConfigs.isEmpty()) {
            configs.addAll(optionalConfigs);
        }
        return configs;
    }

    /**
     * Retrieves the API endpoints and their token from the specified file path.
     *
     * @param filepath Path to the config file for retrieval.
     * @return The available lot endpoint, pricing endpoint, and api token as a queue in this sequence.
     */
    public static Queue<String> retrieveAPIConfig(String filepath) throws IOException {
        String[] requiredProperties = new String[]{API_AVAILABLE_LOT_ENDPOINT_KEY, API_LOT_TOKEN_KEY, API_PRICING_ENDPOINT_KEY};
        return retrieveFileContents(filepath, requiredProperties, true);
    }

    /**
     * Retrieves the building matching configs from the agent.properties file from the specified file path.
     *
     * @param filepath Path to the config file for retrieval.
     * @return The building identification agent endpoint, prefix for building IRI as a queue in this sequence.
     */
    public static Queue<String> retrieveBuildingMatchingConfig(String filepath) throws IOException {
        String[] requiredProperties = new String[]{BUILDING_IDENTIFICATION_AGENT_ENDPOINT_KEY, BUILDING_PREFIX_KEY};
        return retrieveFileContents(filepath, requiredProperties, true);
    }

    /**
     * Retrieves the key to IRI mappings from the specified mapping folder.
     *
     * @param filepath  Path to the agent.properties containing the `carpark.mapping.folder` key.
     * @param iriPrefix The required IRI prefix to append to the IRIs in the mappings.
     * @return All key to IRI mappings.
     */
    public static List<JSONKeyToIRIMapper> retrieveKeyToIriMappings(String filepath, String iriPrefix) throws IOException {
        Queue<String> fileContent = retrieveFileContents(filepath, new String[]{CARPARK_MAPPING_FOLDER_KEY}, true);
        String mappingFolderEnvironmentVar = fileContent.poll();
        String mappingFolder = System.getenv(mappingFolderEnvironmentVar);
        File folder = new File(mappingFolder);
        File[] mappingFiles = folder.listFiles();
        if (mappingFiles == null || mappingFiles.length == 0) {
            String emptyDirectoryMessage = String.format("Directory is empty. Please ensure there is at least one mapping in the %s directory.", folder.getAbsolutePath());
            LOGGER.fatal(emptyDirectoryMessage);
            throw new IOException(emptyDirectoryMessage);
        }
        List<JSONKeyToIRIMapper> mappings = new ArrayList<>();
        for (File mappingFile : mappingFiles) {
            JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(iriPrefix, mappingFile.getAbsolutePath());
            mappings.add(mapper);
            mapper.saveToFile(mappingFile.getAbsolutePath());
        }
        return mappings;
    }

    /**
     * Retrieves the contents from the specified file path and the requested properties.
     *
     * @param filepath            Path to the config file for retrieval.
     * @param requestedProperties An array of properties that should be retrieved from the file.
     * @param isRequired          Indicates if the properties specified must have values or not.
     * @return All requested values in sequence from the input requested properties as a queue.
     */
    private static Queue<String> retrieveFileContents(String filepath, String[] requestedProperties, boolean isRequired) throws IOException {
        File file = new File(filepath);
        if (!file.exists()) {
            LOGGER.fatal(MISSING_FILE_ERROR + "{}", filepath);
            throw new IOException(MISSING_FILE_ERROR + filepath);
        }
        try (InputStream input = new FileInputStream(file)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            Queue<String> requestedValues = new ArrayDeque<>();
            // Check for required properties
            for (String propertyKey : requestedProperties) {
                if (prop.containsKey(propertyKey)) {
                    if (isRequired && (prop.getProperty(propertyKey) == null || prop.getProperty(propertyKey).isEmpty())) {
                        String errorMessage = String.format("Property %s cannot be empty in the file %s", propertyKey, filepath);
                        LOGGER.fatal(errorMessage);
                        throw new IllegalArgumentException(errorMessage);
                    } else if (!prop.getProperty(propertyKey).isEmpty()) {
                        // Property value should only be added if it is not empty
                        String propertyValue = prop.getProperty(propertyKey);
                        requestedValues.offer(propertyValue);
                    }
                } else {
                    String errorMessage = String.format("Missing property: %s in the file %s", propertyKey, filepath);
                    LOGGER.fatal(errorMessage);
                    throw new IOException(errorMessage);
                }
            }
            return requestedValues;
        }
    }
}
