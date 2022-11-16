package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Manages file retrieval related activities.
 *
 * @author qhouyee
 */
class FileManager {
    private static final String DATA_DIR = System.getProperty("user.dir") + "/data/";
    protected static final String PROPERTIES = DATA_DIR + "excel.properties";
    protected static final String QUERY_ENDPOINT_KEY = "queryEndpoint";
    protected static final String QUERY_ENDPOINT_PROPERTY = "sparql.query.endpoint";
    protected static final String UPDATE_ENDPOINT_KEY = "updateEndpoint";
    protected static final String UPDATE_ENDPOINT_PROPERTY = "sparql.update.endpoint";
    protected static final String RDB_URL_KEY = "dbUrl";
    protected static final String RDB_URL_PROPERTY = "db.url";
    protected static final String RDB_USER_KEY = "dbUser";
    protected static final String RDB_USER_PROPERTY = "db.user";
    protected static final String RDB_PASS_KEY = "dbPassword";
    protected static final String RDB_PASS_PROPERTY = "db.password";

    /**
     * An overloaded method that provides a default Excel workbook path.
     * Retrieves the file path for only one Excel workbook if it is available.
     *
     * @return The file path as a string.
     */
    protected static String retrieveExcelPath() throws IOException {
        return retrieveExcelPath(DATA_DIR);
    }

    /**
     * Retrieves the file path for only one Excel workbook if it is available. It will throw exceptions in other cases.
     *
     * @param directory The path to the directory containing an Excel file.
     * @return The file path as a string.
     */
    protected static String retrieveExcelPath(String directory) throws IOException {
        List<String> result;
        // Find all Excel files in the specified directory
        try (Stream<Path> walk = Files.walk(Paths.get(directory))) {
            result = walk
                    .filter(path -> !Files.isDirectory(path))
                    .map(Path::toString)
                    .filter(file -> file.endsWith("xls") || file.endsWith("xlsx"))
                    .collect(Collectors.toList());
        }
        if (result.size() == 0) {
            throw new JPSRuntimeException("No Excel workbook detected! Please place your file in the directory: " + directory);
        } else if (result.size() > 1) {
            throw new JPSRuntimeException("Multiple Excel workbooks detected! This agent can only process one workbook at a time.");
        } else {
            return result.get(0);
        }
    }

    /**
     * Retrieves client properties stored in the properties file.
     *
     * @param propertiesPath The file path to the .properties file.
     * @return The client configurations as a map, with the values being the retrieved inputs.
     */
    protected static Map<String, String> retrieveClientProperties(String propertiesPath) {
        Map<String, String> clientConfig = new HashMap<>();
        String missingPropertiesErrorMessage = "";
        try (InputStream input = new FileInputStream(propertiesPath)) {
            Properties prop = new Properties();
            prop.load(input);
            missingPropertiesErrorMessage = validateProperties(prop, clientConfig, QUERY_ENDPOINT_PROPERTY, QUERY_ENDPOINT_KEY, missingPropertiesErrorMessage);
            missingPropertiesErrorMessage = validateProperties(prop, clientConfig, UPDATE_ENDPOINT_PROPERTY, UPDATE_ENDPOINT_KEY, missingPropertiesErrorMessage);
            missingPropertiesErrorMessage = validateProperties(prop, clientConfig, RDB_URL_PROPERTY, RDB_URL_KEY, missingPropertiesErrorMessage);
            missingPropertiesErrorMessage = validateProperties(prop, clientConfig, RDB_USER_PROPERTY, RDB_USER_KEY, missingPropertiesErrorMessage);
            missingPropertiesErrorMessage = validateProperties(prop, clientConfig, RDB_PASS_PROPERTY, RDB_PASS_KEY, missingPropertiesErrorMessage);
            if (!missingPropertiesErrorMessage.isEmpty()) {
                throw new JPSRuntimeException("Missing Properties:\n" + missingPropertiesErrorMessage);
            }
        } catch (FileNotFoundException e) {
            throw new JPSRuntimeException("No client.properties file detected! Please place the file in the config directory.");
        } catch (IOException e) {
            throw new JPSRuntimeException("File could not be accessed! See error message for more details: " + e);
        }
        return clientConfig;
    }

    /**
     * Validates the client properties, and store their value in a map if it exists.
     *
     * @param prop                          A Properties object containing the required properties.
     * @param clientConfig                  A map to store the retrieved values.
     * @param property                      The property stored in the properties file.
     * @param propertyKey                   The mapping key to associate with the value.
     * @param missingPropertiesErrorMessage An error message that will be written if there is no property.
     */
    protected static String validateProperties(Properties prop, Map<String, String> clientConfig, String property, String propertyKey, String missingPropertiesErrorMessage) {
        if (prop.getProperty(property) == null) {
            missingPropertiesErrorMessage += property + " is missing! Please add the input to client.properties.\n";
        } else {
            clientConfig.put(propertyKey, prop.getProperty(property));
        }
        return missingPropertiesErrorMessage;
    }
}
