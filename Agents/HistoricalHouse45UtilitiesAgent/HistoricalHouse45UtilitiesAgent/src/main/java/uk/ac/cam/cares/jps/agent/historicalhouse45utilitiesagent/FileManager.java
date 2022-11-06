package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
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
                    .map(path -> path.toString())
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
     * Retrieves the endpoint url specified in client.properties.
     *
     * @param propertiesPath The file path to the .properties file.
     * @param key            The key to retrieve the endpoint url stored as a value in the .properties.
     * @return The endpoint's url as a string.
     */
    protected static String retrieveEndpoint(String propertiesPath, String key) {
        String endpoint = "";
        try (InputStream input = new FileInputStream(propertiesPath)) {
            Properties prop = new Properties();
            prop.load(input);
            endpoint = prop.getProperty(key);
        } catch (FileNotFoundException e) {
            throw new JPSRuntimeException("No client.properties file detected! Please place the file in the config directory.");
        } catch (IOException e) {
            throw new JPSRuntimeException("File could not be accessed! See error message for more details: " + e);
        }
        return endpoint;
    }
}
