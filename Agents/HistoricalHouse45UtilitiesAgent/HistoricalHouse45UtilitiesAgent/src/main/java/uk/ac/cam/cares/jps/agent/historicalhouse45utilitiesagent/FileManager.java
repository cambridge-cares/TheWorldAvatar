package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
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
}
