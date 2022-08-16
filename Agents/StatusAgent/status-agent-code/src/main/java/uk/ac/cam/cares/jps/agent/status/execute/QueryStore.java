package uk.ac.cam.cares.jps.agent.status.execute;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.status.TestHandler;

/**
 * Handles reading SPARQL queries defined in text files.
 *
 * @author Michael Hillman
 */
public class QueryStore {

    /**
     * Logger.
     */
    private static final Logger LOGGER = LogManager.getLogger(QueryStore.class);

    /**
     * Absolute location of query directory.
     */
    private static final Path QUERY_DIR;

    /**
     * Static initialization.
     */
    static {
        QUERY_DIR = Paths.get(
                System.getProperty("user.home"),
                ".jps",
                "queries"
        );
    }

    /**
     * Reads and returns the content of the input query file.
     *
     * @param fileName query file name
     * @return
     */
    public static String readQuery(String fileName) {
        Path queryFile = Paths.get(QUERY_DIR.toString(), fileName);
        
        if (Files.exists(queryFile)) {

            try {
                return Files.readString(queryFile);
            } catch (IOException exception) {
                LOGGER.error("Could not ready query file '" + fileName + "'!", exception);
                return null;
            }
        }
        return null;
    }

}
// End of class.
