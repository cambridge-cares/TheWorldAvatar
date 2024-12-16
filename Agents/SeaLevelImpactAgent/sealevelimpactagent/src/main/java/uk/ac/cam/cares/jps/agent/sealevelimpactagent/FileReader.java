package uk.ac.cam.cares.jps.agent.sealevelimpactagent;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class FileReader {

    /**
     * Read input files
     * @param path
     * @return
     * @throws FileNotFoundException
     */
    public static InputStream getStream(String path) throws FileNotFoundException {
        return new FileInputStream(path);
    }
}
