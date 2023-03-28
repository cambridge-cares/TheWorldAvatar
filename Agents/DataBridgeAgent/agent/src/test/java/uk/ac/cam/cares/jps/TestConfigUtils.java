package uk.ac.cam.cares.jps;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;

public class TestConfigUtils {
    private static final String originKey = "sparql.origin.endpoint";
    private static final String destinationKey = "sparql.destination.endpoint";

    public static File genSampleConfigFile(boolean isComplete, String originSparql, String destinationSparql) throws IOException {
        File file = new File(System.getProperty("user.dir") + "/config/endpoint.properties");
        // Check if the directory exists, create it if it doesn't
        if (!file.getParentFile().exists()) {
            file.getParentFile().mkdirs();
        }
        file.createNewFile();
        PrintWriter writer = new PrintWriter(file);
        writer.println(originKey + "=" + originSparql);
        if (isComplete) {
            writer.println(destinationKey + "=" + destinationSparql);
        }
        writer.close();
        return file;
    }
}
