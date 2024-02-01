package uk.ac.cam.cares.jps.agent.gfaagent;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

public class FileReader {
    public static InputStream getStream(String path) throws FileNotFoundException {
        return new FileInputStream(path);
    }
}

