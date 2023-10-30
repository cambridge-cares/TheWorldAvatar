package uk.ac.cam.cares.jps.agent.cea.utils;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

public class FileReader {
    public static InputStream getStream(String path) throws FileNotFoundException {
        return new FileInputStream(path);
    }
}
