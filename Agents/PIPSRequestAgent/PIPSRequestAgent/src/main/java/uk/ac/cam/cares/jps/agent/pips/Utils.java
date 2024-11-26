package uk.ac.cam.cares.jps.agent.pips;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.DataInputStream;

public class Utils {
        /**
     * read content of files
     * @param filePath file path
     * @return content of the file
     * @throws IOException
     */
    public String readFromFile(String filePath) throws IOException {
        String content = null;
        try (DataInputStream reader = new DataInputStream(new FileInputStream(filePath))) {
            int nBytesToRead = reader.available();
            if(nBytesToRead > 0) {
                byte[] bytes = new byte[nBytesToRead];
                reader.read(bytes);
                content = new String(bytes);
            }
        }
        return content;
    }
}