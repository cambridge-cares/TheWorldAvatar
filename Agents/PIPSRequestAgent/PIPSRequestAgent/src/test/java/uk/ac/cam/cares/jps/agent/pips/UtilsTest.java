package uk.ac.cam.cares.jps.agent.pips;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import org.junit.*;
import org.junit.rules.TemporaryFolder;

public class UtilsTest {

    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private Utils mockUtils;

    /**
     * Write to file
     * @param filepath path of file to write to
     * @param text String of characters to write into file
     * @throws IOException
     */
    private void writeToFile(String filepath, String text) throws IOException {
        // Overwrite potentially existing properties file
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        writer.write(text);
        // Close the file and return the file
        writer.close();
    }

    /**
     * Successful test for utils.readFromFile
     * @throws IOException
     */
    @Test
    public void testReadFromFileSuccess() throws IOException {
        String tempFile = Paths.get(folder.getRoot().toString(), "username.txt").toString();
        writeToFile(tempFile,"this is correct");
        mockUtils = new Utils();
        String text = mockUtils.readFromFile(tempFile);
        Assert.assertEquals("this is correct", text);
    }

    /**
     * Failed test for utils.readFromFile
     * @throws IOException
     */
    @Test
    public void testReadFromFileFail() throws IOException {
        String tempFile = "non existent filepath";
        mockUtils = new Utils();
        try {
            mockUtils.readFromFile(tempFile);
        } catch (Exception e) {
            Assert.assertTrue(e.getMessage().contains("non existent filepath"));
        }
    }
}