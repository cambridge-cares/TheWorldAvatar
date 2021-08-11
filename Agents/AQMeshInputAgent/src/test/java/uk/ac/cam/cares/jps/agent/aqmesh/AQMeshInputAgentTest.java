package uk.ac.cam.cares.jps.agent.aqmesh;

import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import uk.ac.cam.cares.jps.agent.utils.JSONKeyToIRIMapper;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class AQMeshInputAgentTest {

    // Temporary folder to place a properties file (same file for all potential tests)
    @ClassRule
    public static TemporaryFolder folder = new TemporaryFolder();

    @Test
    public void testConstructor() throws IOException, NoSuchFieldException, IllegalAccessException {
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "aqmesh.properties").toString();
        // Create a property file with a mapping folder that does not exist
        String folderName = "no_valid_folder";
        writePropertyFile(propertiesFile, Collections.singletonList("aqmesh.mappingfolder=" + folderName));
        // Run constructor that should give an exception
        try {
            new AQMeshInputAgent(propertiesFile);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertTrue(e.getMessage().contains("Folder does not exist:"));
            Assert.assertTrue(e.getMessage().contains(folderName));
        }

        // Create an empty folder
        folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Create a property file with the empty folder
        folderName = mappingFolder.getCanonicalPath().replace("\\","/");
        writePropertyFile(propertiesFile, Collections.singletonList("aqmesh.mappingfolder=" + folderName));
        // Run constructor that should give an exception
        try {
            new AQMeshInputAgent(propertiesFile);
            Assert.fail();
        }
        catch (IOException e) {
            Assert.assertTrue(e.getMessage().contains("No files in the folder:"));
            Assert.assertTrue(e.getMessage().contains(folderName));
        }

        // Add mapping files into the empty folder
        // All IRIs set
        String firstMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "firstMapping.properties").toString();
        String[] keys = {"key1", "key2" ,"key3"};
        ArrayList<String> mappings = new ArrayList<>();
        for (String key: keys) {
            mappings.add(key + "=example:prefix/api_" + key);
        }
        writePropertyFile(firstMappingFile, mappings);
        // No IRIs set
        String secondMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "secondMapping.properties").toString();
        mappings = new ArrayList<>();
        for (String key: keys) {
            mappings.add(key + "=");
        }
        writePropertyFile(secondMappingFile, mappings);
        // Create agent
        AQMeshInputAgent agent = new AQMeshInputAgent(propertiesFile);
        // Assert that the mappings were set
        Assert.assertEquals(2, agent.getNumberOfTimeSeries());
    }

    private void writePropertyFile(String filepath, List<String> properties) throws IOException {
        // Overwrite potentially existing properties file
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        for (String s : properties) {
            writer.write(s + "\n");
        }
        // Close the file and return the file
        writer.close();
    }

}
