package uk.ac.cam.cares.jps.agent.aqmesh;

import org.junit.Assert;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

public class AQMeshInputAgentTest {

    // Temporary folder to place a properties file (same file for all potential tests)
    @ClassRule
    public static TemporaryFolder folder = new TemporaryFolder();

    // The default instance used in the tests
    private AQMeshInputAgent testAgent;

    @Before
    public void initializeAgent() throws URISyntaxException, IOException {
        // Create a properties file that points to the example/test mapping folder in the resources //
        String mappingFolder = Paths.get(Objects.requireNonNull(getClass().getResource("/mappings"))
                .toURI()).toString().replace("\\","/");
        // Filepath for the properties file
        String propertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writePropertyFile(propertiesFile, Collections.singletonList("aqmesh.mappingfolder=" + mappingFolder));
        testAgent = new AQMeshInputAgent(propertiesFile);
    }

    @Test
    public void testConstructor() throws IOException {
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

    @Test
    public void testGetClassFromJSONKey() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        // Make private method accessible
        Method getClassFromJSONKey = AQMeshInputAgent.class.getDeclaredMethod("getClassFromJSONKey", String.class);
        getClassFromJSONKey.setAccessible(true);
        // No specific key should return the string class
        Assert.assertEquals(String.class, getClassFromJSONKey.invoke(testAgent, "key"));
        // Interval should return integer class
        Assert.assertEquals(Integer.class, getClassFromJSONKey.invoke(testAgent, "key_p1"));
        // Voltage should return double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "key_voltage"));
        // Environment conditions should be double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "temperature"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "pressure"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "humidity"));
        // Noise readings should be double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "key_noise"));
        // Sensor readings should be double class
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "key_prescaled"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "key_prescale"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "key_slope"));
        Assert.assertEquals(Double.class, getClassFromJSONKey.invoke(testAgent, "key_offset"));
        // Battery low warning and particle modem overlap should be boolean
        Assert.assertEquals(Boolean.class, getClassFromJSONKey.invoke(testAgent, "battery_low"));
        Assert.assertEquals(Boolean.class, getClassFromJSONKey.invoke(testAgent, "particle_modem_overlap"));
    }

}
