package uk.ac.cam.cares.jps.agent.utils;

import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class JSONKeyToIRIMapperTest {

    // Temporary folder to place a properties file (same file for all potential tests)
    @ClassRule
    public static TemporaryFolder folder = new TemporaryFolder();

    private final String prefix = "http://example.com/api";

    @Test
    public void testConstructorWithPrefix() {
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(prefix);
        Assert.assertEquals(prefix, mapper.getIRIPrefix());
    }

    @Test
    public void testConstructorWithPrefixAndMappingFile() throws IOException {
        // Create a mapping file
        String filepath = Paths.get(folder.getRoot().toString(), "mapping.properties").toString();
        String[] keys = new String[]{"key1", "key2", "key3"};
        ArrayList<String> mappings = generateMapping(keys, null, "prefix");
        writeMappingFile(filepath, mappings);
        // Create mapper
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(prefix, filepath);
        Assert.assertEquals(prefix, mapper.getIRIPrefix());
        // Assert that the map was set correctly
        for(String key: keys) {
            Assert.assertEquals(prefix + "_" + key, mapper.getIRI(key));
        }
    }

    @Test
    public void testSetAndGetIriPrefix() {
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper();
        // Should result in an error due to prefix not a valid IRI
        try {
            mapper.setIRIPrefix("prefix");
            Assert.fail();
        }
        catch (IllegalArgumentException e) {
            Assert.assertEquals("prefix is not a valid IRI.", e.getMessage());
        }
        mapper.setIRIPrefix(prefix);
        Assert.assertEquals(prefix, mapper.getIRIPrefix());
    }

    @Test
    public void testGenerateIRI() {
        String uuid = "12345";
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper();
        try(MockedStatic<UUID> mockUUID = Mockito.mockStatic(UUID.class, Mockito.RETURNS_DEEP_STUBS)) {
            mockUUID.when(() -> UUID.randomUUID().toString()).thenReturn(uuid);
            Assert.assertEquals(prefix + "_key_" + uuid, mapper.generateIRI(prefix, "key"));
        }
    }

    @Test
    public void testReadMappingFromFile() throws IOException {
        // Filepath to not yet created file in temporary test folder
        String filepath = Paths.get(folder.getRoot().toString(), "mapping.properties").toString();
        // Error messages
        String fileNotFound = "No properties file found at specified filepath: " + filepath;
        // Initialize mapper
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(prefix);
        // Test for non-existing properties file
        try {
            mapper.readMappingFromFile(filepath);
            Assert.fail();
        } catch (IOException e) {
            Assert.assertEquals(FileNotFoundException.class, e.getClass());
            Assert.assertEquals(fileNotFound, e.getMessage());
        }
        // Test with one mapping that is invalid IRI //
        // Initialize mapping
        String[] keys = new String[]{"key1", "key2", "key3"};
        ArrayList<String> mappings = generateMapping(keys, "key3", "prefix");
        // Write mapping to file
        writeMappingFile(filepath, mappings);
        // Test that error is thrown when parsing invalid IRI
        try {
            mapper.readMappingFromFile(filepath);
            Assert.fail();
        } catch (IOException e) {
            Assert.assertEquals("The value for key key3 is not a valid URI: prefix", e.getMessage());
        }
        // Test with one mapping that requires the generation of an IRI //
        // Define the key without mapping
        String keyWithoutMapping = "key2";
        // Create a spy to mock the generating method
        JSONKeyToIRIMapper spyMapper = Mockito.spy(mapper);
        Mockito.when(spyMapper.generateIRI(prefix, keyWithoutMapping)).thenReturn(prefix + "_" + keyWithoutMapping);
        // Initialize mapping
        mappings = generateMapping(keys, keyWithoutMapping, "");
        // Write mapping to file
        writeMappingFile(filepath, mappings);
        // Parse the mapping
        spyMapper.readMappingFromFile(filepath);
        // Assert that the map was set correctly
        for(String key: keys) {
            Assert.assertEquals(prefix + "_" + key, spyMapper.getIRI(key));
        }
    }

    private ArrayList<String> generateMapping(String[] keys, String keyWithInvalidIRI, String invalidOrMissingIRI) {
        ArrayList<String> iris = new ArrayList<>();
        for (String key: keys) {
            iris.add(prefix + "_" + key);
        }
        ArrayList<String> mappings = new ArrayList<>();
        for (int i = 0; i < keys.length; i++) {
            if (keys[i].equals(keyWithInvalidIRI)) {
                mappings.add(keys[i] + "=" + invalidOrMissingIRI);
            }
            else {
                mappings.add(keys[i] + "=" + iris.get(i));
            }
        }
        return mappings;
    }

    private void writeMappingFile(String filepath, List<String> mappings) throws IOException {
        // Overwrite potentially existing properties file
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        for (String s : mappings) {
            writer.write(s + "\n");
        }
        // Close the file and return the file
        writer.close();
    }

}
