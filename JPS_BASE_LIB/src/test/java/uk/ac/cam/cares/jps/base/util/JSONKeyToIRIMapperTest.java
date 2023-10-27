package uk.ac.cam.cares.jps.base.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.UUID;

import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

public class JSONKeyToIRIMapperTest {

    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private final String prefix = "http://example.com/api";
    private final String[] keys = new String[]{"key1", "key2", "key3"};

    @Test
    public void testConstructorWithPrefix() {
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(prefix);
        Assert.assertEquals(prefix, mapper.getIRIPrefix());
    }

    @Test
    public void testConstructorWithPrefixAndMappingFile() throws IOException {
        // Create a mapping file
        String filepath = Paths.get(folder.getRoot().toString(), "mapping.properties").toString();
        ArrayList<String> mappings = generateMapping(keys, null, "prefix");
        writeMappingFile(filepath, mappings);
        // Create mapper
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(prefix, filepath);
        Assert.assertEquals(prefix, mapper.getIRIPrefix());
        // Assert that the map was set correctly
        for(String key: keys) {
            Assert.assertEquals(prefix + "_" + key, mapper.getIRI(key));
            Assert.assertEquals(key, mapper.getJSONKey(prefix + "_" + key));
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
    public void testReadMappingFromFileWithInvalidPath() {
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
    }

    @Test
    public void testReadMappingFromFileWithInvalidIRI() throws IOException {
        // Initialize mapper
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(prefix);
        // Initialize mapping
        ArrayList<String> mappings = generateMapping(keys, "key3", "prefix");
        // Write mapping to file
        String filepath = Paths.get(folder.getRoot().toString(), "mapping.properties").toString();
        writeMappingFile(filepath, mappings);
        // Test that error is thrown when parsing invalid IRI
        try {
            mapper.readMappingFromFile(filepath);
            Assert.fail();
        } catch (IOException e) {
            Assert.assertEquals("The value for key key3 is not a valid URI: prefix", e.getMessage());
        }
    }

    @Test
    public void testReadMappingFromFileWithDuplicatedIRI() throws IOException {
        // Initialize mapper
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(prefix);
        // Initialize mapping
        ArrayList<String> mappings = generateMapping(keys, null, null);
        String sameIRI = mappings.get(0).split("=")[1];
        mappings.set(2, keys[2] + "=" + sameIRI);
        // Write mapping to file
        String filepath = Paths.get(folder.getRoot().toString(), "mapping.properties").toString();
        writeMappingFile(filepath, mappings);
        // Test that error is thrown when using same IRI more than once
        try {
            mapper.readMappingFromFile(filepath);
            Assert.fail();
        } catch (IOException e) {
            Assert.assertTrue(e.getMessage().contains("The IRI " + sameIRI + " is already used for the key "));
            Assert.assertTrue(e.getMessage().contains(keys[0]));
            Assert.assertTrue(e.getMessage().contains(keys[2]));
        }
    }

    @Test
    public void testReadMappingFromFileWithEmptyIRI() throws IOException {
        // Initialize mapper
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(prefix);
        // Define the key without mapping
        String keyWithoutMapping = "key2";
        // Create a spy to mock the generating method
        JSONKeyToIRIMapper spyMapper = Mockito.spy(mapper);
        Mockito.when(spyMapper.generateIRI(prefix, keyWithoutMapping)).thenReturn(prefix + "_" + keyWithoutMapping);
        // Initialize mapping
        ArrayList<String> mappings = generateMapping(keys, keyWithoutMapping, "");
        // Write mapping to file
        String filepath = Paths.get(folder.getRoot().toString(), "mapping.properties").toString();
        writeMappingFile(filepath, mappings);
        // Parse the mapping
        spyMapper.readMappingFromFile(filepath);
        // Assert that the map was set correctly
        for(String key: keys) {
            Assert.assertEquals(prefix + "_" + key, spyMapper.getIRI(key));
        }
    }

    @Test
    public void testSaveToFile() throws IOException {
        // Create a mapping file
        String filepath = Paths.get(folder.getRoot().toString(), "mapping.properties").toString();
        ArrayList<String> mappings = generateMapping(keys, null, "prefix");
        writeMappingFile(filepath, mappings);
        // Create mapper
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(prefix, filepath);
        // Create a file in the temporary folder to store the mapping
        File mappingFile = folder.newFile("test.properties");
        // Save mappings to file
        mapper.saveToFile(mappingFile.getCanonicalPath());
        // Read in the file and check if the mapping is correct
        try(InputStream input = new FileInputStream(mappingFile.getCanonicalPath())) {
            Properties prop = new Properties();
            prop.load(input);
            for(String mapping: mappings) {
                String[] key_value = mapping.split("=");
                // The key should be present in the properties
                Assert.assertTrue(prop.containsKey(key_value[0]));
                // The value should have the correct value
                Assert.assertEquals(key_value[1], prop.getProperty(key_value[0]));
            }
        }
    }

    @Test
    public void testGetAllIRIs() throws IOException {
        // Create a mapping file
        String filepath = Paths.get(folder.getRoot().toString(), "mapping.properties").toString();
        ArrayList<String> mappings = generateMapping(keys, null, "prefix");
        writeMappingFile(filepath, mappings);
        // Initialize mapper
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(prefix, filepath);
        List<String> iris = mapper.getAllIRIs();
        for (String mapping: mappings) {
            String iri = mapping.split("=")[1];
            Assert.assertTrue(iris.contains(iri));
        }
    }

    @Test
    public void testGetAllJSONKeys() throws IOException {
        // Create a mapping file
        String filepath = Paths.get(folder.getRoot().toString(), "mapping.properties").toString();
        String[] keys = new String[]{"key1", "key2", "key3"};
        ArrayList<String> mappings = generateMapping(keys, null, "prefix");
        writeMappingFile(filepath, mappings);
        // Initialize mapper
        JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(prefix, filepath);
        List<String> setKeys = mapper.getAllJSONKeys();
        for (String key: keys) {
            Assert.assertTrue(setKeys.contains(key));
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
