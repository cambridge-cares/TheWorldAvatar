package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class AccessClientTest {
    @TempDir
    private static Path tempDir;
    @TempDir
    private static Path tempSecDir;
    private static final String endpoint = "http://ipaddress:9999/blazegraph/namespace/test/sparql";
    private static final String ifcOwlApi = "http://ifcowlconverter:8080/ifcowlconverter/";
    private static final String queryKey = "sparql.query.endpoint";
    private static final String updateKey = "sparql.update.endpoint";
    private static final String owlAgentKey = "ifc.owl.agent";

    @Test
    void testListTTLFiles() throws IOException {
        // Set up
        Path file = genSampleFile("ttl");
        // Test that there is a ttl file, and the results generated the same file path
        Set<String> results = AccessClient.listTTLFiles(tempSecDir.toString());
        assertTrue(results.size() > 0);
        results.forEach(line -> assertEquals(file.toString(), line));
    }

    @Test
    void testListTTLFilesFailWithNoDir() {
        Path noExistingDir = Paths.get(tempDir.toString(), "doesNotExist");
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> AccessClient.listTTLFiles(noExistingDir.toString()));
        assertTrue(thrownError.getMessage().contains("Failed to access target directory at"));
    }

    @Test
    void testListTTLFilesFailWithNoTTL() throws IOException {
        genSampleFile("txt");
        // No error is thrown when a directory exists, but should not return any path if there is no TTl file
        Set<String> results = AccessClient.listTTLFiles(tempSecDir.toString());
        assertEquals(0, results.size());
    }

    @Test
    void testRetrieveClientPropertiesNoFile() {
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, AccessClient::retrieveClientProperties);
        assertEquals("No config.properties file detected! Please place the file in the config directory.", thrownError.getMessage());
    }

    @Test
    void testRetrieveClientPropertiesMissingInputs() throws IOException {
        File config = genSampleConfigFile(false);
        try {
            JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, AccessClient::retrieveClientProperties);
            assertEquals("Missing Properties:\n" +
                    "sparql.update.endpoint is missing! Please add the input to client.properties.\n" +
                    "ifc.owl.agent is missing! Please add the input to client.properties.\n", thrownError.getMessage());
        } finally {
            config.delete();
        }
    }

    @Test
    void testRetrieveClientProperties() throws IOException {
        File config = genSampleConfigFile(true);
        try {
            Map<String, String> result = AccessClient.retrieveClientProperties();
            assertEquals(endpoint, result.get(queryKey));
            assertEquals(endpoint, result.get(updateKey));
            assertEquals(ifcOwlApi, result.get(owlAgentKey));
        } finally {
            config.delete();
        }
    }

    @Test
    void testCleanUp() throws IOException {
        Path sample = genSampleFile("txt");
        // Check the file is created and exists
        assertTrue(sample.toFile().exists());
        AccessClient.cleanUp(sample.toString());
        // Verify it has been deleted
        assertFalse(sample.toFile().exists());
    }

    @AfterAll
    static void cleanExtraDirectory() {
        File clientProperties = new File(System.getProperty("user.dir") + "/config/config.properties");
        File directory = clientProperties.getParentFile();
        if (directory.exists() && directory.isDirectory() && directory.list().length == 0) {
            directory.delete();
        }
    }

    private static Path genSampleFile(String fileExt) throws IOException {
        Path filePath = tempSecDir.resolve("data." + fileExt);
        List<String> lines = Arrays.asList("statement1", "statement2", "statement3");
        Files.write(filePath, lines);
        return filePath;
    }

    private static File genSampleConfigFile(boolean isComplete) throws IOException {
        File file = new File(System.getProperty("user.dir") + "/config/config.properties");
        // Check if the directory exists, create it if it doesn't
        if (!file.getParentFile().exists()) {
            file.getParentFile().mkdirs();
        }
        file.createNewFile();
        PrintWriter writer = new PrintWriter(file);
        writer.println(queryKey + "=" + endpoint);
        if (isComplete) {
            writer.println(updateKey + "=" + endpoint);
            writer.println(owlAgentKey + "=" + ifcOwlApi);
        }
        writer.close();
        return file;
    }
}