package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class AccessClientIntegrationTest {
    @TempDir
    private static Path tempDir;
    private static final String TEST_BASE_URI = "https://www.example.org/";
    private static final String SUBJECT = TEST_BASE_URI + "Instance_123";
    private static final String PROPERTY = JunitTestUtils.RDF_TYPE.toString();
    private static final String OBJECT = JunitTestUtils.bimUri + "Instance";

    @AfterAll
    static void clearEndpoint() {
        JunitIntegrationTestUtils.clear();
    }

    @Test
    void testUploadStatements() throws IOException {
        // Set up
        Path file = genSampleFile("ttl");
        List<Path> pathList = genSampleFilePaths(file);
        // Execute method
        AccessClient.uploadStatements(JunitIntegrationTestUtils.SPARQL_ENDPOINT, pathList);
        // Verify that the file have been uploaded and the right triples are generated
        String[] result = JunitIntegrationTestUtils.query();
        assertEquals(SUBJECT, result[0]);
        assertEquals(PROPERTY, result[1]);
        assertEquals(OBJECT, result[2]);
    }

    private static Path genSampleFile(String fileExt) throws IOException {
        Path filePath = tempDir.resolve("data." + fileExt);
        String line = "<" + SUBJECT + "> <" + PROPERTY + "> <" + OBJECT + "> .";
        List<String> lines = List.of(line);
        Files.write(filePath, lines);
        return filePath;
    }

    private static List<Path> genSampleFilePaths(Path... filePaths) {
        List<Path> sampleFilePaths = new ArrayList<>();
        for (Path filePath : filePaths) {
            sampleFilePaths.add(filePath);
        }
        return sampleFilePaths;
    }
}