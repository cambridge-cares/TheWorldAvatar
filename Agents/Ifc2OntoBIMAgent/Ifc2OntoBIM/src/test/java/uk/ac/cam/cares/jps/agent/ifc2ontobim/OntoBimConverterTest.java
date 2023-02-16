package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.TTLWriter;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class OntoBimConverterTest {
    @TempDir
    private static Path tempDir;
    @TempDir
    private static Path tempSecDir;
    private static Path sampleTtl;

    @BeforeAll
    static void init() throws IOException {
        sampleTtl = tempDir.resolve("test.ttl");
        List<String> lines = new ArrayList<>();
        lines.add("@prefix rdf: <"+ JunitTestUtils.rdfUri +"> .");
        lines.add("@prefix bim: <"+ JunitTestUtils.bimUri +"> .");
        lines.add("@prefix inst: <"+ JunitTestUtils.bimUri +"> .");
        lines.add("");
        lines.add("bim:Zone_19 rdf:type bim:Zone .");
        Files.write(sampleTtl, lines);
    }

    @Test
    void testListTTLFiles() throws IOException {
        // Set up
        Path file = genSampleFile("ttl");
        OntoBimConverter converter = new OntoBimConverter();

        // Test that there is a ttl file, and the results generated the same file path
        Set<String> results = converter.listTTLFiles(tempSecDir.toString());
        assertTrue(results.size() > 0);
        results.forEach(line -> assertEquals(file.toString(), line));
    }

    @Test
    void testListTTLFilesFailWithNoDir() {
        Path noExistingDir = Paths.get(tempDir.toString(), "doesNotExist");
        OntoBimConverter converter = new OntoBimConverter();
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> converter.listTTLFiles(noExistingDir.toString()));
        assertTrue(thrownError.getMessage().contains("Failed to access target directory at"));
    }

    @Test
    void testListTTLFilesFailWithNoTTL() throws IOException {
        genSampleFile("txt");
        OntoBimConverter converter = new OntoBimConverter();
        // No error is thrown when a directory exists, but should not return any path if there is no TTl file
        Set<String> results = converter.listTTLFiles(tempSecDir.toString());
        assertEquals(0, results.size());
    }

    @Test
    void testConvertOntoBIM() {
        OntoBimConverter converter = new OntoBimConverter();
        try (MockedConstruction<TTLWriter> mockWriter = Mockito.mockConstruction(TTLWriter.class)) {
            converter.convertOntoBIM(sampleTtl.toString());
            // Verify the last method was called
            Mockito.verify(mockWriter.constructed().get(0)).writeTTLWithIntermediateFiles(Mockito.anyList(), Mockito.any(), Mockito.anyMap());
        }
    }

    @Test
    void testConvertOntoBIMFailWithWrongFile() {
        // May throw different exceptions locally and in Docker based on the dependency. Safer to add the parent Exception class
        assertThrows(Exception.class, () ->
                new OntoBimConverter().convertOntoBIM(""));
    }


    private static Path genSampleFile(String fileExt) throws IOException {
        Path filePath = tempSecDir.resolve("data." + fileExt);
        List<String> lines = Arrays.asList("statement1", "statement2", "statement3");
        Files.write(filePath, lines);
        return filePath;
    }

}