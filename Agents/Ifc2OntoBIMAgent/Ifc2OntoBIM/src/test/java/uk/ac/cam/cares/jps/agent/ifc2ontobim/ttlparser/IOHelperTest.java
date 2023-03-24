package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.*;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IOHelperTest {
    private final String testString = "test string";

    @Test
    void testCreateTempFile() {
        Path file = IOHelper.createTempFile();
        assertTrue(Files.exists(file));
        // Always delete the temporary file generated
        IOHelper.deleteTempFile(file);
    }

    @Test
    void testReadFile() {
        Path file = IOHelper.createTempFile();
        // If the function did not work, there would be an exception rather than an empty list
        List<String> result = IOHelper.readFile(file);
        // As no text is written, the result should be empty
        assertTrue(result.isEmpty());
        // Always delete the temporary file generated
        IOHelper.deleteTempFile(file);
    }

    @Test
    void testReadFileFail() {
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> IOHelper.readFile(Paths.get("")));
        assertTrue(thrownError.getMessage().contains(" cannot be accessed or opened!"));
    }

    @Test
    void testWriteLinesToFile() {
        Path file = IOHelper.createTempFile();
        IOHelper.writeLinesToFile(file, testString);
        List<String> result = IOHelper.readFile(file);
        assertEquals(testString, result.get(0));
        // Always delete the temporary file generated
        IOHelper.deleteTempFile(file);
    }

    @Test
    public void testWriteTempTTLFile() throws IOException {
        // Create one sample statement
        Model expected = ModelFactory.createDefaultModel();
        Statement sampleStatement = expected.createStatement(
                ModelFactory.createDefaultModel().createResource("http://example.com/subject"),
                ModelFactory.createDefaultModel().createProperty("http://example.com/predicate"),
                ModelFactory.createDefaultModel().createTypedLiteral(42));
        LinkedHashSet<Statement> statements = new LinkedHashSet<>();
        statements.add(sampleStatement);
        // Create a temporary file for testing
        Path filePath = IOHelper.createTempFile();
        // Execute method
        IOHelper.writeTempTTLFile(filePath, statements);
        // Read the contents of the file and verify it matches the expected data
        InputStream inputStream = new FileInputStream(filePath.toString());
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, inputStream, Lang.TTL);
        assertTrue(model.containsAll(expected));
    }

    @Test
    void testReplaceTargetFileWithSource() {
        Path src = IOHelper.createTempFile();
        Path tgt = IOHelper.createTempFile();
        IOHelper.writeLinesToFile(src, testString);
        IOHelper.replaceTargetFileWithSource(src, tgt);

        // Test outcomes
        List<String> result = IOHelper.readFile(tgt);
        assertFalse(Files.exists(src)); // Src file should be deleted in the function
        assertEquals(testString, result.get(0)); // Tgt file should have src file's contents
        IOHelper.deleteTempFile(tgt); // Delete the remaining file
    }

    @Test
    void testDeleteTempFile() {
        Path file = IOHelper.createTempFile();
        IOHelper.deleteTempFile(file);
        assertFalse(Files.exists(file));
    }

    @Test
    void testDeleteTempFileFail() {
        assertThrows(JPSRuntimeException.class, () -> IOHelper.deleteTempFile(Paths.get("")));
    }
}
