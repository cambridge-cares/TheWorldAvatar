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
import java.util.LinkedHashSet;

import static org.junit.jupiter.api.Assertions.*;

class IOHelperTest {

    @Test
    void testCreateTempFile() {
        Path file = IOHelper.createTempFile();
        assertTrue(Files.exists(file));
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
        // Execute method
        Path filePath = IOHelper.writeIntermediateTempFile(statements);
        // Read the contents of the file and verify it matches the expected data
        InputStream inputStream = new FileInputStream(filePath.toString());
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, inputStream, Lang.TTL);
        assertTrue(model.containsAll(expected));
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
