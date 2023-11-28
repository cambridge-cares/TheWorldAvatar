package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.TTLWriter;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class OntoBimConverterTest {
    @TempDir
    private static Path tempDir;
    private static Path sampleTtl;

    @BeforeAll
    static void init() throws IOException {
        sampleTtl = tempDir.resolve("test.ttl");
        List<String> lines = new ArrayList<>();
        lines.add("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .");
        lines.add("@prefix bim: <http://www.theworldavatar.com/ontology/ontobim/ontoBIM#> .");
        lines.add("");
        lines.add("bim:Zone_19 rdf:type bim:Zone .");
        Files.write(sampleTtl, lines);
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
}