package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import org.mockito.stubbing.Answer;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class OntoBimConverterTest {
    @TempDir
    private static Path tempDir;
    private static Path sampleTtl;

    @BeforeAll
    static void init() throws IOException {
        sampleTtl = tempDir.resolve("test.ttl");
        List<String> lines = new ArrayList<>();
        lines.add("@prefix rdf: <" + JunitTestUtils.rdfUri + "> .");
        lines.add("@prefix bim: <" + JunitTestUtils.bimUri + "> .");
        lines.add("@prefix inst: <" + JunitTestUtils.bimUri + "> .");
        lines.add("");
        lines.add("bim:Zone_19 rdf:type bim:Zone .");
        Files.write(sampleTtl, lines);
    }

    @Test
    void testConvertOntoBIM() {
        OntoBimConverter converter = new OntoBimConverter();
        try (MockedStatic<AccessClient> mockAccessClient = Mockito.mockStatic(AccessClient.class)) {
            // Stub the method to do nothing when called
            mockAccessClient.when(() -> AccessClient.uploadStatements(Mockito.anyString(), Mockito.any())).thenAnswer((Answer<Void>) invocation -> null);
            List<Path> sampleFilePaths = converter.convertOntoBIM(sampleTtl.toString(), true);
            assertEquals(0, sampleFilePaths.size());
        }
    }
}