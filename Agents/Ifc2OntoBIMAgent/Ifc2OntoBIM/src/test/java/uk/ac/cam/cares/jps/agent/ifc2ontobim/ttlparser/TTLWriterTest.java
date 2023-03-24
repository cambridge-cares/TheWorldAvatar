package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import org.apache.jena.rdf.model.*;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.VCARD;
import org.junit.jupiter.api.*;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class TTLWriterTest {
    private static LinkedHashSet<Statement> testSet;
    private static Set<String> ignoreSet;
    private static Map<String, String> nsMap;
    private static final String baseURI = "http://example.com/";
    private static final String janeURI = "JaneDoe";
    private static final String janeFullName = "Jane Doe";
    private static final String personClass = "person";
    private static Path tempFile;
    private static Model tempModel;


    @BeforeAll
    static void setup() {
        genIgnoreSet();
        genNSMap();
    }

    @BeforeEach
    void genSampleStatements() {
        tempModel = ModelFactory.createDefaultModel();
        tempModel.createResource(baseURI + janeURI)
                .addProperty(VCARD.FN, janeFullName)
                .addProperty(RDF.type, tempModel.createResource(baseURI + personClass));

        testSet = new LinkedHashSet<>();
        StmtIterator iter = tempModel.listStatements();
        while (iter.hasNext()) {
            Statement stmt = iter.nextStatement();
            testSet.add(stmt);
        }
    }

    @Test
    void testTTLWriterConstructor() {
        assertNotNull(new TTLWriter(nsMap));
    }

    @Test
    void testWriteIntermediateTempFile() throws FileNotFoundException {
        // Execute method
        tempFile = new TTLWriter(nsMap).writeIntermediateTempFile(testSet);
        // Read the contents of the file and verify it matches the expected data
        InputStream inputStream = new FileInputStream(tempFile.toString());
        Model model = ModelFactory.createDefaultModel();
        RDFDataMgr.read(model, inputStream, Lang.TTL);
        assertTrue(model.containsAll(tempModel));
    }

    @Test
    void testWriteTTL() {
        tempFile = IOHelper.createTempFile();
        new TTLWriter(nsMap).writeTTL(testSet, tempFile, new HashMap<>());
        List<String> contents = IOHelper.readFile(tempFile);
        List<String> expected = genFinalResults();
        assertAll(
                () -> assertEquals(expected.get(0), contents.get(0)),
                () -> assertEquals(expected.get(1), contents.get(1)),
                () -> assertEquals(expected.get(2), contents.get(2)),
                () -> assertEquals(expected.get(3), contents.get(3)),
                () -> assertEquals(expected.get(4), contents.get(4)),
                () -> assertEquals(expected.get(5), contents.get(5)),
                () -> assertEquals(expected.get(6), contents.get(6))
        );
    }

    @Test
    void testWriteTTLWithIntermediateFiles() {
        List<Path> tempFilePaths = genTwoIntermediateFiles();
        tempFile = IOHelper.createTempFile();
        Map<String, String> emptyMap = new HashMap<>();
        TTLWriter writer = new TTLWriter(nsMap);
        writer.writeTTLWithIntermediateFiles(tempFilePaths, tempFile, emptyMap);

        List<String> contents = IOHelper.readFile(tempFile);
        List<String> expected = genFinalResults();
        assertAll(
                () -> assertFalse(Files.exists(tempFilePaths.get(0))),
                () -> assertFalse(Files.exists(tempFilePaths.get(1))),
                () -> assertEquals(expected.get(0), contents.get(0)),
                () -> assertEquals(expected.get(1), contents.get(1)),
                () -> assertEquals(expected.get(2), contents.get(2)),
                () -> assertEquals(expected.get(3), contents.get(3)),
                () -> assertEquals(expected.get(4), contents.get(4)),
                () -> assertEquals(expected.get(7), contents.get(5)),
                () -> assertEquals(expected.get(8), contents.get(7))
        );
    }

    @AfterEach
    void cleanUpEach() {
        if (tempFile != null) {
            IOHelper.deleteTempFile(tempFile);
        }
    }

    private static void genIgnoreSet() {
        ignoreSet = new HashSet<>();
        ignoreSet.add("person");
    }

    private static void genNSMap() {
        nsMap = new HashMap<>();
        nsMap.put("vcard", VCARD.getURI());
        nsMap.put("rdf", RDF.getURI());
        nsMap.put("eg", baseURI);
    }

    private static List<String> genIntermediateFileResults() {
        List<String> results = new ArrayList<>();
        results.add("eg:" + janeURI + " rdf:type eg:" + personClass);
        results.add("eg:" + janeURI + " vcard:FN \"" + janeFullName + "\"");
        return results;
    }

    private static List<String> genFinalResults() {
        List<String> results = new ArrayList<>();
        results.add("@prefix eg: <" + baseURI + "> . ");
        results.add("@prefix rdf: <" + RDF.getURI() + "> . ");
        results.add("@prefix vcard: <" + VCARD.getURI() + "> . ");
        results.add("");
        results.add("");
        // Lines for writeTTL()
        results.add("eg:" + janeURI + " rdf:type eg:" + personClass + ";");
        results.add("\tvcard:FN \"" + janeFullName + "\".");
        // Lines for writeTTLWithIntermediateFiles()
        results.add("eg:" + janeURI + " vcard:FN \"" + janeFullName + "\".");
        results.add("eg:" + janeURI + " rdf:type eg:" + personClass + ".");
        return results;
    }

    private static List<Path> genTwoIntermediateFiles() {
        List<Path> tempFilePaths = new ArrayList<>();
        Path tempFile1 = IOHelper.createTempFile();
        Path tempFile2 = IOHelper.createTempFile();
        String result1 = "eg:" + janeURI + " vcard:FN \"" + janeFullName + "\"";
        String result2 = "eg:" + janeURI + " rdf:type eg:" + personClass;
        IOHelper.writeLinesToFile(tempFile1, result1);
        IOHelper.writeLinesToFile(tempFile2, result2);
        tempFilePaths.add(tempFile1);
        tempFilePaths.add(tempFile2);
        return tempFilePaths;
    }
}