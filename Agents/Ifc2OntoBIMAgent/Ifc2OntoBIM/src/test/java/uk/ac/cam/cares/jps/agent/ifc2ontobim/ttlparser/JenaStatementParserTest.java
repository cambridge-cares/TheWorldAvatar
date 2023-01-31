package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.VCARD;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class JenaStatementParserTest {
    private static LinkedHashSet<Statement> testSet;
    private static Set<String> ignoreSet;
    private static Map<String, String> nsMap;
    private static final String baseURI = "http://example.com/";
    private static final String listURI = "https://w3id.org/list#";

    private static final String janeURI = "JaneDoe";
    private static final String janeFullName = "Jane Doe";
    private static final String personClass = "person";
    @BeforeAll
    static void setup() {
        genIgnoreSet();
        genInverseNSMap();
    }

    private static void genIgnoreSet() {
        ignoreSet = new HashSet<>();
        ignoreSet.add("person");
    }

    private static void genInverseNSMap() {
        nsMap = new HashMap<>();
        nsMap.put(VCARD.getURI(), "vcard");
        nsMap.put(RDF.getURI(), "rdf");
        nsMap.put("http://www.theworldavatar.com/ontology/ontobim/ontoBIM#", "bim");
        nsMap.put(baseURI, "eg");
    }

    @BeforeEach
    void genSampleStatements() {
        Model sampleModel = ModelFactory.createDefaultModel();
        sampleModel.createResource(baseURI + janeURI)
                .addProperty(VCARD.FN, janeFullName)
                .addProperty(RDF.type, sampleModel.createResource(baseURI + personClass));
        sampleModel.createResource(baseURI + "IfcSite_123")
                .addProperty(RDF.type, sampleModel.createResource(baseURI + "IfcSite"));
        sampleModel.createResource(baseURI + "IfcDoor_123")
                .addProperty(RDF.type, sampleModel.createResource(baseURI + "IfcDoor"));
        sampleModel.createResource(baseURI + "IfcCartesianPoint_List_3523")
                .addProperty(RDF.type, sampleModel.createResource(baseURI + "LineVertex"))
                .addProperty(sampleModel.createProperty(listURI+"hasNext"),sampleModel.createResource(baseURI + "IfcCartesianPoint_List_5555"))
                .addProperty(sampleModel.createProperty(listURI+"hasContents"),sampleModel.createResource(baseURI + "IfcCartesianPoint_222"));
        addStatementsToSet(sampleModel);
    }

    @Test
    void testParseTriples() {
        String result = JenaStatementParser.parseTriples(testSet, nsMap).toString();
        Model sampleModel = ModelFactory.createDefaultModel();
        sampleModel.createProperty(listURI+"hasContents");
        assertAll(
                () -> assertTrue(result.contains("eg:" + janeURI + " vcard:FN \"" + janeFullName)),
                () -> assertTrue(result.contains("eg:" + janeURI + " rdf:type eg:person")),
                // Ensure that all statements have been parsed without any ignored
                () -> assertTrue(testSet.isEmpty()),
                // Ensure IFC instance name are replaced
                () -> assertTrue(result.contains("eg:Site_123 rdf:type eg:Site")),
                () -> assertTrue(result.contains("eg:Element_123 rdf:type eg:IfcDoor")),
                () -> assertTrue(result.contains("eg:LineVertex_3523 rdf:type eg:LineVertex")),
                // Ensure List predicates are renamed
                () -> assertTrue(result.contains("eg:LineVertex_3523 bim:hasNextVertex eg:LineVertex_5555")),
                () -> assertTrue(result.contains("eg:LineVertex_3523 bim:hasRefPoint eg:CartesianPoint_222"))
        );
    }

    @Test
    void testParseTriplesIgnoreSet() {
        String result = JenaStatementParser.parseTriples(testSet, nsMap, ignoreSet).toString();
        assertAll(
                () -> assertTrue(result.contains("eg:" + janeURI + " vcard:FN \"" + janeFullName)),
                // Ensure the ignore statements are ignored and remain in testSet
                () -> assertFalse(testSet.isEmpty())
        );
    }

    private void addStatementsToSet (Model sampleModel){
        testSet = new LinkedHashSet<>();
        StmtIterator iter = sampleModel.listStatements();
        while (iter.hasNext()) {
            Statement stmt = iter.nextStatement();
            testSet.add(stmt);
        }
    }
}