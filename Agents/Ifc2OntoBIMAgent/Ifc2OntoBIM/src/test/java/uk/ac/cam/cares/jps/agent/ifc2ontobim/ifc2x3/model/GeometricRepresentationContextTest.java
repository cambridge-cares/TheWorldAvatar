package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class GeometricRepresentationContextTest {
    private static final String testBaseUri1 = "https://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcGeometricRepresentationContext_142";
    private static final String testBIMIri1 = testBaseUri1 + "GeometricRepresentationContext_142";
    private static final String testBaseUri2 = "https://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcGeometricRepresentationContext_1322";
    private static final String testBIMIri2 = testBaseUri2 + "GeometricRepresentationContext_1322";
    private static final String testClassName = "GeometricRepresentationContext";
    private static final String testWCSIri = testBaseUri1 + "LocalPlacement_2531";
    private static final String testTrueNorthIri = testBaseUri1 + "DirectionVector_5513";
    private static final Double testDim = 2.0;
    private static final Double testPrecision = 10.3;

    @BeforeEach
    void createNamespace(){
        NamespaceMapper.setBaseNameSpace(testBaseUri1);
    }

    @AfterAll
    static void resetNamespace(){
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testConstructor() {
        // First constructor
        GeometricRepresentationContext sample = new GeometricRepresentationContext(testIri1, testDim.toString(), testPrecision.toString(), testWCSIri, testTrueNorthIri);
        // Test that the sample fields are correct
        assertEquals(testBIMIri1, sample.getIri());
        // Second constructor
        NamespaceMapper.setBaseNameSpace(testBaseUri2);
        GeometricRepresentationContext sample2 = new GeometricRepresentationContext(testIri2, testDim.toString(), testPrecision.toString(), testWCSIri, testTrueNorthIri);
        // Test that the sample fields are correct
        assertEquals(testBIMIri2, sample2.getIri());
    }

    @Test
    void constructStatementsNoOptionalValues() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        GeometricRepresentationContext sample = new GeometricRepresentationContext(testIri1, testDim.toString(), null, testWCSIri, null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        // Verify that optional statements are not generated
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalStatements(), result);
    }

    @Test
    void constructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        GeometricRepresentationContext sample = new GeometricRepresentationContext(testIri1, testDim.toString(), testPrecision.toString(), testWCSIri, testTrueNorthIri);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalStatements(), result);
    }

    private List<String> genExpectedStatements() {
        java.util.List<java.lang.String> expected = new ArrayList<>();
        expected.add(testBIMIri1 + ", " + JunitTestUtils.RDF_TYPE + ", https://www.theworldavatar.com/kg/ontobim/GeometricRepresentationContext");
        expected.add(testBIMIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasSpaceDimensions, \"" + testDim);
        expected.add(testBIMIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasWorldCoordinateSystem, " + testWCSIri);
        return expected;
    }

    private List<String> genExpectedOptionalStatements() {
        java.util.List<java.lang.String> expected = new ArrayList<>();
        expected.add(testBIMIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasPrecision, \"" + testPrecision);
        expected.add(testBIMIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasTrueNorth, " + testTrueNorthIri);
        return expected;
    }
}