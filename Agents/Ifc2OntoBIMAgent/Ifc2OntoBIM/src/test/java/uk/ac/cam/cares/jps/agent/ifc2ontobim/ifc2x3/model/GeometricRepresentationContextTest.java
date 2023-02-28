package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcProjectRepresentation;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class GeometricRepresentationContextTest {
    private static final String testBaseUri1 = "http://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcGeometricRepresentationContext_142";
    private static final String testBaseUri2 = "http://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcGeometricRepresentationContext_1322";
    private static final String testClassName = "GeometricRepresentationContext";
    private static final String testWCSIri = testBaseUri1 + "LocalPlacement_2531";
    private static final String testTrueNorthIri = testBaseUri1 + "DirectionVector_5513";
    private static final Double testDim = 2.0;
    private static final Double testPrecision = 10.3;

    @Test
    void testConstructor() {
        // First constructor
        GeometricRepresentationContext sample = new GeometricRepresentationContext(testIri1, testDim.toString(), testPrecision.toString(), testWCSIri, testTrueNorthIri);
        // Test that the sample fields are correct
        assertTrue(sample.getIri().contains(testBaseUri1 + testClassName + "_"));
        // Second constructor
        GeometricRepresentationContext sample2 = new GeometricRepresentationContext(testIri2, testDim.toString(), testPrecision.toString(), testWCSIri, testTrueNorthIri);        // Test that the sample fields are correct
        assertTrue(sample2.getIri().contains(testBaseUri2 + testClassName + "_"));
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
        expected.add(testBaseUri1 + "GeometricRepresentationContext_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/GeometricRepresentationContext");
        expected.add(testBaseUri1 + "GeometricRepresentationContext_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSpaceDimensions, \"" + testDim);
        expected.add(testBaseUri1 + "GeometricRepresentationContext_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasWorldCoordinateSystem, " + testWCSIri);
        expected.add(testWCSIri + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        return expected;
    }

    private List<String> genExpectedOptionalStatements() {
        java.util.List<java.lang.String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "GeometricRepresentationContext_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasPrecision, \"" + testPrecision);
        expected.add(testBaseUri1 + "GeometricRepresentationContext_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasTrueNorth, " + testTrueNorthIri);
        expected.add(testTrueNorthIri + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/DirectionVector");
        return expected;
    }
}