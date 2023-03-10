package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertNotEquals;

class GeometricVoidTest {
    private static final String testBaseUri1 = "http://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "Floor_5110";
    private static final String testBaseUri2 = "http://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "Roof_138110";
    private static final String testVoidModelRepIri = testBaseUri1 + "ModelRepresentation3D_39185";
    private static final String testRepType = "Opening";
    private static final String testPlacementIRI = testBaseUri1 + "LocalPlacement_1517";

    @Test
    void testConstructor() {
        GeometricVoid sample = new GeometricVoid(testIri1, testVoidModelRepIri, testRepType, testPlacementIRI);
        GeometricVoid sample2 = new GeometricVoid(testIri2, testVoidModelRepIri, testRepType, testPlacementIRI);
        // Test that the created voids are different
        assertNotEquals(sample, sample2);
    }

    @Test
    void testAddGeometricVoidStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        GeometricVoid sample = new GeometricVoid(testIri1, testVoidModelRepIri, testRepType, testPlacementIRI);
        // Execute method
        sample.addGeometricVoidStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
    }

    private List<String> genExpectedStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testIri1 + ", http://www.theworldavatar.com/kg/ontobim/hasVoid, " + testBaseUri1 + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri1 + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/GeometricVoid");
        expected.add(testBaseUri1 + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasGeometricRepresentation, " + testVoidModelRepIri);
        expected.add(testBaseUri1 + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasVoidType, \"" + testRepType);
        expected.add(testBaseUri1 + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + testPlacementIRI);
        expected.add(testPlacementIRI + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        return expected;
    }
}