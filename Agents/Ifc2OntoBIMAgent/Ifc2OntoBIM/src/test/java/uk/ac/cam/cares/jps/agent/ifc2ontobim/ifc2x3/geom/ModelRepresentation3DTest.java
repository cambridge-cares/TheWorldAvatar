package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class ModelRepresentation3DTest {
    private static final String testBaseUri1 = "http://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcShapeRepresentation_332";
    private static final String testBaseUri2 = "http://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcWall_322";
    private static final String testSubContextIri = testBaseUri1 + "GeometricRepresentationSubContext_117";
    private static final String testGeomIri = testBaseUri1 + "FacetedBrep_9185";
    private static final String testShapeRepType = "Brep";
    private static final String testPlacementValIRI = "2517";
    private static final String testPlacementIri = testBaseUri1 + "IfcLocalPlacement_" + testPlacementValIRI;
    private static final String testBIMPlacementIRI = testBaseUri1 + "LocalPlacement_" + testPlacementValIRI;
    private static final String testTransformOperatorIri = testBaseUri1 + "CartesianTransformationOperator_515";
    private static final String testGeomClass = JunitTestUtils.bimUri + "FacetedBrep";
    private static final String testAdditionalGeomIri = testBaseUri1 + "ExtrudedAreaSolid_37216";
    private static final String testAdditionalGeomIri2 = testBaseUri1 + "PolygonalBoundedHalfSpace_59158";
    private static final String testAdditionalGeomClass = JunitTestUtils.bimUri + "ExtrudedAreaSolid";
    private static final String testAdditionalGeomClass2 = JunitTestUtils.bimUri + "PolygonalBoundedHalfSpace";

    @Test
    void testConstructor() {
        ModelRepresentation3D sample = new ModelRepresentation3D(testIri1, testSubContextIri, testGeomIri, testShapeRepType, testPlacementIri, testTransformOperatorIri);
        ModelRepresentation3D sample2 = new ModelRepresentation3D(testIri2, testSubContextIri, testGeomIri, testShapeRepType, testPlacementIri, testTransformOperatorIri);
        // Test that the created geometry representation have different IRIs
        assertNotEquals(sample.getBimIri(), sample2.getBimIri());
    }

    @Test
    void testAppendGeometry() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModelRepresentation3D sample = new ModelRepresentation3D(testIri1, testSubContextIri, testGeomIri, testShapeRepType, testPlacementIri, testTransformOperatorIri);
        // Execute method
        sample.appendGeometry(testAdditionalGeomIri);
        // Clean up results as one string
        sample.addModelRepresentation3DStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAreaSolidStatements(), result);
        // Ensure half space solid is not generated as it was not added
        JunitTestUtils.doesExpectedListNotExist(genExpectedHalfSpaceSolidStatements(), result);
    }

    @Test
    void testAppendGeometryMoreThanOnce() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModelRepresentation3D sample = new ModelRepresentation3D(testIri1, testSubContextIri, testGeomIri, testShapeRepType, testPlacementIri, testTransformOperatorIri);
        // Execute method
        sample.appendGeometry(testAdditionalGeomIri);
        sample.appendGeometry(testAdditionalGeomIri2);
        // Clean up results as one string
        sample.addModelRepresentation3DStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAreaSolidStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedHalfSpaceSolidStatements(), result);
    }

    @Test
    void testAddModelRepresentation3DStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModelRepresentation3D sample = new ModelRepresentation3D(testIri1, testSubContextIri, testGeomIri, testShapeRepType, testPlacementIri, testTransformOperatorIri);
        // Execute method
        sample.addModelRepresentation3DStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalStatements(), result);
    }

    @Test
    void testAddModelRepresentation3DStatementsNoOptionalFields() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModelRepresentation3D sample = new ModelRepresentation3D(testIri1, testSubContextIri, testGeomIri, null, null, null);
        // Execute method
        sample.addModelRepresentation3DStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalStatements(), result);
    }

    private List<String> genExpectedCommonStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/ModelRepresentation3D");
        expected.add(testBaseUri1 + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSubContext, " + testSubContextIri);
        expected.add(testSubContextIri + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/GeometricRepresentationSubContext");
        expected.add(testBaseUri1 + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + testGeomIri);
        expected.add(testGeomIri + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, " + testGeomClass);
        return expected;
    }

    private List<String> genExpectedOptionalStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationType, \"Brep");
        expected.add(testBaseUri1 + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSourcePlacement, " + testBIMPlacementIRI);
        expected.add(testBIMPlacementIRI + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(testBaseUri1 + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasTargetPlacement, " + testTransformOperatorIri);
        expected.add(testTransformOperatorIri + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/CartesianTransformationOperator");
        return expected;
    }

    private List<String> genExpectedAreaSolidStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + testAdditionalGeomIri);
        expected.add(testAdditionalGeomIri + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, " + testAdditionalGeomClass);
        return expected;
    }

    private List<String> genExpectedHalfSpaceSolidStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + testAdditionalGeomIri2);
        expected.add(testAdditionalGeomIri2 + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, " + testAdditionalGeomClass2);
        return expected;
    }
}