package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class ModelRepresentation3DTest {
    private static final String testBaseUri = "https://www.example.org/";
    private static final String testSubContextIri = testBaseUri + "GeometricRepresentationSubContext_117";
    private static final String testGeomIri = testBaseUri + "IfcFacetedBrep_9185";
    private static final String testGeomBimIri = testBaseUri + "FacetedBrep_9185";

    private static final String testShapeRepType = "Brep";
    private static final String testPlacementValIRI = "2517";
    private static final String testPlacementIri = testBaseUri + "IfcLocalPlacement_" + testPlacementValIRI;
    private static final String testBIMPlacementIRI = testBaseUri + "LocalPlacement_" + testPlacementValIRI;
    private static final String testTransformOperatorIri = testBaseUri + "IfcCartesianTransformationOperator_515";
    private static final String testBimTransformOperatorIri = testBaseUri + "CartesianTransformationOperator_515";
    private static final String testAdditionalGeomIri = testBaseUri + "IfcExtrudedAreaSolid_37216";
    private static final String testAdditionalGeomIri2 = testBaseUri + "IfcPolygonalBoundedHalfSpace_59158";
    private static final String testAdditionalGeomBimIri = testBaseUri + "ExtrudedAreaSolid_37216";
    private static final String testAdditionalGeomBimIri2 = testBaseUri + "PolygonalBoundedHalfSpace_59158";
    @BeforeAll
    static void createNamespace(){ NamespaceMapper.setBaseNameSpace(testBaseUri); }
    @AfterAll
    static void resetNamespace(){ NamespaceMapper.setBaseNameSpace("");}
    
    @Test
    void testConstructor() {
        ModelRepresentation3D sample = new ModelRepresentation3D(testSubContextIri, testGeomIri, testShapeRepType, testPlacementIri, testTransformOperatorIri);
        ModelRepresentation3D sample2 = new ModelRepresentation3D(testSubContextIri, testGeomIri, testShapeRepType, testPlacementIri, testTransformOperatorIri);
        // Test that the created geometry representation have different IRIs
        assertNotEquals(sample.getBimIri(), sample2.getBimIri());
    }

    @Test
    void testAppendGeometry() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModelRepresentation3D sample = new ModelRepresentation3D(testSubContextIri, testGeomIri, testShapeRepType, testPlacementIri, testTransformOperatorIri);
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
        ModelRepresentation3D sample = new ModelRepresentation3D(testSubContextIri, testGeomIri, testShapeRepType, testPlacementIri, testTransformOperatorIri);
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
        ModelRepresentation3D sample = new ModelRepresentation3D(testSubContextIri, testGeomIri, testShapeRepType, testPlacementIri, testTransformOperatorIri);
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
        ModelRepresentation3D sample = new ModelRepresentation3D(testSubContextIri, testGeomIri, null, null, null);
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
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDF_TYPE + ", https://www.theworldavatar.com/kg/ontobim/ModelRepresentation3D");
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasSubContext, " + testSubContextIri);
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + testGeomBimIri);
        return expected;
    }

    private List<String> genExpectedOptionalStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRepresentationType, \"Brep");
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasSourcePlacement, " + testBIMPlacementIRI);
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasTargetPlacement, " + testBimTransformOperatorIri);
        return expected;
    }

    private List<String> genExpectedAreaSolidStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + testAdditionalGeomBimIri);
        return expected;
    }

    private List<String> genExpectedHalfSpaceSolidStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + testAdditionalGeomBimIri2);
        return expected;
    }
}