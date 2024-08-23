package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestGeometryUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.LinkedHashSet;

import static org.junit.jupiter.api.Assertions.*;

class FacetedBrepTest {
    private static final String TEST_BASE_URI = "http://www.example.org/";
    private static final String IFC_FACETED_BREP = "IfcFacetedBrep";
    private static final String BIM_FACETED_BREP = "FacetedBrep";
    private static final String IFC_POLY_LOOP = "IfcPolyLoop";
    private static final String BIM_POLY_LOOP = "PolyLoop";
    private static final String TEST_INSTANCE = TEST_BASE_URI + IFC_FACETED_BREP + "_2004";
    private static final String TEST_BIM_INSTANCE = TEST_BASE_URI + BIM_FACETED_BREP + "_2004";
    private static final String TEST_FIRST_FACE_BOUNDARY_LOOP_INSTANCE = TEST_BASE_URI + IFC_POLY_LOOP + "_2185";
    private static final String TEST_FIRST_FACE_BOUNDARY_LOOP_BIM_INSTANCE = TEST_BASE_URI + BIM_POLY_LOOP + "_2185";
    private static final String TEST_SEC_FACE_BOUNDARY_LOOP_INSTANCE = TEST_BASE_URI + IFC_POLY_LOOP + "_2201";
    private static final String TEST_SEC_FACE_BOUNDARY_LOOP_BIM_INSTANCE = TEST_BASE_URI + BIM_POLY_LOOP + "_2201";
    private static final String TEST_THIRD_FACE_BOUNDARY_LOOP_INSTANCE = TEST_BASE_URI + IFC_POLY_LOOP + "_2234";
    private static final String TEST_THIRD_FACE_BOUNDARY_LOOP_BIM_INSTANCE = TEST_BASE_URI + BIM_POLY_LOOP + "_2234";
    private static final boolean TEST_FIRST_NON_INVERSED_ORIENTATION_INDICATOR = true;
    private static final boolean TEST_SEC_NON_INVERSED_ORIENTATION_INDICATOR = false;
    private static final boolean TEST_THIRD_NON_INVERSED_ORIENTATION_INDICATOR = true;


    @BeforeAll
    static void createNamespace() {
        NamespaceMapper.setBaseNameSpace(TEST_BASE_URI);
    }

    @AfterAll
    static void resetNamespace() {
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testConstructor() {
        FacetedBrep sample = new FacetedBrep(TEST_INSTANCE, TEST_FIRST_FACE_BOUNDARY_LOOP_INSTANCE, TEST_FIRST_NON_INVERSED_ORIENTATION_INDICATOR);
        FacetedBrep sample2 = new FacetedBrep(TEST_INSTANCE, TEST_SEC_FACE_BOUNDARY_LOOP_INSTANCE, TEST_SEC_NON_INVERSED_ORIENTATION_INDICATOR);
        // Test that the created geometry objects are different
        assertNotEquals(sample, sample2);
    }

    @Test
    void testConstructStatementsForOneFace() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        FacetedBrep sample = new FacetedBrep(TEST_INSTANCE, TEST_FIRST_FACE_BOUNDARY_LOOP_INSTANCE, TEST_FIRST_NON_INVERSED_ORIENTATION_INDICATOR);
        // Execute method
        sample.constructStatements(sampleSet);
        // Write statements as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepStatements(TEST_BASE_URI, TEST_BIM_INSTANCE), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepFaceStatements(TEST_BASE_URI, TEST_FIRST_FACE_BOUNDARY_LOOP_BIM_INSTANCE, TEST_FIRST_NON_INVERSED_ORIENTATION_INDICATOR), result);
    }

    @Test
    void testAppendFaceForTwoFaces() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        FacetedBrep sample = new FacetedBrep(TEST_INSTANCE, TEST_FIRST_FACE_BOUNDARY_LOOP_INSTANCE, TEST_FIRST_NON_INVERSED_ORIENTATION_INDICATOR);
        // Execute method
        sample.appendFace(TEST_SEC_FACE_BOUNDARY_LOOP_INSTANCE, TEST_SEC_NON_INVERSED_ORIENTATION_INDICATOR);
        // Construct the statements to test if it works properly
        sample.constructStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepStatements(TEST_BASE_URI, TEST_BIM_INSTANCE), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepFaceStatements(TEST_BASE_URI, TEST_FIRST_FACE_BOUNDARY_LOOP_BIM_INSTANCE, TEST_FIRST_NON_INVERSED_ORIENTATION_INDICATOR), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepFaceStatements(TEST_BASE_URI, TEST_SEC_FACE_BOUNDARY_LOOP_BIM_INSTANCE, TEST_SEC_NON_INVERSED_ORIENTATION_INDICATOR), result);
    }

    @Test
    void testAppendFaceForMultipleFaces() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        FacetedBrep sample = new FacetedBrep(TEST_INSTANCE, TEST_FIRST_FACE_BOUNDARY_LOOP_INSTANCE, TEST_FIRST_NON_INVERSED_ORIENTATION_INDICATOR);
        // Execute method
        sample.appendFace(TEST_SEC_FACE_BOUNDARY_LOOP_INSTANCE, TEST_SEC_NON_INVERSED_ORIENTATION_INDICATOR);
        sample.appendFace(TEST_THIRD_FACE_BOUNDARY_LOOP_BIM_INSTANCE, TEST_THIRD_NON_INVERSED_ORIENTATION_INDICATOR);
        // Construct the statements to test if it works properly
        sample.constructStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepStatements(TEST_BASE_URI, TEST_BIM_INSTANCE), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepFaceStatements(TEST_BASE_URI, TEST_FIRST_FACE_BOUNDARY_LOOP_BIM_INSTANCE, TEST_FIRST_NON_INVERSED_ORIENTATION_INDICATOR), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepFaceStatements(TEST_BASE_URI, TEST_SEC_FACE_BOUNDARY_LOOP_BIM_INSTANCE, TEST_SEC_NON_INVERSED_ORIENTATION_INDICATOR), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepFaceStatements(TEST_BASE_URI, TEST_THIRD_FACE_BOUNDARY_LOOP_BIM_INSTANCE, TEST_THIRD_NON_INVERSED_ORIENTATION_INDICATOR), result);
    }
}