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

class PolylineTest {
    private static final String TEST_BASE_URI = "http://www.example.org/";
    private static final String IFC_POLYLINE = "IfcPolyline";
    private static final String BIM_POLYLINE = "Polyline";
    private static final String IFC_LINE_VERTEX = "IfcCartesianPoint_List";
    private static final String BIM_LINE_VERTEX = "LineVertex";
    private static final String BIM_POINT = "CartesianPoint";
    private static final String TEST_INSTANCE = TEST_BASE_URI + IFC_POLYLINE + "_5195";
    private static final String TEST_BIM_INSTANCE = TEST_BASE_URI + BIM_POLYLINE + "_5195";
    private static final String TEST_STARTING_VERTEX = TEST_BASE_URI + IFC_LINE_VERTEX + "_5681";
    private static final String TEST_SEC_VERTEX = TEST_BASE_URI + IFC_LINE_VERTEX + "_5682";
    private static final String TEST_THIRD_VERTEX = TEST_BASE_URI + IFC_LINE_VERTEX + "_5683";
    private static final String TEST_FORTH_VERTEX = TEST_BASE_URI + IFC_LINE_VERTEX + "_5684";
    private static final String TEST_BIM_STARTING_VERTEX = TEST_BASE_URI + BIM_LINE_VERTEX + "_5681";
    private static final String TEST_BIM_SEC_VERTEX = TEST_BASE_URI + BIM_LINE_VERTEX + "_5682";
    private static final String TEST_BIM_THIRD_VERTEX = TEST_BASE_URI + BIM_LINE_VERTEX + "_5683";
    private static final String TEST_BIM_FORTH_VERTEX = TEST_BASE_URI + BIM_LINE_VERTEX + "_5684";
    private static final String TEST_STARTING_POINT = TEST_BASE_URI + BIM_POINT + "_5685";
    private static final String TEST_SEC_POINT = TEST_BASE_URI + BIM_POINT + "_5686";
    private static final String TEST_THIRD_POINT = TEST_BASE_URI + BIM_POINT + "_5687";
    private static final String TEST_FORTH_POINT = TEST_BASE_URI + BIM_POINT + "_5688";

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
        Polyline sample = new Polyline(TEST_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, TEST_SEC_VERTEX);
        Polyline sample2 = new Polyline(TEST_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, null);
        // Test that the created geometry objects are different
        assertNotEquals(sample, sample2);
    }

    @Test
    void testConstructStatementsForOneVertex() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        Polyline sample = new Polyline(TEST_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Write statements as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedCommonPolylineStatements(TEST_BIM_INSTANCE, TEST_BIM_STARTING_VERTEX, TEST_STARTING_POINT), result);
    }

    @Test
    void testConstructStatementsMissingNextVertex() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        Polyline sample = new Polyline(TEST_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, TEST_SEC_VERTEX);
        // Assert the right error and message are thrown
        IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class, () -> sample.constructStatements(sampleSet));
        assertEquals("Detected a next line vertex! But its contents does not exist for " + TEST_BIM_STARTING_VERTEX, thrownError.getMessage());
    }

    @Test
    void testAppendVertexForTwoVertex() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        Polyline sample = new Polyline(TEST_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, TEST_SEC_VERTEX);
        // Execute method
        sample.appendVertex(TEST_SEC_VERTEX, TEST_SEC_POINT, null);
        // Construct the statements to test if it works properly
        sample.constructStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedCommonPolylineStatements(TEST_BIM_INSTANCE, TEST_BIM_STARTING_VERTEX, TEST_STARTING_POINT), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedNextLineVertexStatements(TEST_BIM_STARTING_VERTEX, TEST_BIM_SEC_VERTEX, TEST_SEC_POINT), result);
        // Verify that the second vertex does not have a hasNextVertex statement
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedNextLineVertexStatements(TEST_BIM_SEC_VERTEX, TEST_BIM_THIRD_VERTEX, TEST_THIRD_POINT), result);
    }

    @Test
    void testAppendVertexForMultipleVertex() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        Polyline sample = new Polyline(TEST_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, TEST_SEC_VERTEX);
        // Execute method
        sample.appendVertex(TEST_SEC_VERTEX, TEST_SEC_POINT, TEST_THIRD_VERTEX);
        sample.appendVertex(TEST_THIRD_VERTEX, TEST_THIRD_POINT, TEST_FORTH_VERTEX);
        sample.appendVertex(TEST_FORTH_VERTEX, TEST_FORTH_POINT, null);
        // Construct the statements to test if it works properly
        sample.constructStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedCommonPolylineStatements(TEST_BIM_INSTANCE, TEST_BIM_STARTING_VERTEX, TEST_STARTING_POINT), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedNextLineVertexStatements(TEST_BIM_STARTING_VERTEX, TEST_BIM_SEC_VERTEX, TEST_SEC_POINT), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedNextLineVertexStatements(TEST_BIM_SEC_VERTEX, TEST_BIM_THIRD_VERTEX, TEST_THIRD_POINT), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedNextLineVertexStatements(TEST_BIM_THIRD_VERTEX, TEST_BIM_FORTH_VERTEX, TEST_FORTH_POINT), result);
    }
}