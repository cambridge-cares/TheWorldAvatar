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

class PolygonalBoundedHalfSpaceTest {
    private static final String TEST_BASE_URI = "http://www.example.org/";
    private static final String IFC_HALF_SPACE_CLASS = "IfcPolygonalBoundedHalfSpace";
    private static final String BIM_HALF_SPACE_CLASS = "PolygonalBoundedHalfSpace";
    private static final String IFC_POLYLINE_CLASS = "IfcPolyline";
    private static final String BIM_POLYLINE_CLASS = "Polyline";
    private static final String BIM_POSITION = "LocalPlacement";
    private static final String TEST_INSTANCE = TEST_BASE_URI + IFC_HALF_SPACE_CLASS + "_4461";
    private static final String TEST_BIM_INSTANCE = TEST_BASE_URI + BIM_HALF_SPACE_CLASS + "_4461";
    private static final String TEST_POSITION = TEST_BASE_URI + BIM_POSITION + "_4462";
    private static final String TEST_SURFACE_POSITION = TEST_BASE_URI + BIM_POSITION + "_4463";
    private static final String TEST_POLYLINE = TEST_BASE_URI + IFC_POLYLINE_CLASS + "_4466";
    private static final String TEST_BIM_POLYLINE = TEST_BASE_URI + BIM_POLYLINE_CLASS + "_4466";
    private static final boolean TEST_FLAG = false;

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
        PolygonalBoundedHalfSpace sample = new PolygonalBoundedHalfSpace(TEST_INSTANCE, TEST_POSITION, TEST_SURFACE_POSITION, TEST_POLYLINE, TEST_FLAG);
        PolygonalBoundedHalfSpace sample2 = new PolygonalBoundedHalfSpace(TEST_INSTANCE, TEST_POSITION, TEST_SURFACE_POSITION, TEST_POLYLINE, TEST_FLAG);
        // Test that the created geometry objects are different
        assertNotEquals(sample, sample2);
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        PolygonalBoundedHalfSpace sample = new PolygonalBoundedHalfSpace(TEST_INSTANCE, TEST_POSITION, TEST_SURFACE_POSITION, TEST_POLYLINE, TEST_FLAG);
        // Execute method
        sample.constructStatements(sampleSet);
        // Write statements as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedHalfSpaceStatements(TEST_BASE_URI, TEST_BIM_INSTANCE, TEST_POSITION, TEST_SURFACE_POSITION, TEST_BIM_POLYLINE, TEST_FLAG), result);
    }
}