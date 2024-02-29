package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestGeometryUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.LinkedHashSet;

import static org.junit.jupiter.api.Assertions.assertNotEquals;

class HalfSpaceSolidTest {
    private static final String TEST_BASE_URI = "http://www.example.org/";
    private static final String IFC_HALF_SPACE_SOLID_CLASS = "IfcHalfSpaceSolid";
    private static final String BIM_HALF_SPACE_SOLID_CLASS = "HalfSpaceSolid";
    private static final String BIM_POSITION = "LocalPlacement";
    private static final String TEST_INSTANCE = TEST_BASE_URI + IFC_HALF_SPACE_SOLID_CLASS + "_6682";
    private static final String TEST_BIM_INSTANCE = TEST_BASE_URI + BIM_HALF_SPACE_SOLID_CLASS + "_6682";
    private static final String TEST_SURFACE_POSITION = TEST_BASE_URI + BIM_POSITION + "_6686";
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
        HalfSpaceSolid sample = new HalfSpaceSolid(TEST_INSTANCE, TEST_SURFACE_POSITION, TEST_FLAG);
        HalfSpaceSolid sample2 = new HalfSpaceSolid(TEST_INSTANCE, TEST_SURFACE_POSITION, TEST_FLAG);
        // Test that the created geometry objects are different
        assertNotEquals(sample, sample2);
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        HalfSpaceSolid sample = new HalfSpaceSolid(TEST_INSTANCE, TEST_SURFACE_POSITION, TEST_FLAG);
        // Execute method
        sample.constructStatements(sampleSet);
        // Write statements as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedHalfSpaceSolidStatements(TEST_BASE_URI, TEST_BIM_INSTANCE, BIM_HALF_SPACE_SOLID_CLASS, TEST_SURFACE_POSITION, TEST_FLAG), result);
    }
}