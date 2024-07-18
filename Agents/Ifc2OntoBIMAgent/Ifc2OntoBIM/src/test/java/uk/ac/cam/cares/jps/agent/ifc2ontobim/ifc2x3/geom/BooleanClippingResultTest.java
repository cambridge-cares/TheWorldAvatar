package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestGeometryUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.LinkedHashSet;

import static org.junit.jupiter.api.Assertions.assertNotEquals;

class BooleanClippingResultTest {
    private static final String TEST_BASE_URI_1 = "http://www.example.org/";
    private static final String TEST_IRI_1 = TEST_BASE_URI_1 + "IfcBooleanClippingResult_874";
    private static final String TEST_BIM_IRI_1 = TEST_BASE_URI_1 + "BooleanClippingResult_874";
    private static final String TEST_BASE_URI_2 = "http://www.example.org/test#";
    private static final String TEST_IRI_2 = TEST_BASE_URI_2 + "IfcBooleanClippingResult_6424";
    private static final String TEST_OPERATOR = JunitTestUtils.ifc2x3Uri + "DIFFERENCE";
    private static final String TEST_FIRST_GEOM = TEST_BASE_URI_1 + "IfcPolygonalBoundedHalfSpace_892";
    private static final String TEST_SEC_GEOM = TEST_BASE_URI_1 + "IfcPolyline_996";
    private static final String TEST_NESTED_BOOLEAN_OPERATOR = TEST_BASE_URI_1 + "IfcBooleanClippingResult_1002";
    private static final String TEST_BIM_FIRST_GEOM = TEST_BASE_URI_1 + "PolygonalBoundedHalfSpace_892";
    private static final String TEST_BIM_SEC_GEOM = TEST_BASE_URI_1 + "Polyline_996";
    private static final String TEST_BIM_NESTED_BOOLEAN_OPERATOR = TEST_BASE_URI_1 + "BooleanClippingResult_1002";

    @BeforeEach
    void createNamespace() {
        NamespaceMapper.setBaseNameSpace(TEST_BASE_URI_1);
    }

    @AfterAll
    static void resetNamespace() {
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testConstructor() {
        BooleanClippingResult sample = new BooleanClippingResult(TEST_IRI_1, TEST_OPERATOR, TEST_FIRST_GEOM, TEST_SEC_GEOM);
        BooleanClippingResult sample2 = new BooleanClippingResult(TEST_IRI_2, TEST_OPERATOR, TEST_FIRST_GEOM, TEST_SEC_GEOM);
        // Test that the created samples are different
        assertNotEquals(sample, sample2);
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BooleanClippingResult sample = new BooleanClippingResult(TEST_IRI_1, TEST_OPERATOR, TEST_FIRST_GEOM, TEST_SEC_GEOM);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedBooleanClippingResultStatements(TEST_BIM_IRI_1, TEST_OPERATOR, TEST_BIM_FIRST_GEOM, TEST_BIM_SEC_GEOM), result);
    }

    @Test
    void testConstructStatementsForNestedClippingResultOperand() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BooleanClippingResult sample = new BooleanClippingResult(TEST_IRI_1, TEST_OPERATOR, TEST_FIRST_GEOM, TEST_NESTED_BOOLEAN_OPERATOR);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedBooleanClippingResultStatements(TEST_BIM_IRI_1, TEST_OPERATOR, TEST_BIM_FIRST_GEOM, TEST_BIM_NESTED_BOOLEAN_OPERATOR), result);
    }
}