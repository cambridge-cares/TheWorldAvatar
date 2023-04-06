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

import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CartesianTransformationOperatorTest {
    private static final String testBaseUri = "https://www.example.org/";
    private static final String testIri1 = testBaseUri + "IfcCartesianTransformationOperator_39122";
    private static final String testBimIri1 = testBaseUri + "CartesianTransformationOperator_39122";
    private static final String testOriginIri = testBaseUri + "CartesianPoint_51172";
    private static final String testXDirection = testBaseUri + "DirectionVector_12095";
    private static final String testYDirection = testBaseUri + "DirectionVector_330826";
    private static final Double testScaleFactor = 1.185;

    @BeforeEach
    void createNamespace(){ NamespaceMapper.setBaseNameSpace(testBaseUri); }
    @AfterAll
    static void resetNamespace(){
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testConstructor() {
        CartesianTransformationOperator sample = new CartesianTransformationOperator(testIri1, testOriginIri, testScaleFactor.toString(), testXDirection, testYDirection);
        CartesianTransformationOperator sample2 = new CartesianTransformationOperator(testIri1, testOriginIri, testScaleFactor.toString(), testXDirection, testYDirection);
        CartesianTransformationOperator sample3 = new CartesianTransformationOperator(testIri1, testOriginIri, null, null, null);
        // Test that none of the sample objects are equal
        // Same values but variables are generated differently
        assertNotEquals(sample, sample2);
        assertNotEquals(sample, sample3);
        assertNotEquals(sample2, sample3);
    }

    @Test
    void constructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        CartesianTransformationOperator sample = new CartesianTransformationOperator(testIri1, testOriginIri, testScaleFactor.toString(), testXDirection, testYDirection);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalStatements(), result);
    }

    @Test
    void constructStatementsOptionalValues() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        CartesianTransformationOperator sample = new CartesianTransformationOperator(testIri1, testOriginIri, null, null, null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        // Verify the z coordinate statement is not generated
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalStatements(), result);
    }

    private List<String> genExpectedStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBimIri1 + ", https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/CartesianTransformationOperator");
        expected.add(testBimIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasLocalOrigin, " + testOriginIri);
        return expected;
    }

    private List<String> genExpectedOptionalStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBimIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasScale, \"" + testScaleFactor);
        expected.add(testBimIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasDerivedXAxis, " + testXDirection);
        expected.add(testBimIri1 + ", https://www.theworldavatar.com/kg/ontobim/hasDerivedYAxis, " + testYDirection);
        return expected;
    }
}