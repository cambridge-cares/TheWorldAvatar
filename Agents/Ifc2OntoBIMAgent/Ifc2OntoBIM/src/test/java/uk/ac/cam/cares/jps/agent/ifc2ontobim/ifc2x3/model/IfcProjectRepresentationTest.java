package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.GeometricRepresentationContext;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.IfcProjectRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IfcProjectRepresentationTest {
    private static final String testBaseUri = "https://www.example.org/";
    private static final String testClassName = "IfcProjectRepresentation";
    private static final String testName = "Boulevard";
    private static final String testPhase = "Construction";
    private static final String testGeomRepContext = testBaseUri + "IfcGeometricRepresentationContext_142";
    private static final String testBimGeomRepContext = testBaseUri + "GeometricRepresentationContext_142";
    private static final String testWCSIri = testBaseUri + "LocalPlacement_2531";
    private static final String testTrueNorthIri = testBaseUri + "DirectionVector_5513";
    private static final Double testDim = 2.0;
    private static final Double testPrecision = 10.3;
    private static GeometricRepresentationContext testContext;

    @BeforeAll
    static void setup(){
        NamespaceMapper.setBaseNameSpace(testBaseUri);
        testContext = new GeometricRepresentationContext(testGeomRepContext,
                testDim.toString(), testPrecision.toString(), testWCSIri, testTrueNorthIri);
    }
    
    @AfterAll
    static void resetNamespace(){
        NamespaceMapper.setBaseNameSpace("");
    }
    
    @Test
    void testConstructor() {
        // First constructor
        IfcProjectRepresentation sample = new IfcProjectRepresentation(testName, testPhase, testContext);
        // Test that the sample fields are correct
        assertTrue(sample.getIri().contains(testBaseUri + testClassName + "_"));
        // Second constructor
        IfcProjectRepresentation sample2 = new IfcProjectRepresentation(testName, testPhase, testContext);
        // Test that the sample fields are correct
        assertTrue(sample2.getIri().contains(testBaseUri + testClassName + "_"));
    }

    @Test
    void constructStatementsNoOptionalValues() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcProjectRepresentation sample = new IfcProjectRepresentation(null, null, testContext);
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
        IfcProjectRepresentation sample = new IfcProjectRepresentation(testName, testPhase, testContext);
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
        expected.add(testBaseUri + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDF_TYPE + ", https://www.theworldavatar.com/kg/ontobim/IfcProjectRepresentation");
        expected.add(testBaseUri + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasContext, " + testBimGeomRepContext);
        return expected;
    }

    private List<String> genExpectedOptionalStatements() {
        java.util.List<java.lang.String> expected = new ArrayList<>();
        expected.add(testBaseUri + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDFS_LABEL + ", \"" + testName);
        expected.add(testBaseUri + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasPhase, \"" + testPhase);
        return expected;
    }
}