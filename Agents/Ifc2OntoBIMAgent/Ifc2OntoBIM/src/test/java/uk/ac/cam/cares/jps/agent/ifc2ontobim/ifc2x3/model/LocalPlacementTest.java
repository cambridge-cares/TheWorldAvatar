package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class LocalPlacementTest {
    private static final String testBaseUri1 = "https://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcLocalPlacement_512";
    private static final String testBimIri1 = testBaseUri1 + "LocalPlacement_512";
    private static final String testBaseUri2 = "https://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcLocalPlacement_1322";
    private static final String testBimIri2 = testBaseUri2 + "LocalPlacement_1322";
    private static final String testClassName = "LocalPlacement";
    private static final String testRefPoint = testBaseUri1 + "CartesianPoint_18232";
    private static final String testRefDirection = testBaseUri1 + "DirectionVector_1922";
    private static final String testAxisDirection = testBaseUri1 + "DirectionVector_3516";
    private static final String testRelPosition = testBaseUri1 + "LocalPlacement_4576";

    @BeforeEach
    void createNamespace(){ NamespaceMapper.setBaseNameSpace(testBaseUri1); }
    @AfterAll
    static void resetNamespace(){
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testConstructor() {
        // First constructor
        LocalPlacement sample = new LocalPlacement(testIri1, testRefPoint, testRefDirection, testAxisDirection, testRelPosition);
        // Test that the sample fields are correct
        assertEquals(testBimIri1, sample.getIri());
        // Second constructor
        NamespaceMapper.setBaseNameSpace(testBaseUri2);
        LocalPlacement sample2 = new LocalPlacement(testIri2, testRefPoint, testRefDirection, testAxisDirection, null);
        assertEquals(testBimIri2, sample2.getIri());
    }

    @Test
    void testAlternateConstructor() {
        LocalPlacement sample = new LocalPlacement(testRefPoint, testRefDirection, testAxisDirection);
        // Test that the sample fields are correct
        assertTrue(sample.getIri().contains(testBaseUri1 + testClassName + "_"));
    }

    @Test
    void constructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        LocalPlacement sample = new LocalPlacement(testIri1, testRefPoint, testRefDirection, testAxisDirection, testRelPosition);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(testBaseUri1), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalStatements(testBaseUri1), result);
    }

    @Test
    void constructStatementsOptionalValues() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        LocalPlacement sample = new LocalPlacement(testIri1, testRefPoint, null, null, null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(testBaseUri1), result);
        // Verify the z coordinate statement is not generated
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalStatements(testBaseUri1), result);
    }

    private List<String> genExpectedStatements(String baseURI) {
        List<String> expected = new ArrayList<>();
        expected.add(baseURI + "LocalPlacement_512, " + JunitTestUtils.RDF_TYPE + ", https://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(baseURI + "LocalPlacement_512, https://www.theworldavatar.com/kg/ontobim/hasRefPoint, " + testRefPoint);
        return expected;
    }

    private List<String> genExpectedOptionalStatements(String baseURI) {
        List<String> expected = new ArrayList<>();
        expected.add(baseURI + "LocalPlacement_512, https://www.theworldavatar.com/kg/ontobim/hasRefDirection, " + testRefDirection);
        expected.add(baseURI + "LocalPlacement_512, https://www.theworldavatar.com/kg/ontobim/hasAxisDirection, " + testAxisDirection);
        expected.add(baseURI + "LocalPlacement_512, https://www.theworldavatar.com/kg/ontobim/hasRelativePositionTo, " + testRelPosition);
        return expected;
    }
}