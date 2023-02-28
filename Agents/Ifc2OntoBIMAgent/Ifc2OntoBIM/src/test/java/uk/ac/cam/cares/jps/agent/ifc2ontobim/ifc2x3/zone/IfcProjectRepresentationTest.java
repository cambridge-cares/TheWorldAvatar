package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IfcProjectRepresentationTest {
    private static final String testBaseUri1 = "http://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcProjectRepresentation_142";
    private static final String testBaseUri2 = "http://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcProjectRepresentation_1322";
    private static final String testClassName = "IfcProjectRepresentation";
    private static final String testName = "Boulevard";
    private static final String testPhase = "Construction";

    @Test
    void testConstructor() {
        // First constructor
        IfcProjectRepresentation sample = new IfcProjectRepresentation(testIri1, testName, testPhase);
        // Test that the sample fields are correct
        assertTrue(sample.getIri().contains(testBaseUri1 + testClassName + "_"));
        // Second constructor
        IfcProjectRepresentation sample2 = new IfcProjectRepresentation(testIri2, testName, testPhase);
        // Test that the sample fields are correct
        assertTrue(sample2.getIri().contains(testBaseUri2 + testClassName + "_"));
    }

    @Test
    void constructStatementsNoOptionalValues() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcProjectRepresentation sample = new IfcProjectRepresentation(testIri1, null, null);
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
        IfcProjectRepresentation sample = new IfcProjectRepresentation(testIri1, testName, testPhase);
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
        expected.add(testBaseUri1 + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/IfcProjectRepresentation");
        return expected;
    }

    private List<String> genExpectedOptionalStatements() {
        java.util.List<java.lang.String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + testName);
        expected.add(testBaseUri1 + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasPhase, \"" + testPhase);
        return expected;
    }
}