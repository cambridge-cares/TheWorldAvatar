package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IfcRoomRepresentationTest {
    private static final String testBaseUri1 = "http://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcRoomRepresentation_142";
    private static final String testBaseUri2 = "http://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcRoomRepresentation_1322";
    private static final String testStoreyIri = testBaseUri1 + "Storey_531";


    @Test
    void testConstructor() {
        // First constructor
        IfcRoomRepresentation sample = new IfcRoomRepresentation(testIri1, testStoreyIri);
        // Test that the sample fields are correct
        assertEquals(testIri1, sample.getIri());
        assertEquals(testBaseUri1, sample.getPrefix());
        assertTrue(sample.getBimRoomIRI().contains(sample.getPrefix() + "Room_"));
        // Second constructor
        IfcRoomRepresentation sample2 = new IfcRoomRepresentation(testIri2, testStoreyIri);
        // Test that the sample fields are correct
        assertEquals(testIri2, sample2.getIri());
        assertEquals(testBaseUri2, sample2.getPrefix());
        assertTrue(sample2.getBimRoomIRI().contains(sample2.getPrefix() + "Room_"));
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcRoomRepresentation sample = new IfcRoomRepresentation(testIri1, testStoreyIri);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
    }

    private List<String> genExpectedStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/Room");
        expected.add(testBaseUri1 + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + testIri1);
        expected.add(testStoreyIri + ", http://www.theworldavatar.com/kg/ontobim/hasRoom, " + testBaseUri1 + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }
}