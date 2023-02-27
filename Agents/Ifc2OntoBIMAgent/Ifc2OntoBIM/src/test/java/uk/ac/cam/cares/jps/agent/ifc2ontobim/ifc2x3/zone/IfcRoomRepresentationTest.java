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
    private static final String testClassName = "IfcRoomRepresentation";
    private static final String testName = "Kitchen";
    private static final String testUID = "lu1276r9a";
    private static final String testStoreyIri = testBaseUri1 + "Storey_531";


    @Test
    void testConstructor() {
        // First constructor
        IfcRoomRepresentation sample = new IfcRoomRepresentation(testIri1, testName, testUID, testStoreyIri);
        // Test that the sample fields are correct
        assertEquals(testBaseUri1, sample.getPrefix());
        assertNotEquals(testIri1, sample.getIri());
        assertTrue(sample.getIri().contains(testBaseUri1 + testClassName + "_"));
        assertEquals(testName, sample.getName());
        assertEquals(testUID, sample.getUid());
        assertTrue(sample.getBimRoomIRI().contains(sample.getPrefix() + "Room_"));
        // Second constructor
        IfcRoomRepresentation sample2 = new IfcRoomRepresentation(testIri2, testName, testUID, testStoreyIri);
        // Test that the sample fields are correct
        assertEquals(testBaseUri2, sample2.getPrefix());
        assertNotEquals(testIri2, sample2.getIri());
        assertTrue(sample2.getIri().contains(testBaseUri2 + testClassName + "_"));
        assertEquals(testName, sample2.getName());
        assertEquals(testUID, sample2.getUid());
        assertTrue(sample2.getBimRoomIRI().contains(sample2.getPrefix() + "Room_"));
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcRoomRepresentation sample = new IfcRoomRepresentation(testIri1, testName, testUID, testStoreyIri);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
    }

    private List<String> genExpectedStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testStoreyIri + ", http://www.theworldavatar.com/kg/ontobim/hasRoom, " + testBaseUri1 + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri1 + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/Room");
        expected.add(testBaseUri1 + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + testBaseUri1 + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri1 + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/IfcRoomRepresentation");
        expected.add(testBaseUri1 + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + testName);
        expected.add(testBaseUri1 + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + testUID);
        return expected;
    }
}