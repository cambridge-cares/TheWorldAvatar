package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class IfcAbstractRepresentationTest {
    private static final String testBaseUri1 = "http://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcAbstractRepresentation_142";
    private static final String testBaseUri2 = "http://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcAbstractRepresentation_142";

    @Test
    void testConstructor() {
        IfcAbstractRepresentation sample = new IfcAbstractRepresentation(testIri1);
        // Test that the sample fields are correct
        assertEquals(testIri1, sample.getIri());
        assertEquals(testBaseUri1, sample.getPrefix());

        IfcAbstractRepresentation sample2 = new IfcAbstractRepresentation(testIri2);
        // Test that the sample fields are correct
        assertEquals(testIri2, sample2.getIri());
        assertEquals(testBaseUri2, sample2.getPrefix());
    }
}