package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class IfcAbstractRepresentationTest {
    private static final String testBaseUri1 = "http://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcAbstractRepresentation_142";
    private static final String testClassName1 = "IfcSiteRepresentation";
    private static final String testName1 = "Free land";
    private static final String testUID1 = "afi193";
    private static final String testPlacementIri1 = testBaseUri1+ "LocalPlacement_324";
    private static final String testBaseUri2 = "http://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcAbstractRepresentation_142";
    private static final String testClassName2 = "IfcStoreyRepresentation";
    private static final String testName2 = "First floor";
    private static final String testUID2 = "b18aqw1";
    private static final String testPlacementIri2 = testBaseUri2 + "LocalPlacement_5120";

    @Test
    void testConstructor() {
        IfcAbstractRepresentation sample = new IfcAbstractRepresentation(testIri1, testClassName1, testName1, testUID1, testPlacementIri1);
        // Test that the sample fields are correct
        assertEquals(testBaseUri1, sample.getPrefix());
        assertNotEquals(testIri1, sample.getIri());
        assertTrue(sample.getIri().contains(testBaseUri1 + testClassName1 + "_"));
        assertEquals(testName1, sample.getName());
        assertEquals(testUID1, sample.getUid());
        assertEquals(testPlacementIri1, sample.getPlacementIri());

        IfcAbstractRepresentation sample2 = new IfcAbstractRepresentation(testIri2, testClassName2, testName2, testUID2, testPlacementIri2);
        // Test that the sample fields are correct
        assertEquals(testBaseUri2, sample2.getPrefix());
        assertNotEquals(testIri2, sample2.getIri());
        assertTrue(sample2.getIri().contains(testBaseUri2 + testClassName2 + "_"));
        assertEquals(testName2, sample2.getName());
        assertEquals(testUID2, sample2.getUid());
        assertEquals(testPlacementIri2, sample2.getPlacementIri());
    }
}