package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class StairFlightTest {
    private static final String testBaseUri1 = "http://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "StairFlight_1242";
    private static final String testName1 = "Round flights";
    private static final String testUID1 = "vsd15da";
    private static final String testPlacementIri1 = testBaseUri1 + "LocalPlacement_515";
    private static final String testBaseUri2 = "http://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "StairFlight_122";
    private static final String testName2 = "Concrete stair run";
    private static final String testUID2 = "e1re2sv32";
    private static final String testPlacementIri2 = testBaseUri2 + "LocalPlacement_8172";
    private static final String testHostStairIRI = testBaseUri1 + "Stair_67152";
    private static final String testGeomModelIRI = testBaseUri1 + "ModelRepresentation3D_715";
    private static final String testClassName = "IfcModelRepresentation";
    private static final String testClass = JunitTestUtils.bimUri + testClassName;
    private static final Integer testNoOfRiser = 12;
    private static final Integer testNoOfTread = 6;
    private static final Double testRiserHeight = 0.158;
    private static final Double testTreadLength = 1.29;

    @Test
    void testConstructor() {
        StairFlight sample = new StairFlight(testIri1, testName1, testUID1, testPlacementIri1, testHostStairIRI, testGeomModelIRI,
                testNoOfRiser.toString(), testNoOfTread.toString(), testRiserHeight.toString(), testTreadLength.toString());
        // Test that the sample fields are correct
        assertEquals(testBaseUri1, sample.getPrefix());
        assertNotEquals(testIri1, sample.getIfcRepIri());
        assertTrue(sample.getIfcRepIri().contains(testBaseUri1 + testClassName + "_"));
        assertEquals(testName1, sample.getName());
        assertEquals(testUID1, sample.getUid());
        assertEquals(testPlacementIri1, sample.getPlacementIri());

        StairFlight sample2 = new StairFlight(testIri2, testName2, testUID2, testPlacementIri2, testHostStairIRI, testGeomModelIRI,
                testNoOfRiser.toString(), testNoOfTread.toString(), testRiserHeight.toString(), testTreadLength.toString());
        // Test that the sample fields are correct
        assertEquals(testBaseUri2, sample2.getPrefix());
        assertNotEquals(testIri2, sample2.getIfcRepIri());
        assertTrue(sample2.getIfcRepIri().contains(testBaseUri2 + testClassName + "_"));
        assertEquals(testName2, sample2.getName());
        assertEquals(testUID2, sample2.getUid());
        assertEquals(testPlacementIri2, sample2.getPlacementIri());
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        StairFlight sample = new StairFlight(testIri1, testName1, testUID1, testPlacementIri1, testHostStairIRI, testGeomModelIRI,
                testNoOfRiser.toString(), testNoOfTread.toString(), testRiserHeight.toString(), testTreadLength.toString());
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
    }

    private List<String> genExpectedCommonStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testHostStairIRI + ", http://www.theworldavatar.com/kg/ontobuildingstructure/consistsOf, " + testBaseUri1 + "StairFlight_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri1 + "StairFlight_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobuildingstructure/StairFlight");
        expected.add(testBaseUri1 + "StairFlight_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/hasNumOfRiser, \"" + testNoOfRiser);
        expected.add(testBaseUri1 + "StairFlight_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/hasNumOfTread, \"" + testNoOfTread);
        expected.add(testBaseUri1 + "StairFlight_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/hasRiserHeight, \"" + testRiserHeight);
        expected.add(testBaseUri1 + "StairFlight_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/hasTreadLength, \"" + testTreadLength);
        expected.add(testBaseUri1 + "StairFlight_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + testBaseUri1 + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri1 + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, " + testClass);
        expected.add(testBaseUri1 + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + testName1);
        expected.add(testBaseUri1 + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + testUID1);
        expected.add(testBaseUri1 + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + testPlacementIri1);
        expected.add(testBaseUri1 + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasGeometricRepresentation, " + testGeomModelIRI);
        return expected;
    }
}