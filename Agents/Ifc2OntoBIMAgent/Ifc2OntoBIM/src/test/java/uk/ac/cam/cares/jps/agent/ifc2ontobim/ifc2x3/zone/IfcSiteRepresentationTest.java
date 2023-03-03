package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.apache.jena.base.Sys;
import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class IfcSiteRepresentationTest {
    private static final String testBaseUri1 = "http://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcSiteRepresentation_142";
    private static final String testBaseUri2 = "http://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcSiteRepresentation_1322";
    private static final String testClassName = "IfcSiteRepresentation";
    private static final String testName = "Free land";
    private static final String testUID = "zh163";
    private static final String testPlacementIri = testBaseUri1+ "LocalPlacement_541";
    private static final String testProjectIri = testBaseUri1 + "IfcProjectRepresentation_091383";
    private static Queue<String> testLatitude;
    private static final Double testLatDegree = 10.21;
    private static final Double testLatMinute = 10.0;
    private static final Double testLatSec= 1.35;
    private static final Double testLatMilSec = 0.0;
    private static Queue<String> testLongitude;
    private static final Double testLongDegree = 21.5;
    private static final Double testLongMinute = 20.0;
    private static final Double testLongSec= 3.15;
    private static final Double testLongMilSec = 5.6;
    private static final Double testRefElev1 = 125.0;
    private static final Double testRefElev2 = 125.15;
    // IfcOwl generates double values in this format
    private static final String testRefElevation1 = testRefElev1 + " .";
    private static final String testRefElevation2 = testRefElev2 + " .";
    // Error message
    private static final String INVALID_LATITUDE_PARAM_ERROR = "Invalid latitude input. There should be four values!";
    private static final String INVALID_LONGITUDE_PARAM_ERROR = "Invalid longitude input. There should be four values!";
    @BeforeEach
    void setup(){
        testLatitude = new ArrayDeque<>();
        testLatitude.offer(testLatDegree.toString());
        testLatitude.offer(testLatMinute.toString());
        testLatitude.offer(testLatSec.toString());
        testLatitude.offer(testLatMilSec.toString());
        testLongitude = new ArrayDeque<>();
        testLongitude.offer(testLongDegree.toString());
        testLongitude.offer(testLongMinute.toString());
        testLongitude.offer(testLongSec.toString());
        testLongitude.offer(testLongMilSec.toString());
    }


    @Test
    void testSuperConstructor() {
        // First constructor
        IfcSiteRepresentation sample = new IfcSiteRepresentation(testIri1, testName, testUID, testPlacementIri, testProjectIri, null, null, testRefElev1.toString());
        // Test that the sample fields are correct
        assertEquals(testBaseUri1, sample.getPrefix());
        assertNotEquals(testIri1, sample.getIri());
        assertTrue(sample.getIri().contains(testBaseUri1 + testClassName + "_"));
        assertEquals(testName, sample.getName());
        assertEquals(testUID, sample.getUid());
        assertEquals(testPlacementIri, sample.getPlacementIri());
        assertEquals(testRefElev1, sample.getRefElevation());
        // Second constructor
        IfcSiteRepresentation sample2 = new IfcSiteRepresentation(testIri2, testName, testUID, testPlacementIri, null, null, null, null);
        // Test that the sample fields are correct
        assertEquals(testBaseUri2, sample2.getPrefix());
        assertNotEquals(testIri2, sample2.getIri());
        assertTrue(sample2.getIri().contains(testBaseUri2 + testClassName + "_"));
        assertEquals(testName, sample2.getName());
        assertEquals(testUID, sample2.getUid());
        assertEquals(testPlacementIri, sample2.getPlacementIri());
    }

    @Test
    void testConstructorNoOptionalParams() {
        IfcSiteRepresentation sample = new IfcSiteRepresentation(testIri1, testName, testUID, testPlacementIri, null, null, null,null);
        // Test that the sample fields are correct
        assertEquals(testBaseUri1, sample.getPrefix());
        assertTrue(sample.getIri().contains(testBaseUri1 + testClassName + "_"));
        assertEquals(testName, sample.getName());
        assertEquals(testUID, sample.getUid());
        assertEquals(testPlacementIri, sample.getPlacementIri());
        // Test that no reference elevation is initialised
        assertNull(sample.getRefElevation());
    }

    @Test
    void testConstructorRefElevation() {
        IfcSiteRepresentation sample1 = new IfcSiteRepresentation(testIri1, testName, testUID, testPlacementIri, null, null, null, testRefElev1.toString());
        IfcSiteRepresentation sample2 = new IfcSiteRepresentation(testIri1, testName, testUID, testPlacementIri, null, null, null, testRefElevation1);
        IfcSiteRepresentation sample3 = new IfcSiteRepresentation(testIri1, testName, testUID, testPlacementIri, null, null, null, testRefElev2.toString());
        IfcSiteRepresentation sample4 = new IfcSiteRepresentation(testIri1, testName, testUID, testPlacementIri, null, null, null, testRefElevation2);
        // Test that the sample fields are correct
        assertEquals(testRefElev1, sample1.getRefElevation());
        assertEquals(testRefElev1, sample2.getRefElevation());
        assertEquals(testRefElev2, sample3.getRefElevation());
        assertEquals(testRefElev2, sample4.getRefElevation());
    }

    @Test
    void testConstructorInvalidLat() {
        // Empty latitude queue
        IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class, () ->
                new IfcSiteRepresentation(testIri1, testName, testUID, testPlacementIri, null, new ArrayDeque<>(), testLongitude, testRefElev1.toString()));
        assertEquals(INVALID_LATITUDE_PARAM_ERROR, thrownError.getMessage());
        // Less than 4 values in latitude queue
        Queue<String> incompleteLatitude = new ArrayDeque<>();
        incompleteLatitude.offer(testLatDegree.toString());
        IllegalArgumentException thrownError2 = assertThrows(IllegalArgumentException.class, () ->
                new IfcSiteRepresentation(testIri1, testName, testUID, testPlacementIri, null, incompleteLatitude, testLongitude, testRefElev1.toString()));
        assertEquals(INVALID_LATITUDE_PARAM_ERROR, thrownError2.getMessage());
    }

    @Test
    void testConstructorInvalidLong() {
        Queue<String> excessLongitude = new ArrayDeque<>(testLongitude);
        excessLongitude.offer("51");
        IllegalArgumentException thrownError = assertThrows(IllegalArgumentException.class, () ->
                new IfcSiteRepresentation(testIri1, testName, testUID, testPlacementIri, null, testLatitude, excessLongitude, testRefElev1.toString()));
        assertEquals(INVALID_LONGITUDE_PARAM_ERROR, thrownError.getMessage());
    }

    @Test
    void testConstructStatementsNoRefElev() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcSiteRepresentation sample = new IfcSiteRepresentation(testIri1, testName, testUID, testPlacementIri, testProjectIri, testLatitude, testLongitude,null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedLatLongStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedProjectStatement(), result);
        // Check that the below statements does not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedElevationStatements(), result);
    }

    @Test
    void testConstructStatementsNoOptionalParam() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcSiteRepresentation sample = new IfcSiteRepresentation(testIri1, testName, testUID, testPlacementIri, null, null, null,null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        // Check that the below statements does not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedProjectStatement(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedLatLongStatements(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedElevationStatements(), result);
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcSiteRepresentation sample = new IfcSiteRepresentation(testIri1, testName, testUID, testPlacementIri, testProjectIri, testLatitude, testLongitude, testRefElev1.toString());
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedProjectStatement(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedLatLongStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedElevationStatements(), result);

    }

    private List<String> genExpectedCommonStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "Site_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://w3id.org/bot#Site");
        expected.add(testBaseUri1 + "Site_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/IfcSiteRepresentation");
        expected.add(testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + testName);
        expected.add(testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + testUID);
        expected.add(testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + testPlacementIri);
        expected.add(testPlacementIri + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        return expected;
    }

    private List<String> genExpectedProjectStatement() {
        List<String> expected = new ArrayList<>();
        expected.add(testProjectIri + ", http://www.theworldavatar.com/kg/ontobim/hasRootZone, " + testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }

    private List<String> genExpectedLatLongStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefLatitude, " + testBaseUri1 + "Latitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri1 + "Latitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/CompoundPlaneAngle");
        expected.add(testBaseUri1 + "Latitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasDegree, \"" + testLatDegree);
        expected.add(testBaseUri1 + "Latitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasMinute, \"" + testLatMinute);
        expected.add(testBaseUri1 + "Latitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSecond, \"" + testLatSec);
        expected.add(testBaseUri1 + "Latitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasMillionthSecond, \"" + testLatMilSec);
        expected.add(testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefLongitude, " + testBaseUri1 + "Longitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri1 + "Longitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasDegree, \"" + testLongDegree);
        expected.add(testBaseUri1 + "Longitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasMinute, \"" + testLongMinute);
        expected.add(testBaseUri1 + "Longitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSecond, \"" + testLongSec);
        expected.add(testBaseUri1 + "Longitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasMillionthSecond, \"" + testLongMilSec);
        return expected;
    }

    private List<String> genExpectedElevationStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + testBaseUri1 + "Height_");
        expected.add(testBaseUri1 + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Height");
        expected.add(testBaseUri1 + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue, " + testBaseUri1 + "Measure_");
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Measure");
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + testRefElev1);
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit, " + testBaseUri1 + "Length_");
        expected.add(testBaseUri1 + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Length");
        expected.add(testBaseUri1 + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2004/02/skos/core#notation, \"m\"");
        return expected;
    }
}