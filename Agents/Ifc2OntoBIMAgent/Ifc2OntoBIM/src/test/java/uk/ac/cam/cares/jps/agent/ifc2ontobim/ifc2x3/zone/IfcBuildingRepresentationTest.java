package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IfcBuildingRepresentationTest {
    private static final String testBaseUri1 = "http://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcBuildingRepresentation_142";
    private static final String testBaseUri2 = "http://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcBuildingRepresentation_1322";
    private static final String testClassName = "IfcBuildingRepresentation";
    private static final String testName = "Building Host";
    private static final String testUID = "0192auje134";
    private static final String testPlacementIriVal = "3131";
    private static final String testPlacementIri = testBaseUri1 + "IfcLocalPlacement_" + testPlacementIriVal;
    private static final String testBimPlacementIri = testBaseUri1 + "LocalPlacement_" + testPlacementIriVal;
    private static final String testBimPlacementIri2 = testBaseUri2 + "LocalPlacement_" + testPlacementIriVal;
    private static final String testProjectIri = testBaseUri1 + "IfcProjectRepresentation_ba12";
    private static final String testSiteIri = testBaseUri1 + "Site_531";
    private static final Double testRefElev1 = 125.0;
    private static final Double testRefElev2 = 125.15;
    private static final Double testTerElev1 = 396.0;
    private static final Double testTerElev2 = 396.24;
    // IfcOwl generates double values in this format
    private static final String testRefElevation1 = testRefElev1 + " .";
    private static final String testRefElevation2 = testRefElev2 + " .";
    private static final String testTerElevation1 = testTerElev1 + " .";
    private static final String testTerElevation2 = testTerElev2 + " .";


    @Test
    void testConstructor() {
        // First constructor
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testIri1, testName, testUID, testPlacementIri, testProjectIri, testSiteIri, testRefElev1.toString(), testTerElev1.toString());
        // Test that the sample fields are correct
        assertEquals(testBaseUri1, sample.getPrefix());
        assertNotEquals(testIri1, sample.getIri());
        assertTrue(sample.getIri().contains(testBaseUri1 + testClassName + "_"));
        assertEquals(testName, sample.getName());
        assertEquals(testUID, sample.getUid());
        assertEquals(testBimPlacementIri, sample.getPlacementIri());
        assertEquals(testRefElev1, sample.getRefElevation());
        assertEquals(testTerElev1, sample.getTerElevation());
        assertTrue(sample.getBotBuildingIRI().contains(sample.getPrefix() + "Building_"));
        // Second constructor
        IfcBuildingRepresentation sample2 = new IfcBuildingRepresentation(testIri2, testName, testUID, testPlacementIri, null, testSiteIri, testRefElev1.toString(), testTerElev1.toString());
        // Test that the sample fields are correct
        assertEquals(testBaseUri2, sample2.getPrefix());
        assertNotEquals(testIri2, sample2.getIri());
        assertTrue(sample2.getIri().contains(testBaseUri2 + testClassName + "_"));
        assertEquals(testName, sample2.getName());
        assertEquals(testUID, sample2.getUid());
        assertEquals(testBimPlacementIri2, sample2.getPlacementIri());
        assertEquals(testRefElev1, sample2.getRefElevation());
        assertEquals(testTerElev1, sample2.getTerElevation());
        assertTrue(sample2.getBotBuildingIRI().contains(sample2.getPrefix() + "Building_"));
    }

    @Test
    void testConstructorRefElevation() {
        IfcBuildingRepresentation sample1 = new IfcBuildingRepresentation(testIri1, testName, testUID, testPlacementIri, null, testSiteIri, testRefElev1.toString(), testTerElev1.toString());
        IfcBuildingRepresentation sample2 = new IfcBuildingRepresentation(testIri1, testName, testUID, testPlacementIri, null, testSiteIri, testRefElevation1, testTerElevation1);
        IfcBuildingRepresentation sample3 = new IfcBuildingRepresentation(testIri1, testName, testUID, testPlacementIri, null, testSiteIri, testRefElev2.toString(), testTerElev2.toString());
        IfcBuildingRepresentation sample4 = new IfcBuildingRepresentation(testIri1, testName, testUID, testPlacementIri, null, testSiteIri, testRefElevation2, testTerElevation2);
        // Test that the sample fields are correct
        assertEquals(testRefElev1, sample1.getRefElevation());
        assertEquals(testRefElev1, sample2.getRefElevation());
        assertEquals(testRefElev2, sample3.getRefElevation());
        assertEquals(testRefElev2, sample4.getRefElevation());
        assertEquals(testTerElev1, sample1.getTerElevation());
        assertEquals(testTerElev1, sample2.getTerElevation());
        assertEquals(testTerElev2, sample3.getTerElevation());
        assertEquals(testTerElev2, sample4.getTerElevation());
    }

    @Test
    void testConstructStatementsNoRefElev() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testIri1, testName, testUID, testPlacementIri, testProjectIri, testSiteIri, null, testTerElev1.toString());
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Ensure that there is more than one statement generated
        assertFalse(sampleSet.size() == 0);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedProjectStatement(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedTerrainElevationStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedElevationStatements(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedRefElevationStatements(), result);
    }

    @Test
    void testConstructStatementsNoTerElev() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testIri1, testName, testUID, testPlacementIri, testProjectIri, testSiteIri, testRefElev1.toString(), null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Ensure that there is more than one statement generated
        assertFalse(sampleSet.size() == 0);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedRefElevationStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedElevationStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedProjectStatement(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedTerrainElevationStatements(), result);
    }

    @Test
    void testConstructStatementsNoOptionalValues() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testIri1, testName, testUID, testPlacementIri, null, testSiteIri, null, null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results to a string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        // Verify these statements are not generated
        JunitTestUtils.doesExpectedListNotExist(genExpectedProjectStatement(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedRefElevationStatements(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedElevationStatements(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedTerrainElevationStatements(), result);
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testIri1, testName, testUID, testPlacementIri, testProjectIri, testSiteIri, testRefElev1.toString(), testTerElev1.toString());
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedProjectStatement(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedRefElevationStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedTerrainElevationStatements(), result);
    }

    private List<String> genExpectedCommonStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://w3id.org/bot#Building");
        expected.add(testBaseUri1 + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + testBaseUri1 + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri1 + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/IfcBuildingRepresentation");
        expected.add(testBaseUri1 + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + testName);
        expected.add(testBaseUri1 + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + testUID);
        expected.add(testSiteIri + ", https://w3id.org/bot#hasBuilding, " + testBaseUri1 + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri1 + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + testBimPlacementIri);
        return expected;
    }

    private List<String> genExpectedProjectStatement() {
        List<String> expected = new ArrayList<>();
        expected.add(testProjectIri + ", http://www.theworldavatar.com/kg/ontobim/hasRootZone, " + testBaseUri1 + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }

    private List<String> genExpectedElevationStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Height");
        expected.add(testBaseUri1 + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue, " + testBaseUri1 + "Measure_");
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Measure");
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit, " + testBaseUri1 + "Length_");
        expected.add(testBaseUri1 + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Length");
        expected.add(testBaseUri1 + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2004/02/skos/core#notation, \"m\"");
        return expected;
    }

    private List<String> genExpectedRefElevationStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + testBaseUri1 + "Height_");
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + testRefElev1);
        return expected;
    }

    private List<String> genExpectedTerrainElevationStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasTerrainElevation, " + testBaseUri1 + "Height_");
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + testTerElev1);
        return expected;
    }
}