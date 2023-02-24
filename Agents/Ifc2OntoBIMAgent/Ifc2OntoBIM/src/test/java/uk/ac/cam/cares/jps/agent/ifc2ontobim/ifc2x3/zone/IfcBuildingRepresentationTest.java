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
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testIri1, testSiteIri, testRefElev1.toString(), testTerElev1.toString());
        // Test that the sample fields are correct
        assertEquals(testIri1, sample.getIri());
        assertEquals(testBaseUri1, sample.getPrefix());
        assertEquals(testRefElev1, sample.getRefElevation());
        assertEquals(testTerElev1, sample.getTerElevation());
        assertTrue(sample.getBotBuildingIRI().contains(sample.getPrefix() + "Building_"));
        // Second constructor
        IfcBuildingRepresentation sample2 = new IfcBuildingRepresentation(testIri2, testSiteIri, testRefElev1.toString(), testTerElev1.toString());
        // Test that the sample fields are correct
        assertEquals(testIri2, sample2.getIri());
        assertEquals(testBaseUri2, sample2.getPrefix());
        assertEquals(testRefElev1, sample.getRefElevation());
        assertEquals(testTerElev1, sample.getTerElevation());
        assertTrue(sample2.getBotBuildingIRI().contains(sample2.getPrefix() + "Building_"));
    }

    @Test
    void testConstructorRefElevation() {
        IfcBuildingRepresentation sample1 = new IfcBuildingRepresentation(testIri1, testSiteIri, testRefElev1.toString(), testTerElev1.toString());
        IfcBuildingRepresentation sample2 = new IfcBuildingRepresentation(testIri1, testSiteIri, testRefElevation1, testTerElevation1);
        IfcBuildingRepresentation sample3 = new IfcBuildingRepresentation(testIri1, testSiteIri, testRefElev2.toString(), testTerElev2.toString());
        IfcBuildingRepresentation sample4 = new IfcBuildingRepresentation(testIri1, testSiteIri, testRefElevation2, testTerElevation2);
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
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testIri1, testSiteIri, null, testTerElev1.toString());
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Ensure that there is more than one statement generated
        assertFalse(sampleSet.size() == 0);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedTerrainElevationStatements(), result);
    }

    @Test
    void testConstructStatementsNoTerElev() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testIri1, testSiteIri, testRefElev1.toString(), null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Ensure that there is more than one statement generated
        assertFalse(sampleSet.size() == 0);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedRefElevationStatements(), result);
    }

    @Test
    void testConstructStatementsNoRefAndTerElev() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testIri1, testSiteIri, null, null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results to a string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testIri1, testSiteIri, testRefElev1.toString(), testTerElev1.toString());
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedRefElevationStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedTerrainElevationStatements(), result);
    }

    private List<String> genExpectedCommonStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://w3id.org/bot#Building");
        expected.add(testBaseUri1 + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + testIri1);
        expected.add(testSiteIri + ", https://w3id.org/bot#hasBuilding, " + testBaseUri1 + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }

    private List<String> genExpectedRefElevationStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testIri1 + ", http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + testBaseUri1 + "Height_");
        expected.add(testBaseUri1 + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Height");
        expected.add(testBaseUri1 + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue, " + testBaseUri1 + "Measure_");
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Measure");
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + testRefElev1);
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit, " + testBaseUri1 + "Length_");
        expected.add(testBaseUri1 + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Length");
        expected.add(testBaseUri1 + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2004/02/skos/core#notation, \"m\"");
        return expected;
    }

    private List<String> genExpectedTerrainElevationStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testIri1 + ", http://www.theworldavatar.com/kg/ontobim/hasTerrainElevation, " + testBaseUri1 + "Height_");
        expected.add(testBaseUri1 + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Height");
        expected.add(testBaseUri1 + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue, " + testBaseUri1 + "Measure_");
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Measure");
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + testTerElev1);
        expected.add(testBaseUri1 + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit, " + testBaseUri1 + "Length_");
        expected.add(testBaseUri1 + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Length");
        expected.add(testBaseUri1 + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2004/02/skos/core#notation, \"m\"");
        return expected;
    }
}