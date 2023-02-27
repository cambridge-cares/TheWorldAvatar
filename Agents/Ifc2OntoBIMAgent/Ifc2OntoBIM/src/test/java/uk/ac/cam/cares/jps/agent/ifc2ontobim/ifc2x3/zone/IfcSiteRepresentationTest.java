package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IfcSiteRepresentationTest {
    private static final String testBaseUri1 = "http://www.example.org/";
    private static final String testIri1 = testBaseUri1 + "IfcSiteRepresentation_142";
    private static final String testBaseUri2 = "http://www.example.org/test#";
    private static final String testIri2 = testBaseUri2 + "IfcSiteRepresentation_1322";
    private static final String testClassName = "IfcSiteRepresentation";
    private static final String testName = "Free land";
    private static final String testUID = "zh163";
    private static final Double testRefElev1 = 125.0;
    private static final Double testRefElev2 = 125.15;
    // IfcOwl generates double values in this format
    private static final String testRefElevation1 = testRefElev1 + " .";
    private static final String testRefElevation2 = testRefElev2 + " .";


    @Test
    void testSuperConstructor() {
        // First constructor
        IfcSiteRepresentation sample = new IfcSiteRepresentation(testIri1, testName, testUID, testRefElev1.toString());
        // Test that the sample fields are correct
        assertEquals(testBaseUri1, sample.getPrefix());
        assertNotEquals(testIri1, sample.getIri());
        assertTrue(sample.getIri().contains(testBaseUri1 + testClassName + "_"));
        assertEquals(testName, sample.getName());
        assertEquals(testUID, sample.getUid());
        assertEquals(testRefElev1, sample.getRefElevation());
        // Second constructor
        IfcSiteRepresentation sample2 = new IfcSiteRepresentation(testIri2, testName, testUID, testRefElev1.toString());
        // Test that the sample fields are correct
        assertEquals(testBaseUri2, sample2.getPrefix());
        assertNotEquals(testIri2, sample2.getIri());
        assertTrue(sample2.getIri().contains(testBaseUri2 + testClassName + "_"));
        assertEquals(testName, sample2.getName());
        assertEquals(testUID, sample2.getUid());
    }

    @Test
    void testConstructorNoRefElev() {
        IfcSiteRepresentation sample = new IfcSiteRepresentation(testIri1, testName, testUID, null);
        // Test that the sample fields are correct
        assertEquals(testBaseUri1, sample.getPrefix());
        assertTrue(sample.getIri().contains(testBaseUri1 + testClassName + "_"));
        assertEquals(testName, sample.getName());
        assertEquals(testUID, sample.getUid());
        // Test that no reference elevation is initialised
        assertNull(sample.getRefElevation());
    }

    @Test
    void testConstructorRefElevation() {
        IfcSiteRepresentation sample1 = new IfcSiteRepresentation(testIri1, testName, testUID, testRefElev1.toString());
        IfcSiteRepresentation sample2 = new IfcSiteRepresentation(testIri1, testName, testUID, testRefElevation1);
        IfcSiteRepresentation sample3 = new IfcSiteRepresentation(testIri1, testName, testUID, testRefElev2.toString());
        IfcSiteRepresentation sample4 = new IfcSiteRepresentation(testIri1, testName, testUID, testRefElevation2);
        // Test that the sample fields are correct
        assertEquals(testRefElev1, sample1.getRefElevation());
        assertEquals(testRefElev1, sample2.getRefElevation());
        assertEquals(testRefElev2, sample3.getRefElevation());
        assertEquals(testRefElev2, sample4.getRefElevation());
    }

    @Test
    void testConstructStatementsNoRefElev() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcSiteRepresentation sample = new IfcSiteRepresentation(testIri1, testName, testUID, null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcSiteRepresentation sample = new IfcSiteRepresentation(testIri1, testName, testUID, testRefElev1.toString());
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);

    }

    private List<String> genExpectedCommonStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri1 + "Site_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://w3id.org/bot#Site");
        expected.add(testBaseUri1 + "Site_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/IfcSiteRepresentation");
        expected.add(testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + testName);
        expected.add(testBaseUri1 + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + testUID);
        return expected;
    }

    private List<String> genExpectedStatements() {
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