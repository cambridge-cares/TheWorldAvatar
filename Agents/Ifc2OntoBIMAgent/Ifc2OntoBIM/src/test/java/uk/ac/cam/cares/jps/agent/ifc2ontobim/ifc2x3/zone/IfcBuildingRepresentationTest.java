package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.Unit;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class IfcBuildingRepresentationTest {
    private static String elevationUnitIri;
    private static final String testBaseUri = "https://www.example.org/";
    private static final String testIri1 = testBaseUri + "IfcBuildingRepresentation_142";
    private static final String testIri2 = testBaseUri + "IfcBuildingRepresentation_1322";
    private static final String testClassName = "IfcBuildingRepresentation";
    private static final String testName = "Building Host";
    private static final String testUID = "0192auje134";
    private static final String testPlacementIriVal = "3131";
    private static final String testPlacementIri = testBaseUri + "IfcLocalPlacement_" + testPlacementIriVal;
    private static final String testBimPlacementIri = testBaseUri + "LocalPlacement_" + testPlacementIriVal;
    private static final String testBimPlacementIri2 = testBaseUri + "LocalPlacement_" + testPlacementIriVal;
    private static final String testProjectIri = testBaseUri + "IfcProjectRepresentation_ba12";
    private static final String testSiteIri = testBaseUri + "Site_531";
    private static final String[] testAddress = new String[5];
    private static final String testBlock1 = "BLK 1";
    private static final String testBlock2 = "BLK 1A";
    private static final String testBlock3 = "BLK1C";
    private static final String testBlock4 = "1";
    private static final String testBlock5 = "1C";
    private static final String testStreet1 = "Road Street";
    private static final String testStreet2 = "Lorong 20 Geylang";
    private static final String testStreetNumber = "52";
    private static final String testAddressLine = testStreet1 + " " + testStreetNumber;
    private static final String testAddressLine1 = testBlock1 + " " + testStreet1 + " " + testStreetNumber;
    private static final String testAddressLine2 = testBlock2 + " " + testStreet2;
    private static final String testAddressLine3 = testBlock3 + " " + testStreet1;
    private static final String testAddressLine4 = testBlock4 + " " + testStreet2 + " " + testStreetNumber;
    private static final String testAddressLine5 = testBlock5 + " " + testStreet1 + " " + testStreetNumber;
    private static final String testCity = "Cambridge";
    private static final String testState = "Cambridgeshire";
    private static final String testCountry = "England";
    private static final String testPostalCode = "51284";
    private static final Double testRefElev1 = 125.0;
    private static final Double testRefElev2 = 125.15;
    private static final Double testTerElev1 = 396.0;
    private static final Double testTerElev2 = 396.24;
    // IfcOwl generates double values in this format
    private static final String testRefElevation1 = testRefElev1 + " .";
    private static final String testRefElevation2 = testRefElev2 + " .";
    private static final String testTerElevation1 = testTerElev1 + " .";
    private static final String testTerElevation2 = testTerElev2 + " .";

    @BeforeAll
    static void createNamespace() {
        NamespaceMapper.setBaseNameSpace(testBaseUri);
        Unit length = new Unit(JunitTestUtils.LENGTH_CLASS, JunitTestUtils.LENGTH_SYMBOL);
        elevationUnitIri = length.getIri();
        testAddress[0] = testAddressLine;
        testAddress[1] = testCity;
        testAddress[2] = testState;
        testAddress[3] = testCountry;
        testAddress[4] = testPostalCode;
    }

    @AfterAll
    static void resetNamespace() {
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testConstructor() {
        // First constructor
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, testProjectIri, testSiteIri, testRefElev1.toString(), testTerElev1.toString(), null, testAddress);
        // Test that the sample fields are correct
        assertEquals(testBaseUri, sample.getPrefix());
        assertNotEquals(testIri1, sample.getIri());
        assertTrue(sample.getIri().contains(testBaseUri + testClassName + "_"));
        assertEquals(testName, sample.getName());
        assertEquals(testUID, sample.getUid());
        assertEquals(testBimPlacementIri, sample.getPlacementIri());
        assertEquals(testRefElev1, sample.getRefElevation());
        assertEquals(testTerElev1, sample.getTerElevation());
        assertTrue(sample.getBotBuildingIRI().contains(sample.getPrefix() + "Building_"));
        // Second constructor
        IfcBuildingRepresentation sample2 = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, null, testSiteIri, testRefElev1.toString(), testTerElev1.toString(), null, testAddress);
        // Test that the sample fields are correct
        assertEquals(testBaseUri, sample2.getPrefix());
        assertNotEquals(testIri2, sample2.getIri());
        assertTrue(sample2.getIri().contains(testBaseUri + testClassName + "_"));
        assertEquals(testName, sample2.getName());
        assertEquals(testUID, sample2.getUid());
        assertEquals(testBimPlacementIri2, sample2.getPlacementIri());
        assertEquals(testRefElev1, sample2.getRefElevation());
        assertEquals(testTerElev1, sample2.getTerElevation());
        assertTrue(sample2.getBotBuildingIRI().contains(sample2.getPrefix() + "Building_"));
    }

    @Test
    void testConstructorRefElevation() {
        IfcBuildingRepresentation sample1 = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, null, testSiteIri, testRefElev1.toString(), testTerElev1.toString(), null, testAddress);
        IfcBuildingRepresentation sample2 = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, null, testSiteIri, testRefElevation1, testTerElevation1, null, testAddress);
        IfcBuildingRepresentation sample3 = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, null, testSiteIri, testRefElev2.toString(), testTerElev2.toString(), null, testAddress);
        IfcBuildingRepresentation sample4 = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, null, testSiteIri, testRefElevation2, testTerElevation2, null, testAddress);
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
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, testProjectIri, testSiteIri, null, testTerElev1.toString(), elevationUnitIri, testAddress);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Ensure that there is more than one statement generated
        assertFalse(sampleSet.size() == 0);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAddressStatements(true), result);
        JunitTestUtils.doesExpectedListExist(genExpectedProjectStatement(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedTerrainElevationStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedElevationStatements(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedRefElevationStatements(), result);
    }

    @Test
    void testConstructStatementsNoTerElev() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, testProjectIri, testSiteIri, testRefElev1.toString(), null, elevationUnitIri, testAddress);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Ensure that there is more than one statement generated
        assertFalse(sampleSet.size() == 0);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAddressStatements(true), result);
        JunitTestUtils.doesExpectedListExist(genExpectedRefElevationStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedElevationStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedProjectStatement(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedTerrainElevationStatements(), result);
    }

    @Test
    void testConstructStatementsNoOptionalValues() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, null, testSiteIri, null, null, null, testAddress);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results to a string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAddressStatements(true), result);
        // Verify these statements are not generated
        JunitTestUtils.doesExpectedListNotExist(genExpectedProjectStatement(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedRefElevationStatements(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedElevationStatements(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedTerrainElevationStatements(), result);
    }

    @Test
    void testConstructStatementsPermutationsAddressLines() {
        testAddress[0] = testAddressLine1;
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, testProjectIri, testSiteIri, testRefElev1.toString(), testTerElev1.toString(), elevationUnitIri, testAddress);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        try {
            JunitTestUtils.doesExpectedListExist(genExpectedAddressStatements(testStreet1, testStreetNumber, testBlock1), result);
        } finally {
            // Always attempt to reset the values even if the test fail to prevent side effects
            testAddress[0] = testAddressLine;
        }
        // Test second permutation similarly
        testAddress[0] = testAddressLine2;
        sampleSet = new LinkedHashSet<>();
        sample = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, testProjectIri, testSiteIri, testRefElev1.toString(), testTerElev1.toString(), elevationUnitIri, testAddress);
        sample.constructStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        try {
            JunitTestUtils.doesExpectedListExist(genExpectedAddressStatements(testStreet2, "", testBlock2), result);
        } finally {
            testAddress[0] = testAddressLine;
        }
        // Test third permutation similarly
        testAddress[0] = testAddressLine3;
        sampleSet = new LinkedHashSet<>();
        sample = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, testProjectIri, testSiteIri, testRefElev1.toString(), testTerElev1.toString(), elevationUnitIri, testAddress);
        sample.constructStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        try {
            JunitTestUtils.doesExpectedListExist(genExpectedAddressStatements(testStreet1, "", testBlock3), result);
        } finally {
            testAddress[0] = testAddressLine;
        }
        // Test forth permutation similarly
        testAddress[0] = testAddressLine4;
        sampleSet = new LinkedHashSet<>();
        sample = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, testProjectIri, testSiteIri, testRefElev1.toString(), testTerElev1.toString(), elevationUnitIri, testAddress);
        sample.constructStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        try {
            JunitTestUtils.doesExpectedListExist(genExpectedAddressStatements(testStreet2, testStreetNumber, testBlock4), result);
        } finally {
            testAddress[0] = testAddressLine;
        }
        // Test fifth permutation similarly
        testAddress[0] = testAddressLine5;
        sampleSet = new LinkedHashSet<>();
        sample = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, testProjectIri, testSiteIri, testRefElev1.toString(), testTerElev1.toString(), elevationUnitIri, testAddress);
        sample.constructStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        try {
            JunitTestUtils.doesExpectedListExist(genExpectedAddressStatements(testStreet1, testStreetNumber, testBlock5), result);
        } finally {
            testAddress[0] = testAddressLine;
        }
    }

    @Test
    void testConstructStatementsIncompleteAddress() {
        // Set up
        testAddress[0] = testStreet1;
        testAddress[2] = "";
        testAddress[4] = "";
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, testProjectIri, testSiteIri, testRefElev1.toString(), testTerElev1.toString(), elevationUnitIri, testAddress);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        try {
            // Generated expected statement lists and verify their existence
            JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
            JunitTestUtils.doesExpectedListExist(genExpectedAddressStatements(false), result);
            JunitTestUtils.doesExpectedListExist(genExpectedProjectStatement(), result);
            JunitTestUtils.doesExpectedListExist(genExpectedRefElevationStatements(), result);
            JunitTestUtils.doesExpectedListExist(genExpectedTerrainElevationStatements(), result);
        } finally {
            testAddress[0] = testAddressLine;
            testAddress[2] = testState;
            testAddress[4] = testPostalCode;
        }
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        IfcBuildingRepresentation sample = new IfcBuildingRepresentation(testName, testUID, testPlacementIri, testProjectIri, testSiteIri, testRefElev1.toString(), testTerElev1.toString(), elevationUnitIri, testAddress);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAddressStatements(true), result);
        JunitTestUtils.doesExpectedListExist(genExpectedProjectStatement(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedRefElevationStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedTerrainElevationStatements(), result);
    }

    private List<String> genExpectedCommonStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDF_TYPE + ", https://w3id.org/bot#Building");
        expected.add(testBaseUri + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + testBaseUri + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDF_TYPE + ", https://www.theworldavatar.com/kg/ontobim/IfcBuildingRepresentation");
        expected.add(testBaseUri + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDFS_LABEL + ", \"" + testName);
        expected.add(testBaseUri + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + testUID);
        expected.add(testSiteIri + ", https://w3id.org/bot#hasBuilding, " + testBaseUri + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + testBimPlacementIri);
        return expected;
    }

    private List<String> genExpectedProjectStatement() {
        List<String> expected = new ArrayList<>();
        expected.add(testProjectIri + ", https://www.theworldavatar.com/kg/ontobim/hasRootZone, " + testBaseUri + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }

    private List<String> genExpectedAddressStatements(String streetName, String streetNumber, String unitName) {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "Address_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://ontology.eil.utoronto.ca/icontact.owl#hasStreet, \"" + streetName);
        if (!streetNumber.isEmpty())
            expected.add(testBaseUri + "Address_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://ontology.eil.utoronto.ca/icontact.owl#hasStreetNumber, \"" + streetNumber);
        if (!unitName.isEmpty())
            expected.add(testBaseUri + "Address_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobuiltenv/hasUnitName, \"" + unitName);
        return expected;
    }

    private List<String> genExpectedAddressStatements(boolean isComplete) {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobuiltenv/hasAddress, " + testBaseUri + "Address_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri + "Address_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDF_TYPE + ", http://ontology.eil.utoronto.ca/icontact.owl#Address");
        expected.add(testBaseUri + "Address_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://ontology.eil.utoronto.ca/icontact.owl#hasStreet, \"" + testStreet1);
        expected.add(testBaseUri + "Address_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://ontology.eil.utoronto.ca/icontact.owl#hasCity, \"" + testCity);
        expected.add(testBaseUri + "Address_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://ontology.eil.utoronto.ca/icontact.owl#hasCountry, \"" + testCountry);
        if (isComplete) {
            expected.add(testBaseUri + "Address_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://ontology.eil.utoronto.ca/icontact.owl#hasStreetNumber, \"" + testStreetNumber);
            expected.add(testBaseUri + "Address_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://ontology.eil.utoronto.ca/icontact.owl#hasState, \"" + testState);
            expected.add(testBaseUri + "Address_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://ontology.eil.utoronto.ca/icontact.owl#hasPostalCode, \"" + testPostalCode);
        }
        return expected;
    }


    private List<String> genExpectedElevationStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDF_TYPE + ", http://www.ontology-of-units-of-measure.org/resource/om-2/Height");
        expected.add(testBaseUri + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue, " + testBaseUri + "Measure_");
        expected.add(testBaseUri + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDF_TYPE + ", http://www.ontology-of-units-of-measure.org/resource/om-2/Measure");
        expected.add(testBaseUri + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit, " + elevationUnitIri);
        return expected;
    }

    private List<String> genExpectedRefElevationStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + testBaseUri + "Height_");
        expected.add(testBaseUri + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + testRefElev1);
        return expected;
    }

    private List<String> genExpectedTerrainElevationStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasTerrainElevation, " + testBaseUri + "Height_");
        expected.add(testBaseUri + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + testTerElev1);
        return expected;
    }
}