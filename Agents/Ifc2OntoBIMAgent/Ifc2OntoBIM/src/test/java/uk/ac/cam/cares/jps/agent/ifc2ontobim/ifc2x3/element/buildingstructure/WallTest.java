package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class WallTest {
    private static final String testBaseUri = "https://www.example.org/";
    private static final String testIri1 = testBaseUri + "IfcWall_16012";
    private static final String testName1 = "Brick wall";
    private static final String testUID1 = "01294ha";
    private static final String testPlacementIri1 = testBaseUri + "LocalPlacement_515";
    private static final String testIri2 = testBaseUri + "IfcWallStandardCase_515122";
    private static final String testName2 = "Glass Wall";
    private static final String testUID2 = "9sdg781";
    private static final String testPlacementIri2 = testBaseUri + "LocalPlacement_8172";
    private static final String testHostZoneIRI = testBaseUri + "Storey_715";
    private static final String testGeomModelIRI = testBaseUri + "ModelRepresentation3D_715";
    private static final String testSecGeomModelIRI = testBaseUri + "ModelRepresentation3D_5519";
    private static final String testClassName = "IfcModelRepresentation";
    private static final String testClass = JunitTestUtils.bimUri + testClassName;

    @BeforeAll
    static void createNamespace(){ NamespaceMapper.setBaseNameSpace(testBaseUri); }
    @AfterAll
    static void resetNamespace(){
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testConstructor() {
        Wall sample = new Wall(testName1, testUID1, testPlacementIri1, testHostZoneIRI, testGeomModelIRI, null);
        // Test that the sample fields are correct
        assertEquals(testBaseUri, sample.getPrefix());
        assertNotEquals(testIri1, sample.getIfcRepIri());
        assertTrue(sample.getIfcRepIri().contains(testBaseUri + testClassName + "_"));
        assertEquals(testName1, sample.getName());
        assertEquals(testUID1, sample.getUid());
        assertEquals(testPlacementIri1, sample.getPlacementIri());

        Wall sample2 = new Wall(testName2, testUID2, testPlacementIri2, testHostZoneIRI, testGeomModelIRI, null);
        // Test that the sample fields are correct
        assertEquals(testBaseUri, sample2.getPrefix());
        assertNotEquals(testIri2, sample2.getIfcRepIri());
        assertTrue(sample2.getIfcRepIri().contains(testBaseUri + testClassName + "_"));
        assertEquals(testName2, sample2.getName());
        assertEquals(testUID2, sample2.getUid());
        assertEquals(testPlacementIri2, sample2.getPlacementIri());
    }

    @Test
    void testConstructStatementsNoOptionalFields() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        Wall sample = new Wall(testName1, testUID1, testPlacementIri1, testHostZoneIRI, testGeomModelIRI, null);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        // Verify the optional statements are not generated
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalStatements(), result);
    }

    @Test
    void testConstructStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        Wall sample = new Wall(testName1, testUID1, testPlacementIri1, testHostZoneIRI, testGeomModelIRI, testSecGeomModelIRI);
        // Execute method
        sample.constructStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalStatements(), result);
    }

    private List<String> genExpectedCommonStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testHostZoneIRI + ", https://w3id.org/bot#containsElement, " + testBaseUri + "Wall_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri + "Wall_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobuildingstructure/Wall");
        expected.add(testBaseUri + "Wall_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + testBaseUri + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, " + testClass);
        expected.add(testBaseUri + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/2000/01/rdf-schema#label, \"" + testName1);
        expected.add(testBaseUri + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + testUID1);
        expected.add(testBaseUri + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + testPlacementIri1);
        expected.add(testBaseUri + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasGeometricRepresentation, " + testGeomModelIRI);
        return expected;
    }

    private List<String> genExpectedOptionalStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasGeometricRepresentation, " + testSecGeomModelIRI);
        return expected;
    }
}