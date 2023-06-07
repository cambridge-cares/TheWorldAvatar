package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Floor;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Roof;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Stair;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Wall;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.ModelRepresentation3D;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class ElementStorageTest {
    private static ElementStorage testMappings;
    private static final String TEST_BASE_URI = "https://www.example.org/";
    private static final String NON_EXISTENT_IRI = TEST_BASE_URI + "DOES/NOT/EXIST_1223";
    private static final String TEST_ELEMENT_PLACEMENT_IRI = TEST_BASE_URI + JunitTestUtils.BIM_PLACEMENT_CLASS + "_532";
    private static final String TEST_ELEMENT_CLASS = "IfcDoor";
    private static final String TEST_ELEMENT_IRI = TEST_BASE_URI + TEST_ELEMENT_CLASS + "_12";
    private static final String TEST_SEC_ELEMENT_IRI = TEST_BASE_URI + TEST_ELEMENT_CLASS + "_10581";
    private static final String TEST_SUB_CONTEXT_IRI = TEST_BASE_URI + "GeometricRepresentationSubContext_3";
    private static final String TEST_GEOM_IRI = TEST_BASE_URI + "IfcFacetedBrep_10585";
    private static final String TEST_GEOM_BIM_IRI = TEST_BASE_URI + "FacetedBrep_10585";
    private static final String TEST_SEC_SUB_CONTEXT_IRI = TEST_BASE_URI + "GeometricRepresentationSubContext_5";
    private static final String TEST_SEC_GEOM_IRI = TEST_BASE_URI + "IfcHalfSpaceSolid_33321";
    private static final String TEST_SEC_GEOM_BIM_IRI = TEST_BASE_URI + "HalfSpaceSolid_33321";

    private static final String NON_EXISTING_ERROR = NON_EXISTENT_IRI + " does not exist in mappings!";

    @BeforeAll
    static void init() {
        testMappings = ElementStorage.Singleton();
        NamespaceMapper.setBaseNameSpace(TEST_BASE_URI);
    }

    @BeforeEach
    void resetSingleton() {
        ElementStorage.resetSingleton();
    }
    @AfterAll
    static void resetNamespace(){ NamespaceMapper.setBaseNameSpace("");}

    @Test
    void testGetModelRepFail() {
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> testMappings.getModelRep(NON_EXISTENT_IRI));
        assertEquals(NON_EXISTING_ERROR, thrownError.getMessage());
    }

    @Test
    void testGetElementIriFail() {
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> testMappings.getElementIri(NON_EXISTENT_IRI));
        assertEquals(NON_EXISTING_ERROR, thrownError.getMessage());
    }

    @Test
    void testAddAndGetModelRep() {
        // Create a new sample representation
        ModelRepresentation3D sampleModelRep = new ModelRepresentation3D(TEST_SUB_CONTEXT_IRI, TEST_GEOM_IRI, null, null, null);
        // Execute method
        testMappings.add(TEST_ELEMENT_IRI, sampleModelRep);
        // Assert if they are equals
        assertEquals(sampleModelRep, testMappings.getModelRep(TEST_ELEMENT_IRI));
    }

    @Test
    void testAddAndGetElementForWall() {
        // Create a new sample representation
        Wall sampleWall = new Wall(null, null, TEST_ELEMENT_PLACEMENT_IRI, null, null, null);
        // Execute method
        testMappings.add(TEST_ELEMENT_IRI, sampleWall.getElementIri());
        // Assert if they are equals
        assertEquals(sampleWall.getElementIri(), testMappings.getElementIri(TEST_ELEMENT_IRI));
    }

    @Test
    void testAddAndGetElementForFloor() {
        // Create a new sample representation
        Floor sampleFloor = new Floor(null, null, TEST_ELEMENT_PLACEMENT_IRI, null, null);
        // Execute method
        testMappings.add(TEST_ELEMENT_IRI, sampleFloor.getIfcRepIri());
        // Assert if they are equals
        assertEquals(sampleFloor.getIfcRepIri(), testMappings.getElementIri(TEST_ELEMENT_IRI));
    }

    @Test
    void testAddAndGetElementForRoof() {
        // Create a new sample representation
        Roof sampleRoof = new Roof(null, null, TEST_ELEMENT_PLACEMENT_IRI, null, null);
        // Execute method
        testMappings.add(TEST_ELEMENT_IRI, sampleRoof.getIfcRepIri());
        // Assert if they are equals
        assertEquals(sampleRoof.getIfcRepIri(), testMappings.getElementIri(TEST_ELEMENT_IRI));
    }

    @Test
    void testAddAndGetElementForStair() {
        // Create a new sample representation
        Stair sampleStair = new Stair(null, null, TEST_ELEMENT_PLACEMENT_IRI, null);
        // Execute method
        testMappings.add(TEST_ELEMENT_IRI, sampleStair.getBIMIri());
        // Assert if they are equals
        assertEquals(sampleStair.getBIMIri(), testMappings.getElementIri(TEST_ELEMENT_IRI));
    }

    @Test
    void testAddStairSubComponentAndContainsStairSubComponentIri() {
        // Verify that there is no IRI yet
        assertFalse(testMappings.containsStairSubComponentIri(TEST_ELEMENT_IRI));
        // Execute add method
        testMappings.add(TEST_ELEMENT_IRI);
        // Verify the IRI has been added
        assertTrue(testMappings.containsStairSubComponentIri(TEST_ELEMENT_IRI));
    }

    @Test
    void testClear() {
        // Create a new sample representation
        ModelRepresentation3D sampleModelRep = new ModelRepresentation3D(TEST_SUB_CONTEXT_IRI, TEST_GEOM_IRI, null, null, null);
        // Execute method
        testMappings.add(TEST_ELEMENT_IRI, sampleModelRep);
        // Assert if they are equals
        assertTrue(testMappings.containsModelRepIri(TEST_ELEMENT_IRI));
        testMappings.clear();
        assertFalse(testMappings.containsModelRepIri(TEST_ELEMENT_IRI));
    }

    @Test
    void testContainsModelRepIri() {
        // Assert that non-existing IRIs return false
        assertFalse(testMappings.containsModelRepIri(TEST_ELEMENT_IRI));
        assertFalse(testMappings.containsModelRepIri(NON_EXISTENT_IRI));
        // Create a new sample representation
        ModelRepresentation3D sampleModelRep = new ModelRepresentation3D(TEST_SUB_CONTEXT_IRI, TEST_GEOM_IRI, null, null, null);
        // Execute method
        testMappings.add(TEST_ELEMENT_IRI, sampleModelRep);
        // Assert that non-existing IRIs return false and existing IRIs return true
        assertTrue(testMappings.containsModelRepIri(TEST_ELEMENT_IRI));
        assertFalse(testMappings.containsModelRepIri(NON_EXISTENT_IRI));
    }

    @Test
    void testContainsStairAssemblyIri() {
        // Assert that non-existing IRIs return false
        assertFalse(testMappings.containsIri(TEST_ELEMENT_IRI));
        assertFalse(testMappings.containsIri(NON_EXISTENT_IRI));
        // Create a new sample representation
        Stair sampleStair = new Stair(null, null, TEST_ELEMENT_PLACEMENT_IRI, null);
        // Execute method
        testMappings.add(TEST_ELEMENT_IRI, sampleStair.getBIMIri());
        // Assert that non-existing IRIs return false and existing IRIs return true
        assertTrue(testMappings.containsIri(TEST_ELEMENT_IRI));
        assertFalse(testMappings.containsIri(NON_EXISTENT_IRI));
    }

    @Test
    void testConstructModelRepStatementsEmptyMappings() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testMappings.constructModelRepStatements(sampleSet);
        // Assert that there is no statements generated
        assertEquals(0, sampleSet.size());
    }

    @Test
    void testConstructModelRepStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModelRepresentation3D sampleModelRep = new ModelRepresentation3D(TEST_SUB_CONTEXT_IRI, TEST_GEOM_IRI, null, null, null);
        testMappings.add(TEST_ELEMENT_IRI, sampleModelRep);
        // Execute method
        testMappings.constructModelRepStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        // Assert 3 sample statements are generated
        assertEquals(3, sampleSet.size());
    }

    @Test
    void testConstructModelRepStatementsMultipleGeom() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModelRepresentation3D sampleModelRep = new ModelRepresentation3D(TEST_SUB_CONTEXT_IRI, TEST_GEOM_IRI, null, null, null);
        sampleModelRep.appendGeometry(TEST_SEC_GEOM_IRI);
        testMappings.add(TEST_ELEMENT_IRI, sampleModelRep);
        // Execute method
        testMappings.constructModelRepStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAdditionalGeomRepStatements(), result);
    }

    @Test
    void testConstructModelRepStatementsMultipleModelRep() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModelRepresentation3D sampleModelRep = new ModelRepresentation3D(TEST_SUB_CONTEXT_IRI, TEST_GEOM_IRI, null, null, null);
        testMappings.add(TEST_ELEMENT_IRI, sampleModelRep);
        ModelRepresentation3D sampleModelRep2 = new ModelRepresentation3D(TEST_SEC_SUB_CONTEXT_IRI, TEST_SEC_GEOM_IRI, null, null, null);
        testMappings.add(TEST_SEC_ELEMENT_IRI, sampleModelRep2);
        // Execute method
        testMappings.constructModelRepStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAdditionalGeomRepStatements(), result);
        // Assert 6 sample statements are generated, which is double that of 1 model rep object
        assertEquals(6, sampleSet.size());
    }

    private List<String> genExpectedCommonStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDF_TYPE + ", https://www.theworldavatar.com/kg/ontobim/ModelRepresentation3D");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasSubContext, " + TEST_SUB_CONTEXT_IRI);
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + TEST_GEOM_BIM_IRI);
        return expected;
    }

    private List<String> genExpectedAdditionalGeomRepStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + TEST_SEC_GEOM_BIM_IRI);
        return expected;
    }
}