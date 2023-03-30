package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.Polyline;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class GeometryStorageTest {
    private static GeometryStorage testMappings;
    private static final String TEST_BASE_URI = "http://www.example.org/";
    private static final String NON_EXISTENT_IRI = TEST_BASE_URI + "DOES/NOT/EXIST_1223";
    private static final String NON_EXISTING_ERROR = NON_EXISTENT_IRI + " does not exist in mappings!";
    private static final String IFC_POLYLINE = "IfcPolyline";
    private static final String BIM_POLYLINE = "Polyline";
    private static final String IFC_LINE_VERTEX = "IfcCartesianPoint_List";
    private static final String BIM_LINE_VERTEX = "LineVertex";
    private static final String BIM_POINT = "CartesianPoint";
    private static final String TEST_INSTANCE = TEST_BASE_URI + IFC_POLYLINE + "_6347";
    private static final String TEST_BIM_INSTANCE = TEST_BASE_URI + BIM_POLYLINE + "_6347";
    private static final String TEST_STARTING_VERTEX = TEST_BASE_URI + IFC_LINE_VERTEX + "_6348";
    private static final String TEST_BIM_STARTING_VERTEX = TEST_BASE_URI + BIM_LINE_VERTEX + "_6348";
    private static final String TEST_STARTING_POINT = TEST_BASE_URI + BIM_POINT + "_6350";
    private static final String TEST_SEC_INSTANCE = TEST_BASE_URI + IFC_POLYLINE + "_5327";


    @BeforeAll
    static void setUp() {
        testMappings = GeometryStorage.Singleton();
        NamespaceMapper.setBaseNameSpace(TEST_BASE_URI);
    }

    @BeforeEach
    void resetSingleton() {
        GeometryStorage.resetSingleton();
    }

    @AfterAll
    static void reset() {
        GeometryStorage.resetSingleton();
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testGetPolylineFail() {
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> testMappings.getPolyline(NON_EXISTENT_IRI));
        assertEquals(NON_EXISTING_ERROR, thrownError.getMessage());
    }

    @Test
    void testAddPolyline() {
        // Create a new sample
        Polyline sample = new Polyline(TEST_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, null);
        // Execute method
        testMappings.add(TEST_INSTANCE, sample);
        // Assert if the instance is now available
        assertTrue(testMappings.containsIri(TEST_INSTANCE));
    }

    @Test
    void testContainsIri() {
        // Add a sample
        Polyline sample = new Polyline(TEST_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, null);
        testMappings.add(TEST_INSTANCE, sample);
        // Verify method returns correctly
        assertTrue(testMappings.containsIri(TEST_INSTANCE));
        assertFalse(testMappings.containsIri(NON_EXISTENT_IRI));
    }

    @Test
    void testResetSingleton() {
        // Add a sample and verify if it is added
        Polyline sample = new Polyline(TEST_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, null);
        testMappings.add(TEST_INSTANCE, sample);
        assertTrue(testMappings.containsIri(TEST_INSTANCE));
        testMappings.add(TEST_SEC_INSTANCE, sample);
        // Execute method
        testMappings = GeometryStorage.resetSingleton();
        // Verify if the mappings have been reset with none of the previously added IRIs
        assertFalse(testMappings.containsIri(TEST_INSTANCE));
    }

    @Test
    void testGetPolyline() {
        // Set up
        Polyline sample = new Polyline(TEST_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, null);
        testMappings.add(TEST_INSTANCE, sample);
        // Execute method
        Polyline result = testMappings.getPolyline(TEST_INSTANCE);
        // Assert if they are the same object
        assertEquals(sample, result);
    }

    @Test
    void testConstructGeomStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        Polyline sample = new Polyline(TEST_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, null);
        testMappings.add(TEST_INSTANCE, sample);
        // Execute method
        testMappings.constructGeomStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonStatements(), result);
    }


    private List<String> genExpectedCommonStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BIM_INSTANCE + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/Polyline");
        expected.add(TEST_BIM_INSTANCE + ", http://www.theworldavatar.com/kg/ontobim/hasStartingVertex, " + TEST_BIM_STARTING_VERTEX);
        expected.add(TEST_BIM_STARTING_VERTEX + ", http://www.theworldavatar.com/kg/ontobim/hasRefPoint, " + TEST_STARTING_POINT);
        expected.add(TEST_BIM_STARTING_VERTEX + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LineVertex");
        return expected;
    }
}