package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage;

import org.apache.jena.rdf.model.Statement;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestGeometryUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.FacetedBrep;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.Polyline;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.LinkedHashSet;

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
    private static final String TEST_BREP_INSTANCE = TEST_BASE_URI + "IfcFacetedBrep_6362";
    private static final String TEST_BREP_BIM_INSTANCE = TEST_BASE_URI + "FacetedBrep_6362";
    private static final String TEST_POLY_LOOP_BIM_INSTANCE = TEST_BASE_URI + "PolyLoop_6347";
    private static final boolean TEST_IS_ORIENTATION_INDICATOR = true;
    private static final String TEST_POLYLINE_INSTANCE = TEST_BASE_URI + IFC_POLYLINE + "_6347";
    private static final String TEST_POLYLINE_BIM_INSTANCE = TEST_BASE_URI + BIM_POLYLINE + "_6347";
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
    void resetSingleton() { testMappings = GeometryStorage.resetSingleton(); }

    @AfterAll
    static void reset() {
        testMappings = GeometryStorage.resetSingleton();
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testResetSingleton() {
        // Add a sample and verify if it is added
        Polyline sample = new Polyline(TEST_POLYLINE_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, null);
        testMappings.add(TEST_POLYLINE_INSTANCE, sample);
        assertTrue(testMappings.containsIri(TEST_POLYLINE_INSTANCE));
        testMappings.add(TEST_SEC_INSTANCE, sample);
        // Execute method
        testMappings = GeometryStorage.resetSingleton();
        // Verify if the mappings have been reset with none of the previously added IRIs
        assertFalse(testMappings.containsIri(TEST_POLYLINE_INSTANCE));
    }

    @Test
    void testAddFacetedBrep() {
        // Create a new sample
        FacetedBrep sample = new FacetedBrep(TEST_BREP_INSTANCE, TEST_POLYLINE_INSTANCE, TEST_IS_ORIENTATION_INDICATOR);
        // Execute method
        testMappings.add(TEST_BREP_INSTANCE, sample);
        // Assert if the instance is now available
        assertTrue(testMappings.containsIri(TEST_BREP_INSTANCE));
    }

    @Test
    void testAddPolyline() {
        // Create a new sample
        Polyline sample = new Polyline(TEST_POLYLINE_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, null);
        // Execute method
        testMappings.add(TEST_POLYLINE_INSTANCE, sample);
        // Assert if the instance is now available
        assertTrue(testMappings.containsIri(TEST_POLYLINE_INSTANCE));
    }

    @Test
    void testContainsIri() {
        // Add a sample
        Polyline sample = new Polyline(TEST_POLYLINE_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, null);
        testMappings.add(TEST_POLYLINE_INSTANCE, sample);
        // Verify method returns correctly
        assertTrue(testMappings.containsIri(TEST_POLYLINE_INSTANCE));
        assertFalse(testMappings.containsIri(TEST_BREP_INSTANCE));
        // Rerun assertions for another sample
        FacetedBrep sample2 = new FacetedBrep(TEST_BREP_INSTANCE, TEST_POLYLINE_INSTANCE, TEST_IS_ORIENTATION_INDICATOR);
        testMappings.add(TEST_BREP_INSTANCE, sample2);
        assertTrue(testMappings.containsIri(TEST_BREP_INSTANCE));
    }

    @Test
    void testGetFacetedBrepFail() {
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> testMappings.getFacetedBrep(NON_EXISTENT_IRI));
        assertEquals(NON_EXISTING_ERROR, thrownError.getMessage());
    }

    @Test
    void testGetFacetedBrep() {
        // Set up
        FacetedBrep sample = new FacetedBrep(TEST_BREP_INSTANCE, TEST_POLYLINE_INSTANCE, TEST_IS_ORIENTATION_INDICATOR);
        testMappings.add(TEST_BREP_INSTANCE, sample);
        // Execute method
        FacetedBrep result = testMappings.getFacetedBrep(TEST_BREP_INSTANCE);
        // Assert if they are the same object
        assertEquals(sample, result);
    }

    @Test
    void testGetPolylineFail() {
        JPSRuntimeException thrownError = assertThrows(JPSRuntimeException.class, () -> testMappings.getPolyline(NON_EXISTENT_IRI));
        assertEquals(NON_EXISTING_ERROR, thrownError.getMessage());
    }

    @Test
    void testGetPolyline() {
        // Set up
        Polyline sample = new Polyline(TEST_POLYLINE_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, null);
        testMappings.add(TEST_POLYLINE_INSTANCE, sample);
        // Execute method
        Polyline result = testMappings.getPolyline(TEST_POLYLINE_INSTANCE);
        // Assert if they are the same object
        assertEquals(sample, result);
    }

    @Test
    void testConstructFacetedBrepStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        FacetedBrep sample = new FacetedBrep(TEST_BREP_INSTANCE, TEST_POLYLINE_INSTANCE, TEST_IS_ORIENTATION_INDICATOR);
        testMappings.add(TEST_BREP_INSTANCE, sample);
        // Execute method
        testMappings.constructFacetedBrepStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepStatements(TEST_BASE_URI, TEST_BREP_BIM_INSTANCE), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepFaceStatements(TEST_BASE_URI, TEST_POLY_LOOP_BIM_INSTANCE, TEST_IS_ORIENTATION_INDICATOR), result);
    }

    @Test
    void testConstructPolyLineStatements() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        Polyline sample = new Polyline(TEST_POLYLINE_INSTANCE, TEST_STARTING_VERTEX, TEST_STARTING_VERTEX, TEST_STARTING_POINT, null);
        testMappings.add(TEST_POLYLINE_INSTANCE, sample);
        // Execute method
        testMappings.constructPolyLineStatements(sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPolylineStatements(TEST_POLYLINE_BIM_INSTANCE, TEST_BIM_STARTING_VERTEX, TEST_STARTING_POINT, BIM_POLYLINE), result);
    }
}