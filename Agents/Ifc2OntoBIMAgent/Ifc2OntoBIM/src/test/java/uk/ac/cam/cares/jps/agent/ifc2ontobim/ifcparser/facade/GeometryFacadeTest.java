package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.facade;

import org.apache.jena.rdf.model.*;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestGeometryUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.CartesianPoint;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.DirectionVector;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.ModellingOperatorStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

class GeometryFacadeTest {
    private static Model sampleModel;
    private static ModellingOperatorStorage operatorMappings;
    private static GeometryFacade testHelper;
    private static final String TEST_BASE_URI = "https://www.theworldavatar.com/test/";
    private static final String POLYLINE_CLASS = "IfcPolyline";
    private static final String POLYLINE_PROPERTY = "points_IfcPolyline";
    private static final String POLYLOOP_CLASS = "IfcPolyLoop";
    private static final String POLYLOOP_PROPERTY = "polygon_IfcPolyLoop";
    private static final Resource IFC_DIRECTION = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcDirection");
    // Faceted brep fields
    private static final String IFC_FACETED_BREP_INST = TEST_BASE_URI + "IfcFacetedBrep_14827";
    private static final String BIM_FACETED_BREP_INST = TEST_BASE_URI + "FacetedBrep_14827";
    private static final String IFC_CLOSED_SHELL_INST = TEST_BASE_URI + "IfcClosedShell_38157";
    private static final String IFC_BREP_FIRST_FACE_BOUNDARY_INST = TEST_BASE_URI + "IfcPolyLoop_38163";
    private static final String BIM_BREP_FIRST_FACE_BOUNDARY_INST = TEST_BASE_URI + "PolyLoop_38163";
    private static final boolean IFC_BREP_FIRST_FACE_BOUNDARY_ORIENTATION = true;
    private static final String IFC_BREP_SEC_FACE_BOUNDARY_INST = TEST_BASE_URI + "IfcPolyLoop_38166";
    private static final String BIM_BREP_SEC_FACE_BOUNDARY_INST = TEST_BASE_URI + "PolyLoop_38166";
    private static final boolean IFC_BREP_SEC_FACE_BOUNDARY_ORIENTATION = false;
    // Extruded area solid fields
    private static final String IFC_EXTRUDED_AREA_SOLID_INST = TEST_BASE_URI + "IfcExtrudedAreaSolid_14823";
    private static final String BIM_EXTRUDED_AREA_SOLID_INST = TEST_BASE_URI + "ExtrudedAreaSolid_14823";
    private static final String IFC_GEOM_PROFILE_TYPE = JunitTestUtils.ifc2x3Uri + "AREA";
    private static final Double IFC_PROFILE_DIR_VECTOR_X_RATIO = 1.25;
    private static final Double IFC_PROFILE_DIR_VECTOR_Y_RATIO = 2.2;
    private static final Double IFC_PROFILE_DIR_VECTOR_Z_RATIO = 3.2;
    private static final Double EXTRUDED_DEPTH = 5.16;
    private static final Double REC_PROFILE_X_EXTENT = 1.23;
    private static final Double REC_PROFILE_Y_EXTENT = 1.43;
    // Generic direction and cartesian points fields
    private static final String IFC_REF_DIR_VECTOR_INST = TEST_BASE_URI + "IfcDirection_7645";
    private static final String IFC_AXIS_DIR_VECTOR_INST = TEST_BASE_URI + "IfcDirection_6246";
    private static final String IFC_PROFILE_DIR_VECTOR_INST = TEST_BASE_URI + "IfcDirection_7347";
    private static final String IFC_GEOM_POSITION_INST = TEST_BASE_URI + "IfcCartesianPoint_7628";
    private static final String IFC_GEOM_SEC_POSITION_INST = TEST_BASE_URI + "IfcCartesianPoint_6827";
    private static final Double IFC_GEOM_POSITION_X_COORD = 5.12;
    private static final Double IFC_GEOM_POSITION_Y_COORD = 2.13;
    private static final Double IFC_GEOM_POSITION_Z_COORD = 7.42;
    private static final Double IFC_GEOM_SEC_POSITION_X_COORD = 3.28;
    private static final Double IFC_GEOM_SEC_POSITION_Y_COORD = 3.45;
    private static final Double IFC_GEOM_SEC_POSITION_Z_COORD = 0.2;
    private static final Double IFC_REF_DIR_VECTOR_X_RATIO = 1.0;
    private static final Double IFC_REF_DIR_VECTOR_Y_RATIO = 2.0;
    private static final Double IFC_REF_DIR_VECTOR_Z_RATIO = 3.0;
    private static final Double IFC_AXIS_DIR_VECTOR_X_RATIO = 1.3;
    private static final Double IFC_AXIS_DIR_VECTOR_Y_RATIO = 2.3;
    private static final Double IFC_AXIS_DIR_VECTOR_Z_RATIO = 3.3;
    // Bounded Half space fields
    private static final String IFC_HALF_SPACE_SOLID_INSTANCE = TEST_BASE_URI + "IfcHalfSpaceSolid_4205";
    private static final String BIM_HALF_SPACE_SOLID_INSTANCE = TEST_BASE_URI + "HalfSpaceSolid_4205";
    private static final String IFC_HALF_SPACE_INSTANCE = TEST_BASE_URI + "IfcPolygonalBoundedHalfSpace_6287";
    private static final String BIM_HALF_SPACE_INSTANCE = TEST_BASE_URI + "PolygonalBoundedHalfSpace_6287";
    private static final boolean HALF_SPACE_FLAG = false;
    // Polyline fields
    private static final String IFC_POLYLINE_INSTANCE = TEST_BASE_URI + "IfcPolyline_777";
    private static final String BIM_POLYLINE_INSTANCE = TEST_BASE_URI + "Polyline_777";
    private static final String IFC_POLYLOOP_INSTANCE = TEST_BASE_URI + "IfcPolyLoop_625";
    private static final String BIM_POLYLOOP_INSTANCE = TEST_BASE_URI + "PolyLoop_625";
    private static final String IFC_STARTING_VERTEX = TEST_BASE_URI + "IfcCartesianPoint_List_778";
    private static final String BIM_STARTING_VERTEX = TEST_BASE_URI + "LineVertex_778";
    private static final String IFC_SEC_VERTEX = TEST_BASE_URI + "IfcCartesianPoint_List_779";
    private static final String BIM_SEC_VERTEX = TEST_BASE_URI + "LineVertex_779";
    // Boolean clipping result fields
    private static final String IFC_CLIPPING_RESULT_IRI = TEST_BASE_URI + "IfcBooleanClippingResult_874";
    private static final String BIM_CLIPPING_RESULT_IRI = TEST_BASE_URI + "BooleanClippingResult_874";
    private static final String TEST_OPERATOR = JunitTestUtils.ifc2x3Uri + "DIFFERENCE";
    private static final String IFC_FIRST_GEOM = TEST_BASE_URI + "IfcPolygonalBoundedHalfSpace_892";
    private static final String IFC_SEC_GEOM = TEST_BASE_URI + "IfcPolyline_996";
    private static final String IFC_NESTED_BOOLEAN_RESULT = TEST_BASE_URI + "IfcBooleanClippingResult_1002";
    private static final String BIM_FIRST_GEOM = TEST_BASE_URI + "PolygonalBoundedHalfSpace_892";
    private static final String BIM_SEC_GEOM = TEST_BASE_URI + "Polyline_996";
    private static final String BIM_NESTED_BOOLEAN_RESULT = TEST_BASE_URI + "BooleanClippingResult_1002";

    @BeforeAll
    static void setUp() {
        NamespaceMapper.setBaseNameSpace(TEST_BASE_URI);
        operatorMappings = ModellingOperatorStorage.Singleton();
        // Geometry position operators
        CartesianPoint point = new CartesianPoint(IFC_GEOM_POSITION_X_COORD.toString(), IFC_GEOM_POSITION_Y_COORD.toString(), IFC_GEOM_POSITION_Z_COORD.toString());
        operatorMappings.add(IFC_GEOM_POSITION_INST, point);
        DirectionVector direction = new DirectionVector(IFC_REF_DIR_VECTOR_X_RATIO.toString(), IFC_REF_DIR_VECTOR_Y_RATIO.toString(), IFC_REF_DIR_VECTOR_Z_RATIO.toString());
        operatorMappings.add(IFC_REF_DIR_VECTOR_INST, direction);
        direction = new DirectionVector(IFC_AXIS_DIR_VECTOR_X_RATIO.toString(), IFC_AXIS_DIR_VECTOR_Y_RATIO.toString(), IFC_AXIS_DIR_VECTOR_Z_RATIO.toString());
        operatorMappings.add(IFC_AXIS_DIR_VECTOR_INST, direction);
        // Profile position operators
        point = new CartesianPoint(IFC_GEOM_SEC_POSITION_X_COORD.toString(), IFC_GEOM_SEC_POSITION_Y_COORD.toString(), IFC_GEOM_SEC_POSITION_Z_COORD.toString());
        operatorMappings.add(IFC_GEOM_SEC_POSITION_INST, point);
        direction = new DirectionVector(IFC_PROFILE_DIR_VECTOR_X_RATIO.toString(), IFC_PROFILE_DIR_VECTOR_Y_RATIO.toString(), IFC_PROFILE_DIR_VECTOR_Z_RATIO.toString());
        operatorMappings.add(IFC_PROFILE_DIR_VECTOR_INST, direction);
    }

    @BeforeEach
    void reset() {
        sampleModel = ModelFactory.createDefaultModel();
        testHelper = new GeometryFacade();
    }

    @AfterAll
    static void resetParametersForOtherTests() {
        ModellingOperatorStorage.resetSingleton();
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testAddFacetedBrepStatements() {
        // Set up
        addFacetedBrepTriples();
        addFacetedBrepFaceTriples(IFC_BREP_FIRST_FACE_BOUNDARY_INST, IFC_BREP_FIRST_FACE_BOUNDARY_ORIENTATION);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addFacetedBrepStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepStatements(TEST_BASE_URI, BIM_FACETED_BREP_INST), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepFaceStatements(TEST_BASE_URI, BIM_BREP_FIRST_FACE_BOUNDARY_INST, IFC_BREP_FIRST_FACE_BOUNDARY_ORIENTATION), result);
    }

    @Test
    void testAddFacetedBrepStatementsMultipleFaces() {
        // Set up
        addFacetedBrepTriples();
        addFacetedBrepFaceTriples(IFC_BREP_FIRST_FACE_BOUNDARY_INST, IFC_BREP_FIRST_FACE_BOUNDARY_ORIENTATION);
        addFacetedBrepFaceTriples(IFC_BREP_SEC_FACE_BOUNDARY_INST, IFC_BREP_SEC_FACE_BOUNDARY_ORIENTATION);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addFacetedBrepStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepStatements(TEST_BASE_URI, BIM_FACETED_BREP_INST), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepFaceStatements(TEST_BASE_URI, BIM_BREP_FIRST_FACE_BOUNDARY_INST, IFC_BREP_FIRST_FACE_BOUNDARY_ORIENTATION), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedFacetedBrepFaceStatements(TEST_BASE_URI, BIM_BREP_SEC_FACE_BOUNDARY_INST, IFC_BREP_SEC_FACE_BOUNDARY_ORIENTATION), result);
    }

    @Test
    void testAddExtrudedAreaSolidStatements() {
        // Set up
        addExtrudedAreaSolidTriples(IFC_EXTRUDED_AREA_SOLID_INST, true);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addExtrudedAreaSolidStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        JunitTestUtils.doesExpectedListExist(genExpectedExtrudedAreaSolidStatements(BIM_EXTRUDED_AREA_SOLID_INST), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalExtrudedAreaSolidStatements(BIM_EXTRUDED_AREA_SOLID_INST), result);
        // Verify that the right points and direction are generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_POSITION_X_COORD, IFC_GEOM_POSITION_Y_COORD, IFC_GEOM_POSITION_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_REF_DIR_VECTOR_X_RATIO, IFC_REF_DIR_VECTOR_Y_RATIO, IFC_REF_DIR_VECTOR_Z_RATIO, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_AXIS_DIR_VECTOR_X_RATIO, IFC_AXIS_DIR_VECTOR_Y_RATIO, IFC_AXIS_DIR_VECTOR_Z_RATIO, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_SEC_POSITION_X_COORD, IFC_GEOM_SEC_POSITION_Y_COORD, IFC_GEOM_SEC_POSITION_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_PROFILE_DIR_VECTOR_X_RATIO, IFC_PROFILE_DIR_VECTOR_Y_RATIO, IFC_PROFILE_DIR_VECTOR_Z_RATIO, true), result);
    }

    @Test
    void testAddExtrudedAreaSolidStatementsWithNoOptional() {
        // Set up
        addExtrudedAreaSolidTriples(IFC_EXTRUDED_AREA_SOLID_INST, false);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addExtrudedAreaSolidStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        JunitTestUtils.doesExpectedListExist(genExpectedExtrudedAreaSolidStatements(BIM_EXTRUDED_AREA_SOLID_INST), result);
        // Verify optional statements are not generated
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalExtrudedAreaSolidStatements(BIM_EXTRUDED_AREA_SOLID_INST), result);
        // Verify that the right points are generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_POSITION_X_COORD, IFC_GEOM_POSITION_Y_COORD, IFC_GEOM_POSITION_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_SEC_POSITION_X_COORD, IFC_GEOM_SEC_POSITION_Y_COORD, IFC_GEOM_SEC_POSITION_Z_COORD, true), result);
        // Direction for extruded direction will be generated
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_REF_DIR_VECTOR_X_RATIO, IFC_REF_DIR_VECTOR_Y_RATIO, IFC_REF_DIR_VECTOR_Z_RATIO, true), result);
        // Verify no directions are generated
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_AXIS_DIR_VECTOR_X_RATIO, IFC_AXIS_DIR_VECTOR_Y_RATIO, IFC_AXIS_DIR_VECTOR_Z_RATIO), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_PROFILE_DIR_VECTOR_X_RATIO, IFC_PROFILE_DIR_VECTOR_Y_RATIO, IFC_PROFILE_DIR_VECTOR_Z_RATIO), result);
    }

    @Test
    void testAddHalfSpaceSolidStatements() {
        // Set up
        addHalfSpaceSolidTriples(IFC_HALF_SPACE_SOLID_INSTANCE, "IfcHalfSpaceSolid");
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addHalfSpaceSolidStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        String positions = TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}";
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedHalfSpaceSolidStatements(TEST_BASE_URI, BIM_HALF_SPACE_SOLID_INSTANCE, "HalfSpaceSolid", positions, HALF_SPACE_FLAG), result);
        // Verify that the right points and direction are generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_SEC_POSITION_X_COORD, IFC_GEOM_SEC_POSITION_Y_COORD, IFC_GEOM_SEC_POSITION_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_PROFILE_DIR_VECTOR_X_RATIO, IFC_PROFILE_DIR_VECTOR_Y_RATIO, IFC_PROFILE_DIR_VECTOR_Z_RATIO, true), result);
    }

    @Test
    void testAddHalfSpaceStatements() {
        // Set up
        addHalfSpaceTriples();
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addHalfSpaceStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        String positions = TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}";
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedHalfSpaceStatements(TEST_BASE_URI, BIM_HALF_SPACE_INSTANCE, positions, positions, BIM_POLYLINE_INSTANCE, HALF_SPACE_FLAG), result);
        // Verify that the right points and direction are generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_POSITION_X_COORD, IFC_GEOM_POSITION_Y_COORD, IFC_GEOM_POSITION_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_REF_DIR_VECTOR_X_RATIO, IFC_REF_DIR_VECTOR_Y_RATIO, IFC_REF_DIR_VECTOR_Z_RATIO, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_AXIS_DIR_VECTOR_X_RATIO, IFC_AXIS_DIR_VECTOR_Y_RATIO, IFC_AXIS_DIR_VECTOR_Z_RATIO, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_SEC_POSITION_X_COORD, IFC_GEOM_SEC_POSITION_Y_COORD, IFC_GEOM_SEC_POSITION_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_PROFILE_DIR_VECTOR_X_RATIO, IFC_PROFILE_DIR_VECTOR_Y_RATIO, IFC_PROFILE_DIR_VECTOR_Z_RATIO, true), result);
    }

    @Test
    void testAddPolylineStatements() {
        // Set up
        addPolylineTriples(IFC_POLYLINE_INSTANCE, POLYLINE_CLASS, POLYLINE_PROPERTY);
        addPolylineSecondVertexTriples();
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addPolylineStatements(sampleModel, sampleSet, false);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPolylineStatements(BIM_POLYLINE_INSTANCE, BIM_STARTING_VERTEX, operatorMappings.getPoint(IFC_GEOM_POSITION_INST).getIri(), "Polyline"), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedNextLineVertexStatements(BIM_STARTING_VERTEX, BIM_SEC_VERTEX, operatorMappings.getPoint(IFC_GEOM_SEC_POSITION_INST).getIri()), result);
        // Verify that the right points are generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_POSITION_X_COORD, IFC_GEOM_POSITION_Y_COORD, IFC_GEOM_POSITION_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_SEC_POSITION_X_COORD, IFC_GEOM_SEC_POSITION_Y_COORD, IFC_GEOM_SEC_POSITION_Z_COORD, true), result);
    }

    @Test
    void testAddPolylineStatementsOneVertex() {
        // Set up
        addPolylineTriples(IFC_POLYLINE_INSTANCE, POLYLINE_CLASS, POLYLINE_PROPERTY);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addPolylineStatements(sampleModel, sampleSet, false);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPolylineStatements(BIM_POLYLINE_INSTANCE, BIM_STARTING_VERTEX, operatorMappings.getPoint(IFC_GEOM_POSITION_INST).getIri(), "Polyline"), result);
        // Verify the next vertex is not generated since it doesn't exist
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedNextLineVertexStatements(BIM_STARTING_VERTEX, BIM_SEC_VERTEX, operatorMappings.getPoint(IFC_GEOM_SEC_POSITION_INST).getIri()), result);
        // Verify that the right points are generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_POSITION_X_COORD, IFC_GEOM_POSITION_Y_COORD, IFC_GEOM_POSITION_Z_COORD, true), result);
    }

    @Test
    void testAddPolylineStatementsForPolyLoop() {
        // Set up
        addPolylineTriples(IFC_POLYLOOP_INSTANCE, POLYLOOP_CLASS, POLYLOOP_PROPERTY);
        addPolylineSecondVertexTriples();
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addPolylineStatements(sampleModel, sampleSet, true);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPolylineStatements(BIM_POLYLOOP_INSTANCE, BIM_STARTING_VERTEX, operatorMappings.getPoint(IFC_GEOM_POSITION_INST).getIri(), "PolyLoop"), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedNextLineVertexStatements(BIM_STARTING_VERTEX, BIM_SEC_VERTEX, operatorMappings.getPoint(IFC_GEOM_SEC_POSITION_INST).getIri()), result);
        // Verify that the right points are generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_POSITION_X_COORD, IFC_GEOM_POSITION_Y_COORD, IFC_GEOM_POSITION_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_SEC_POSITION_X_COORD, IFC_GEOM_SEC_POSITION_Y_COORD, IFC_GEOM_SEC_POSITION_Z_COORD, true), result);
    }

    @Test
    void testAddPolylineStatementsOneVertexForPolyLoop() {
        // Set up
        addPolylineTriples(IFC_POLYLOOP_INSTANCE, POLYLOOP_CLASS, POLYLOOP_PROPERTY);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addPolylineStatements(sampleModel, sampleSet, true);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPolylineStatements(BIM_POLYLOOP_INSTANCE, BIM_STARTING_VERTEX, operatorMappings.getPoint(IFC_GEOM_POSITION_INST).getIri(), "PolyLoop"), result);
        // Verify the next vertex is not generated since it doesn't exist
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedNextLineVertexStatements(BIM_STARTING_VERTEX, BIM_SEC_VERTEX, operatorMappings.getPoint(IFC_GEOM_SEC_POSITION_INST).getIri()), result);
        // Verify that the right points are generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_GEOM_POSITION_X_COORD, IFC_GEOM_POSITION_Y_COORD, IFC_GEOM_POSITION_Z_COORD, true), result);
    }

    @Test
    void testAddBooleanClippingResultStatements() {
        // Set up
        addClippingResultTriples(IFC_CLIPPING_RESULT_IRI, IFC_FIRST_GEOM, IFC_SEC_GEOM);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addBooleanClippingResultStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedBooleanClippingResultStatements(BIM_CLIPPING_RESULT_IRI, TEST_OPERATOR, BIM_FIRST_GEOM, BIM_SEC_GEOM), result);
    }

    @Test
    void testAddBooleanClippingResultStatementsForNestedOperand() {
        // Set up
        addClippingResultTriples(IFC_CLIPPING_RESULT_IRI, IFC_FIRST_GEOM, IFC_NESTED_BOOLEAN_RESULT);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addBooleanClippingResultStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedBooleanClippingResultStatements(BIM_CLIPPING_RESULT_IRI, TEST_OPERATOR, BIM_FIRST_GEOM, BIM_NESTED_BOOLEAN_RESULT), result);
    }

    private void addFacetedBrepTriples() {
        sampleModel.createResource(IFC_FACETED_BREP_INST)
                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcFacetedBrep"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "outer_IfcManifoldSolidBrep"), sampleModel.createResource(IFC_CLOSED_SHELL_INST)
                        .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcClosedShell"))
                );
    }

    private void addFacetedBrepFaceTriples(String faceBoundaryIri, boolean indicator) {
        sampleModel.getResource(IFC_CLOSED_SHELL_INST)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "cfsFaces_IfcConnectedFaceSet"), sampleModel.createResource()
                        .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcFace"))
                        .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "bounds_IfcFace"), sampleModel.createResource()
                                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcFaceOuterBound"))
                                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "bound_IfcFaceBound"), sampleModel.createResource(faceBoundaryIri))
                                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "orientation_IfcFaceBound"),
                                        sampleModel.createResource().addProperty(JunitTestUtils.hasBoolean, sampleModel.createTypedLiteral(indicator))
                                )
                        )
                );
    }

    private void addExtrudedAreaSolidTriples(String iri, boolean isComplete) {
        Resource extrudedPosition = sampleModel.createResource().addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "location_IfcPlacement"), sampleModel.createResource(IFC_GEOM_POSITION_INST));
        Resource profilePosition = sampleModel.createResource().addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "location_IfcPlacement"), sampleModel.createResource(IFC_GEOM_SEC_POSITION_INST));
        if (isComplete) {
            extrudedPosition.addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "refDirection_IfcAxis2Placement3D"), sampleModel.createResource(IFC_REF_DIR_VECTOR_INST))
                    .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "axis_IfcAxis2Placement3D"), sampleModel.createResource(IFC_AXIS_DIR_VECTOR_INST));
            profilePosition.addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "refDirection_IfcAxis2Placement2D"), sampleModel.createResource(IFC_PROFILE_DIR_VECTOR_INST));
        }
        sampleModel.createResource(iri)
                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcExtrudedAreaSolid"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "position_IfcSweptAreaSolid"), extrudedPosition)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "extrudedDirection_IfcExtrudedAreaSolid"), sampleModel.createResource(IFC_REF_DIR_VECTOR_INST)
                        .addProperty(JunitTestUtils.RDF_TYPE, IFC_DIRECTION))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "depth_IfcExtrudedAreaSolid"), sampleModel.createResource()
                        .addProperty(JunitTestUtils.hasDouble, sampleModel.createTypedLiteral(EXTRUDED_DEPTH)))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "sweptArea_IfcSweptAreaSolid"),
                        sampleModel.createResource()
                                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRectangleProfileDef"))
                                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "profileType_IfcProfileDef"), sampleModel.createResource(IFC_GEOM_PROFILE_TYPE))
                                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "position_IfcParameterizedProfileDef"), profilePosition)
                                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "xDim_IfcRectangleProfileDef"), sampleModel.createResource()
                                        .addProperty(JunitTestUtils.hasDouble, sampleModel.createTypedLiteral(REC_PROFILE_X_EXTENT)))
                                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "yDim_IfcRectangleProfileDef"), sampleModel.createResource()
                                        .addProperty(JunitTestUtils.hasDouble, sampleModel.createTypedLiteral(REC_PROFILE_Y_EXTENT))));
    }


    private void addHalfSpaceSolidTriples(String iri, String ifcClass) {
        sampleModel.createResource(iri)
                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + ifcClass))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "agreementFlag_IfcHalfSpaceSolid"), sampleModel.createResource()
                        .addProperty(JunitTestUtils.hasBoolean, sampleModel.createTypedLiteral(HALF_SPACE_FLAG))
                )
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "baseSurface_IfcHalfSpaceSolid"), sampleModel.createResource()
                        .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcPlane"))
                        .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "position_IfcElementarySurface"),
                                sampleModel.createResource()
                                        .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "location_IfcPlacement"), sampleModel.createResource(IFC_GEOM_SEC_POSITION_INST))
                                        .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "refDirection_IfcAxis2Placement3D"), sampleModel.createResource(IFC_PROFILE_DIR_VECTOR_INST))
                        )
                );
    }

    private void addHalfSpaceTriples() {
        addHalfSpaceSolidTriples(IFC_HALF_SPACE_INSTANCE, "IfcPolygonalBoundedHalfSpace");
        sampleModel.getResource(IFC_HALF_SPACE_INSTANCE)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "polygonalBoundary_IfcPolygonalBoundedHalfSpace"), sampleModel.createResource(IFC_POLYLINE_INSTANCE)
                        .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcPolyline")))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "position_IfcPolygonalBoundedHalfSpace"), sampleModel.createResource()
                        .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "location_IfcPlacement"), sampleModel.createResource(IFC_GEOM_POSITION_INST))
                        .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "refDirection_IfcAxis2Placement3D"), sampleModel.createResource(IFC_REF_DIR_VECTOR_INST))
                        .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "axis_IfcAxis2Placement3D"), sampleModel.createResource(IFC_AXIS_DIR_VECTOR_INST))
                );
    }

    private void addPolylineTriples(String instance, String ifcClass, String property) {
        sampleModel.createResource(instance)
                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + ifcClass))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + property),
                        sampleModel.createResource(IFC_STARTING_VERTEX)
                                .addProperty(JunitTestUtils.hasContents, sampleModel.createResource(IFC_GEOM_POSITION_INST))
                );
    }

    private void addPolylineSecondVertexTriples() {
        sampleModel.getResource(IFC_STARTING_VERTEX)
                .addProperty(JunitTestUtils.hasNext,
                        sampleModel.createResource(IFC_SEC_VERTEX)
                                .addProperty(JunitTestUtils.hasContents, sampleModel.createResource(IFC_GEOM_SEC_POSITION_INST))
                );
    }

    private void addClippingResultTriples(String instance, String firstOperand, String secOperand) {
        sampleModel.createResource(instance)
                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcBooleanClippingResult"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "operator_IfcBooleanResult"), sampleModel.createResource(TEST_OPERATOR))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "firstOperand_IfcBooleanResult"), sampleModel.createResource(firstOperand))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "secondOperand_IfcBooleanResult"), sampleModel.createResource(secOperand));
    }

    private List<String> genExpectedExtrudedAreaSolidStatements(String geomInst) {
        List<String> expected = new ArrayList<>();
        expected.add(geomInst + ", https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/ExtrudedAreaSolid");
        expected.add(geomInst + ", https://www.theworldavatar.com/kg/ontobim/hasExtrusionStartPosition, " + TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(geomInst + ", https://www.theworldavatar.com/kg/ontobim/hasExtrusionDirection, " + TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(geomInst + ", https://www.theworldavatar.com/kg/ontobim/hasExtrusionDepth, \"" + EXTRUDED_DEPTH);
        expected.add(geomInst + ", https://www.theworldavatar.com/kg/ontobim/hasExtrusionProfile, " + TEST_BASE_URI + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/RectangleProfileDefinition");
        expected.add(TEST_BASE_URI + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasProfileType, " + IFC_GEOM_PROFILE_TYPE);
        expected.add(TEST_BASE_URI + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasXDimensionExtent, \"" + REC_PROFILE_X_EXTENT);
        expected.add(TEST_BASE_URI + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasYDimensionExtent, \"" + REC_PROFILE_Y_EXTENT);
        expected.add(TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRefPoint, " + TEST_BASE_URI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }

    private List<String> genExpectedOptionalExtrudedAreaSolidStatements(String geomInst) {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRefDirection, " + TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasAxisDirection, " + TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }
}