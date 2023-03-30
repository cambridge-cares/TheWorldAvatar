package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.facade;

import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;
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
    private static final String TEST_BASE_URI = "http://www.theworldavatar.com/test/";
    private static final Resource IFC_DIRECTION = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcDirection");
    // Extruded area solid fields
    private static final String IFC_EXTRUDED_AREA_SOLID_INST = TEST_BASE_URI + "IfcExtrudedAreaSolid_14823";
    private static final String BIM_EXTRUDED_AREA_SOLID_INST = TEST_BASE_URI + "ExtrudedAreaSolid_14823";
    private static final String IFC_GEOM_PROFILE_TYPE = JunitTestUtils.ifc2x3Uri + "AREA";
    private static final Double IFC_PROFILE_DIR_VECTOR_X_RATIO = 1.2;
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
    // Polyline fields
    private static final String IFC_POLYLINE_INSTANCE = TEST_BASE_URI + "IfcPolyline_777";
    private static final String BIM_POLYLINE_INSTANCE = TEST_BASE_URI + "Polyline_777";
    private static final String IFC_STARTING_VERTEX = TEST_BASE_URI + "IfcCartesianPoint_List_778";
    private static final String BIM_STARTING_VERTEX = TEST_BASE_URI + "LineVertex_778";
    private static final String IFC_SEC_VERTEX = TEST_BASE_URI + "IfcCartesianPoint_List_779";
    private static final String BIM_SEC_VERTEX = TEST_BASE_URI + "LineVertex_779";


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
        JunitTestUtils.doesExpectedListExist(genExpectedPointStatements(IFC_GEOM_POSITION_X_COORD, IFC_GEOM_POSITION_Y_COORD, IFC_GEOM_POSITION_Z_COORD), result);
        JunitTestUtils.doesExpectedListExist(genExpectedDirectionStatements(IFC_REF_DIR_VECTOR_X_RATIO, IFC_REF_DIR_VECTOR_Y_RATIO, IFC_REF_DIR_VECTOR_Z_RATIO), result);
        JunitTestUtils.doesExpectedListExist(genExpectedDirectionStatements(IFC_AXIS_DIR_VECTOR_X_RATIO, IFC_AXIS_DIR_VECTOR_Y_RATIO, IFC_AXIS_DIR_VECTOR_Z_RATIO), result);
        JunitTestUtils.doesExpectedListExist(genExpectedPointStatements(IFC_GEOM_SEC_POSITION_X_COORD, IFC_GEOM_SEC_POSITION_Y_COORD, IFC_GEOM_SEC_POSITION_Z_COORD), result);
        JunitTestUtils.doesExpectedListExist(genExpectedDirectionStatements(IFC_PROFILE_DIR_VECTOR_X_RATIO, IFC_PROFILE_DIR_VECTOR_Y_RATIO, IFC_PROFILE_DIR_VECTOR_Z_RATIO), result);
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
        JunitTestUtils.doesExpectedListExist(genExpectedPointStatements(IFC_GEOM_POSITION_X_COORD, IFC_GEOM_POSITION_Y_COORD, IFC_GEOM_POSITION_Z_COORD), result);
        JunitTestUtils.doesExpectedListExist(genExpectedPointStatements(IFC_GEOM_SEC_POSITION_X_COORD, IFC_GEOM_SEC_POSITION_Y_COORD, IFC_GEOM_SEC_POSITION_Z_COORD), result);
        // Direction for extruded direction will be generated
        JunitTestUtils.doesExpectedListExist(genExpectedDirectionStatements(IFC_REF_DIR_VECTOR_X_RATIO, IFC_REF_DIR_VECTOR_Y_RATIO, IFC_REF_DIR_VECTOR_Z_RATIO), result);
        // Verify no directions are generated
        JunitTestUtils.doesExpectedListNotExist(genExpectedDirectionStatements(IFC_AXIS_DIR_VECTOR_X_RATIO, IFC_AXIS_DIR_VECTOR_Y_RATIO, IFC_AXIS_DIR_VECTOR_Z_RATIO), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedDirectionStatements(IFC_PROFILE_DIR_VECTOR_X_RATIO, IFC_PROFILE_DIR_VECTOR_Y_RATIO, IFC_PROFILE_DIR_VECTOR_Z_RATIO), result);
    }

    @Test
    void testAddPolylineStatements() {
        // Set up
        addPolylineTriples();
        addPolylineSecondVertexTriples();
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addPolylineStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedCommonPolylineStatements(BIM_POLYLINE_INSTANCE, BIM_STARTING_VERTEX, operatorMappings.getPoint(IFC_GEOM_POSITION_INST).getIri()), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedNextLineVertexStatements(BIM_STARTING_VERTEX, BIM_SEC_VERTEX, operatorMappings.getPoint(IFC_GEOM_SEC_POSITION_INST).getIri()), result);
        // Verify that the right points are generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(genExpectedPointStatements(IFC_GEOM_POSITION_X_COORD, IFC_GEOM_POSITION_Y_COORD, IFC_GEOM_POSITION_Z_COORD), result);
        JunitTestUtils.doesExpectedListExist(genExpectedPointStatements(IFC_GEOM_SEC_POSITION_X_COORD, IFC_GEOM_SEC_POSITION_Y_COORD, IFC_GEOM_SEC_POSITION_Z_COORD), result);
    }

    @Test
    void testAddPolylineStatementsOneVertex() {
        // Set up
        addPolylineTriples();
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        testHelper.addPolylineStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify statements have been generated
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedCommonPolylineStatements(BIM_POLYLINE_INSTANCE, BIM_STARTING_VERTEX, operatorMappings.getPoint(IFC_GEOM_POSITION_INST).getIri()), result);
        // Verify the next vertex is not generated since it doesn't exist
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedNextLineVertexStatements(BIM_STARTING_VERTEX, BIM_SEC_VERTEX, operatorMappings.getPoint(IFC_GEOM_SEC_POSITION_INST).getIri()), result);
        // Verify that the right points are generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(genExpectedPointStatements(IFC_GEOM_POSITION_X_COORD, IFC_GEOM_POSITION_Y_COORD, IFC_GEOM_POSITION_Z_COORD), result);
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
                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcExtrudedAreaSolid"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "position_IfcSweptAreaSolid"), extrudedPosition)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "extrudedDirection_IfcExtrudedAreaSolid"), sampleModel.createResource(IFC_REF_DIR_VECTOR_INST)
                        .addProperty(RDF.type, IFC_DIRECTION))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "depth_IfcExtrudedAreaSolid"), sampleModel.createResource()
                        .addProperty(JunitTestUtils.hasDouble, sampleModel.createTypedLiteral(EXTRUDED_DEPTH)))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "sweptArea_IfcSweptAreaSolid"),
                        sampleModel.createResource()
                                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRectangleProfileDef"))
                                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "profileType_IfcProfileDef"), sampleModel.createResource(IFC_GEOM_PROFILE_TYPE))
                                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "position_IfcParameterizedProfileDef"), profilePosition)
                                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "xDim_IfcRectangleProfileDef"), sampleModel.createResource()
                                        .addProperty(JunitTestUtils.hasDouble, sampleModel.createTypedLiteral(REC_PROFILE_X_EXTENT)))
                                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "yDim_IfcRectangleProfileDef"), sampleModel.createResource()
                                        .addProperty(JunitTestUtils.hasDouble, sampleModel.createTypedLiteral(REC_PROFILE_Y_EXTENT))));
    }


    private void addPolylineTriples() {
        sampleModel.createResource(IFC_POLYLINE_INSTANCE)
                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcPolyline"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "points_IfcPolyline"),
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

    private List<String> genExpectedExtrudedAreaSolidStatements(String geomInst) {
        List<String> expected = new ArrayList<>();
        expected.add(geomInst + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/ExtrudedAreaSolid");
        expected.add(geomInst + ", http://www.theworldavatar.com/kg/ontobim/hasExtrusionStartPosition, " + TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(geomInst + ", http://www.theworldavatar.com/kg/ontobim/hasExtrusionDirection, " + TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(geomInst + ", http://www.theworldavatar.com/kg/ontobim/hasExtrusionDepth, \"" + EXTRUDED_DEPTH);
        expected.add(geomInst + ", http://www.theworldavatar.com/kg/ontobim/hasExtrusionProfile, " + TEST_BASE_URI + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/RectangleProfileDefinition");
        expected.add(TEST_BASE_URI + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasProfileType, " + IFC_GEOM_PROFILE_TYPE);
        expected.add(TEST_BASE_URI + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasXDimensionExtent, \"" + REC_PROFILE_X_EXTENT);
        expected.add(TEST_BASE_URI + "RectangleProfileDefinition_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasYDimensionExtent, \"" + REC_PROFILE_Y_EXTENT);
        expected.add(TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefPoint, " + TEST_BASE_URI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }

    private List<String> genExpectedOptionalExtrudedAreaSolidStatements(String geomInst) {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefDirection, " + TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "LocalPlacement_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasAxisDirection, " + TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }

    private List<String> genExpectedPointStatements(Double xCoord, Double yCoord, Double zCoord) {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasXCoordinate, \"" + xCoord);
        expected.add(TEST_BASE_URI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasYCoordinate, \"" + yCoord);
        if (zCoord != null) {
            expected.add(TEST_BASE_URI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasZCoordinate, \"" + zCoord);
        }
        return expected;
    }

    private List<String> genExpectedDirectionStatements(Double xDir, Double yDir, Double zDir) {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasXDirectionRatio, \"" + xDir);
        expected.add(TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasYDirectionRatio, \"" + yDir);
        if (zDir != null) {
            expected.add(TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasZDirectionRatio, \"" + zDir);
        }
        return expected;
    }
}