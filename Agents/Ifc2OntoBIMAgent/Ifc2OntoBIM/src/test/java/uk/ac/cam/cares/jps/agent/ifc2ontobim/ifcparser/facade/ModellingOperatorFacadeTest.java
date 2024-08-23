package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.facade;

import org.apache.jena.rdf.model.*;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestGeometryUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.ModellingOperatorStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ModellingOperatorFacadeTest {
    private static Model sampleModel;
    private static ModellingOperatorStorage operatorMappings;
    private static final String TEST_BASE_URI = "https://www.theworldavatar.com/test/";
    private static final Property hasContents = ResourceFactory.createProperty(JunitTestUtils.listUri + "hasContents");
    private static final Property hasNext = ResourceFactory.createProperty(JunitTestUtils.listUri + "hasNext");
    private static final Property hasDouble = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasDouble");
    private static final Property hasString = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasString");
    private static final Resource IFC_CART_POINT = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcCartesianPoint");
    private static final Resource IFC_DIRECTION = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcDirection");
    private static final String IFC_FIRST_PLACEMENT_INST = TEST_BASE_URI + JunitTestUtils.IFC_PLACEMENT_CLASS + "_14823";
    private static final String IFC_SEC_PLACEMENT_INST = TEST_BASE_URI + JunitTestUtils.IFC_PLACEMENT_CLASS + "_14825";
    private static final String IFC_THIRD_PLACEMENT_INST = TEST_BASE_URI + JunitTestUtils.IFC_PLACEMENT_CLASS + "_14827";
    private static final String BIM_FIRST_PLACEMENT_INST = TEST_BASE_URI + JunitTestUtils.BIM_PLACEMENT_CLASS + "_14823";
    private static final String BIM_SEC_PLACEMENT_INST = TEST_BASE_URI + JunitTestUtils.BIM_PLACEMENT_CLASS + "_14825";
    private static final String BIM_THIRD_PLACEMENT_INST = TEST_BASE_URI + JunitTestUtils.BIM_PLACEMENT_CLASS + "_14827";
    private static final String IFC_FIRST_CART_POINT_INST = TEST_BASE_URI + "IfcCartesianPoint_31965";
    private static final Double IFC_CART_POINT_X_COORD = 5.12;
    private static final Double IFC_CART_POINT_Y_COORD = 2.13;
    private static final Double IFC_CART_POINT_Z_COORD = 7.42;
    private static final String IFC_SEC_CART_POINT_INST = TEST_BASE_URI + "IfcCartesianPoint_8157";
    private static final Double IFC_SEC_CART_POINT_X_COORD = 3.28;
    private static final Double IFC_SEC_CART_POINT_Y_COORD = 3.45;
    private static final Double IFC_SEC_CART_POINT_Z_COORD = 0.2;
    private static final String IFC_FIRST_DIR_VECTOR_INST = TEST_BASE_URI + "IfcDirection_4971";
    private static final Double IFC_DIR_VECTOR_X_RATIO = 1.0;
    private static final Double IFC_DIR_VECTOR_Y_RATIO = 2.0;
    private static final Double IFC_DIR_VECTOR_Z_RATIO = 3.0;
    private static final String IFC_SEC_DIR_VECTOR_INST = TEST_BASE_URI + "IfcDirection_6931";
    private static final Double IFC_SEC_DIR_VECTOR_X_RATIO = 5.11;
    private static final Double IFC_SEC_DIR_VECTOR_Y_RATIO = 4.02;
    private static final Double IFC_SEC_DIR_VECTOR_Z_RATIO = 0.41;
    private static final String IFC_TRANSFORMATION_OPERATOR_IRI = TEST_BASE_URI + JunitTestUtils.IFC_TRANSFORMATION_OPERATOR_CLASS + "_67547";
    private static final String BIM_TRANSFORMATION_OPERATOR_IRI = TEST_BASE_URI + JunitTestUtils.BIM_TRANSFORMATION_OPERATOR_CLASS + "_67547";
    private static final Double TRANSFORMATION_OPERATOR_SCALE = 0.291;
    private static final String TEST_SUB_CONTEXT_IRI = TEST_BASE_URI + "IfcGeometricRepresentationSubContext_4";
    private static final String TEST_SUB_CONTEXT_BIM_IRI = TEST_BASE_URI + "GeometricRepresentationSubContext_4";
    private static final String TEST_PARENT_CONTEXT_IRI = TEST_BASE_URI + "GeometricRepresentationContext_1";
    private static final String TEST_TARGET_VIEW = JunitTestUtils.ifc2x3Uri + "MODEL_VIEW";
    private static final String TEST_CONTEXT_TYPE = "Axis";
    private static final String TEST_CONTEXT_IDENTIFIER = "Model";

    @BeforeAll
    static void setUp() {
        operatorMappings = ModellingOperatorStorage.Singleton();
        NamespaceMapper.setBaseNameSpace(TEST_BASE_URI);
    }

    @BeforeEach
    void reset() {
        sampleModel = ModelFactory.createDefaultModel();
        ModellingOperatorStorage.resetSingleton();
    }

    @AfterAll
    static void resetParametersForOtherTests() {
        ModellingOperatorStorage.resetSingleton();
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testConstructorNoDuplicate() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_SEC_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_FIRST_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPointOrDirectionTriples(IFC_SEC_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPlacementTriples(IFC_FIRST_PLACEMENT_INST, IFC_FIRST_CART_POINT_INST, IFC_FIRST_DIR_VECTOR_INST, null, 1);
        addPlacementTriples(IFC_SEC_PLACEMENT_INST, IFC_SEC_CART_POINT_INST, IFC_SEC_DIR_VECTOR_INST, null, 1);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModellingOperatorFacade testHelper = new ModellingOperatorFacade(sampleModel);
        // Execute method
        testHelper.addLocalPlacementStatements(sampleModel, sampleSet);
        // Clean up results as one string
        operatorMappings.constructAllStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO, true), result);
        JunitTestUtils.doesExpectedListExist(genExpectedPlacementStatements(BIM_FIRST_PLACEMENT_INST, false, false), result);
        JunitTestUtils.doesExpectedListExist(genExpectedPlacementStatements(BIM_SEC_PLACEMENT_INST, false, false), result);
        // Verify the duplicate statements are not generated
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_SEC_CART_POINT_X_COORD, IFC_SEC_CART_POINT_Y_COORD, IFC_SEC_CART_POINT_Z_COORD), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO), result);
    }

    @Test
    void testConstructorForNoPointOrDirectionRetrieved() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_FIRST_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPointOrDirectionTriples(IFC_SEC_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModellingOperatorFacade testHelper = new ModellingOperatorFacade(sampleModel);
        // Execute method
        testHelper.addLocalPlacementStatements(sampleModel, sampleSet);
        testHelper.addCartesianTransformationOperatorStatements(sampleModel, sampleSet);
        // Construct the statements required
        operatorMappings.constructAllStatements(sampleSet);
        // Verify that no point and direction vector statements exist when there is no modelling operator linked to them
        assertEquals(0, sampleSet.size());
    }

    @Test
    void testAddLocalPlacementStatementsWithNoDirection() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPlacementTriples(IFC_FIRST_PLACEMENT_INST, IFC_FIRST_CART_POINT_INST, null, null, 0);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModellingOperatorFacade testHelper = new ModellingOperatorFacade(sampleModel);
        // Execute method
        testHelper.addLocalPlacementStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify placement statements have been generated and point statements are not generated
        JunitTestUtils.doesExpectedListExist(genExpectedPlacementStatements(BIM_FIRST_PLACEMENT_INST, false, false), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD, true), result);
        // Generate point and direction statements and check they have been generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD, true), result);
    }

    @Test
    void testAddLocalPlacementStatementsForIfcPlacement3DOnlyRefDir() {
        // Set up
        addPointOrDirectionTriples(IFC_SEC_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_SEC_CART_POINT_X_COORD, IFC_SEC_CART_POINT_Y_COORD, IFC_SEC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_SEC_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO);
        addPlacementTriples(IFC_SEC_PLACEMENT_INST, IFC_SEC_CART_POINT_INST, IFC_SEC_DIR_VECTOR_INST, null, 1);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModellingOperatorFacade testHelper = new ModellingOperatorFacade(sampleModel);
        // Execute method
        testHelper.addLocalPlacementStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify placement statements have been generated but other statements are not generated yet
        JunitTestUtils.doesExpectedListExist(genExpectedPlacementStatements(BIM_SEC_PLACEMENT_INST, true, false), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_SEC_CART_POINT_X_COORD, IFC_SEC_CART_POINT_Y_COORD, IFC_SEC_CART_POINT_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO, true), result);
        // Generate point and direction statements and check they have been generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_SEC_CART_POINT_X_COORD, IFC_SEC_CART_POINT_Y_COORD, IFC_SEC_CART_POINT_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO, true), result);

    }

    @Test
    void testAddLocalPlacementStatementsForIfcPlacement3D() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_FIRST_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPointOrDirectionTriples(IFC_SEC_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO);
        addPlacementTriples(IFC_SEC_PLACEMENT_INST, IFC_FIRST_CART_POINT_INST, IFC_FIRST_DIR_VECTOR_INST, IFC_SEC_DIR_VECTOR_INST, 1);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModellingOperatorFacade testHelper = new ModellingOperatorFacade(sampleModel);
        // Execute method
        testHelper.addLocalPlacementStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify placement statements have been generated but other statements are not generated yet
        JunitTestUtils.doesExpectedListExist(genExpectedPlacementStatements(BIM_SEC_PLACEMENT_INST, true, false), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO, true), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO, true), result);
        // Generate point and direction statements and check they have been generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO, true), result);
    }

    @Test
    void testAddLocalPlacementStatementsForIfcPlacement2D() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_FIRST_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPlacementTriples(IFC_THIRD_PLACEMENT_INST, IFC_FIRST_CART_POINT_INST, IFC_FIRST_DIR_VECTOR_INST, null, 2);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModellingOperatorFacade testHelper = new ModellingOperatorFacade(sampleModel);
        // Execute method
        testHelper.addLocalPlacementStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify placement statements have been generated but other statements are not generated yet
        JunitTestUtils.doesExpectedListExist(genExpectedPlacementStatements(BIM_THIRD_PLACEMENT_INST, true, false), result);// Clean up results as one string
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO, true), result);
        // Generate point and direction statements and check they have been generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO, true), result);
    }

    @Test
    void testAddCartesianTransformationOperatorStatements() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_FIRST_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPointOrDirectionTriples(IFC_SEC_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO);
        addTransformationOperatorTriples(IFC_TRANSFORMATION_OPERATOR_IRI, IFC_FIRST_CART_POINT_INST, IFC_FIRST_DIR_VECTOR_INST, IFC_SEC_DIR_VECTOR_INST, TRANSFORMATION_OPERATOR_SCALE);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModellingOperatorFacade testHelper = new ModellingOperatorFacade(sampleModel);
        // Execute method
        testHelper.addCartesianTransformationOperatorStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify transformation operators statements have been generated but other statements are not generated yet
        JunitTestUtils.doesExpectedListExist(genExpectedTransformationOperatorStatements(BIM_TRANSFORMATION_OPERATOR_IRI, true), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO, true), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO, true), result);
        // Generate point and direction statements and check they have been generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO, true), result);
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO, true), result);
    }

    @Test
    void testAddCartesianTransformationOperatorStatementsWithOptionalValues() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_FIRST_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPointOrDirectionTriples(IFC_SEC_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO);
        addTransformationOperatorTriples(IFC_TRANSFORMATION_OPERATOR_IRI, IFC_FIRST_CART_POINT_INST, null, null, null);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModellingOperatorFacade testHelper = new ModellingOperatorFacade(sampleModel);
        // Execute method
        testHelper.addCartesianTransformationOperatorStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Verify transformation operators statements have been generated but other statements are not generated yet
        JunitTestUtils.doesExpectedListExist(genExpectedTransformationOperatorStatements(BIM_TRANSFORMATION_OPERATOR_IRI, false), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD), result);
        // Generate point and direction statements and check they have been generated
        operatorMappings.constructAllStatements(sampleSet);
        result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(JunitTestGeometryUtils.genExpectedPointStatements(TEST_BASE_URI, IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD, true), result);
        // The null results should not be retrieved
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO), result);
        JunitTestUtils.doesExpectedListNotExist(JunitTestGeometryUtils.genExpectedDirectionStatements(TEST_BASE_URI, IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO), result);
    }

    @Test
    void testAddGeometricRepresentationSubContextStatements() {
        // Set up
        addSubContextTriples(TEST_SUB_CONTEXT_IRI);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        ModellingOperatorFacade testHelper = new ModellingOperatorFacade(sampleModel);
        // Execute method
        testHelper.addGeometricRepresentationSubContextStatements(sampleModel, sampleSet);
        // Clean up results as one string
        operatorMappings.constructAllStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedSubContextStatements(), result);
    }

    private void addPointOrDirectionTriples(String iri, Resource ifcClass, String property, Double xVal, Double yVal, Double zVal) {
        sampleModel.createResource(iri)
                .addProperty(JunitTestUtils.RDF_TYPE, ifcClass)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + property), sampleModel.createResource()
                        .addProperty(hasContents, sampleModel.createResource()
                                .addProperty(hasDouble, sampleModel.createTypedLiteral(xVal)))
                        .addProperty(hasNext, sampleModel.createResource()
                                .addProperty(hasContents, sampleModel.createResource()
                                        .addProperty(hasDouble, sampleModel.createTypedLiteral(yVal)))
                                .addProperty(hasNext, sampleModel.createResource()
                                        .addProperty(hasContents, sampleModel.createResource()
                                                .addProperty(hasDouble, sampleModel.createTypedLiteral(zVal)))
                                )
                        )
                );
    }

    private void addPlacementTriples(String iri, String pointIri, String refDirectionIri, String axisDirectionIri, Integer pattern) {
        Resource axisPlacementNode = sampleModel.createResource();
        sampleModel.createResource(iri)
                .addProperty(JunitTestUtils.RDF_TYPE,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcLocalPlacement"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relativePlacement_IfcLocalPlacement"), axisPlacementNode
                        .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "location_IfcPlacement"),
                                sampleModel.getResource(pointIri).addProperty(JunitTestUtils.RDF_TYPE, IFC_CART_POINT))
                );
        switch (pattern) {
            case 1:
                if (refDirectionIri != null) {
                    sampleModel.add(axisPlacementNode, sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "refDirection_IfcAxis2Placement3D"),
                            sampleModel.getResource(refDirectionIri));
                }
                if (axisDirectionIri != null) {
                    sampleModel.add(axisPlacementNode, sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "axis_IfcAxis2Placement3D"),
                            sampleModel.getResource(axisDirectionIri));
                }
            case 2:
                sampleModel.add(axisPlacementNode, sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "refDirection_IfcAxis2Placement2D"),
                        sampleModel.getResource(refDirectionIri));
        }
    }

    private void addTransformationOperatorTriples(String iri, String pointIri, String xDirectionIri, String yDirectionIri, Double scale) {
        sampleModel.createResource(iri)
                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcCartesianTransformationOperator3D"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "localOrigin_IfcCartesianTransformationOperator"), sampleModel.getResource(pointIri));
        if (xDirectionIri!=null){
            sampleModel.createResource(iri)
                    .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "axis1_IfcCartesianTransformationOperator"), sampleModel.getResource(xDirectionIri))
                    .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "axis2_IfcCartesianTransformationOperator"), sampleModel.getResource(yDirectionIri))
                    .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "scale_IfcCartesianTransformationOperator"),
                            sampleModel.createResource().addProperty(hasDouble, sampleModel.createTypedLiteral(scale))
                    );
        }
    }

    private void addSubContextTriples(String iri) {
        sampleModel.createResource(iri)
                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcGeometricRepresentationSubContext"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "parentContext_IfcGeometricRepresentationSubContext"),
                        sampleModel.createResource(TEST_PARENT_CONTEXT_IRI).addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcGeometricRepresentationContext"))
                )
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "contextType_IfcRepresentationContext"),
                        sampleModel.createResource().addProperty(hasString, sampleModel.createLiteral(TEST_CONTEXT_TYPE)))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "contextIdentifier_IfcRepresentationContext"),
                        sampleModel.createResource().addProperty(hasString, sampleModel.createLiteral(TEST_CONTEXT_IDENTIFIER)))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "targetView_IfcGeometricRepresentationSubContext"),  sampleModel.createResource(TEST_TARGET_VIEW));
    }

    private List<String> genExpectedPlacementStatements(String placementInst, boolean hasRefDir, boolean hasAxisDir) {
        List<String> expected = new ArrayList<>();
        expected.add(placementInst + ", " + JunitTestUtils.RDF_TYPE + ", https://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(placementInst + ", https://www.theworldavatar.com/kg/ontobim/hasRefPoint, " + TEST_BASE_URI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        if (hasRefDir) {
            expected.add(placementInst + ", https://www.theworldavatar.com/kg/ontobim/hasRefDirection, " + TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        }
        if (hasAxisDir) {
            expected.add(placementInst + ", https://www.theworldavatar.com/kg/ontobim/hasAxisDirection, " + TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        }
        return expected;
    }

    private List<String> genExpectedTransformationOperatorStatements(String inst, boolean hasOptional) {
        List<String> expected = new ArrayList<>();
        expected.add(inst + ", " + JunitTestUtils.RDF_TYPE + ", https://www.theworldavatar.com/kg/ontobim/CartesianTransformationOperator");
        expected.add(inst + ", https://www.theworldavatar.com/kg/ontobim/hasLocalOrigin, " + TEST_BASE_URI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        if (hasOptional) {
            expected.add(inst + ", https://www.theworldavatar.com/kg/ontobim/hasScale, \"" + TRANSFORMATION_OPERATOR_SCALE);
            expected.add(inst + ", https://www.theworldavatar.com/kg/ontobim/hasDerivedXAxis, " + TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
            expected.add(inst + ", https://www.theworldavatar.com/kg/ontobim/hasDerivedYAxis, " + TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        }
        return expected;
    }

    private List<String> genExpectedSubContextStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_SUB_CONTEXT_BIM_IRI + ", " + JunitTestUtils.RDF_TYPE + ", https://www.theworldavatar.com/kg/ontobim/GeometricRepresentationSubContext");
        expected.add(TEST_SUB_CONTEXT_BIM_IRI + ", https://www.theworldavatar.com/kg/ontobim/hasParentContext, " + TEST_PARENT_CONTEXT_IRI);
        expected.add(TEST_SUB_CONTEXT_BIM_IRI + ", https://www.theworldavatar.com/kg/ontobim/hasContextType, \"" + TEST_CONTEXT_TYPE);
        expected.add(TEST_SUB_CONTEXT_BIM_IRI + ", https://www.theworldavatar.com/kg/ontobim/hasContextIdentifier, \"" + TEST_CONTEXT_IDENTIFIER);
        expected.add(TEST_SUB_CONTEXT_BIM_IRI + ", https://www.theworldavatar.com/kg/ontobim/hasTargetView, " + TEST_TARGET_VIEW);
        return expected;
    }
}