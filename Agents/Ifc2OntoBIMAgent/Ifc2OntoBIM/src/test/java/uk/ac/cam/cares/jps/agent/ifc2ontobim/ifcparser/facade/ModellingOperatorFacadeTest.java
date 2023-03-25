package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.facade;

import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
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
    private static final String TEST_BASE_URI = "http://www.theworldavatar.com/test/";
    private static final Property hasContents = ResourceFactory.createProperty(JunitTestUtils.listUri + "hasContents");
    private static final Property hasNext = ResourceFactory.createProperty(JunitTestUtils.listUri + "hasNext");
    private static final Property hasDouble = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasDouble");
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
    void testRetrieveOperatorInstancesForPlacementWithNoDirection() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_SEC_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_SEC_CART_POINT_X_COORD, IFC_SEC_CART_POINT_Y_COORD, IFC_SEC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_FIRST_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPointOrDirectionTriples(IFC_SEC_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO);
        addPlacementTriples(IFC_FIRST_PLACEMENT_INST, IFC_FIRST_CART_POINT_INST, null, null, 0);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        ModellingOperatorFacade.retrieveOperatorInstances(sampleModel);
        // Clean up results as one string
        operatorMappings.constructAllStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedPointStatements(IFC_CART_POINT_X_COORD.toString(), IFC_CART_POINT_Y_COORD.toString(), IFC_CART_POINT_Z_COORD.toString()), result);
        JunitTestUtils.doesExpectedListExist(genExpectedPlacementStatements(BIM_FIRST_PLACEMENT_INST, false, false), result);
    }

    @Test
    void testRetrieveOperatorInstancesForIfcPlacement3DOnlyRefPDir() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_SEC_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_SEC_CART_POINT_X_COORD, IFC_SEC_CART_POINT_Y_COORD, IFC_SEC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_FIRST_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPointOrDirectionTriples(IFC_SEC_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO);
        addPlacementTriples(IFC_SEC_PLACEMENT_INST, IFC_SEC_CART_POINT_INST, IFC_SEC_DIR_VECTOR_INST, null, 1);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        ModellingOperatorFacade.retrieveOperatorInstances(sampleModel);
        // Clean up results as one string
        operatorMappings.constructAllStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedPointStatements(IFC_SEC_CART_POINT_X_COORD.toString(), IFC_SEC_CART_POINT_Y_COORD.toString(), IFC_SEC_CART_POINT_Z_COORD.toString()), result);
        JunitTestUtils.doesExpectedListExist(genExpectedDirectionStatements(IFC_SEC_DIR_VECTOR_X_RATIO.toString(), IFC_SEC_DIR_VECTOR_Y_RATIO.toString(), IFC_SEC_DIR_VECTOR_Z_RATIO.toString()), result);
        JunitTestUtils.doesExpectedListExist(genExpectedPlacementStatements(BIM_SEC_PLACEMENT_INST, true, false), result);
    }

    @Test
    void testRetrieveOperatorInstancesForIfcPlacement3D() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_SEC_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_SEC_CART_POINT_X_COORD, IFC_SEC_CART_POINT_Y_COORD, IFC_SEC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_FIRST_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPointOrDirectionTriples(IFC_SEC_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO);
        addPlacementTriples(IFC_SEC_PLACEMENT_INST, IFC_FIRST_CART_POINT_INST, IFC_FIRST_DIR_VECTOR_INST, IFC_SEC_DIR_VECTOR_INST, 1);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        ModellingOperatorFacade.retrieveOperatorInstances(sampleModel);
        // Clean up results as one string
        operatorMappings.constructAllStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedPointStatements(IFC_CART_POINT_X_COORD.toString(), IFC_CART_POINT_Y_COORD.toString(), IFC_CART_POINT_Z_COORD.toString()), result);
        JunitTestUtils.doesExpectedListExist(genExpectedDirectionStatements(IFC_DIR_VECTOR_X_RATIO.toString(), IFC_DIR_VECTOR_Y_RATIO.toString(), IFC_DIR_VECTOR_Z_RATIO.toString()), result);
        JunitTestUtils.doesExpectedListExist(genExpectedDirectionStatements(IFC_SEC_DIR_VECTOR_X_RATIO.toString(), IFC_SEC_DIR_VECTOR_Y_RATIO.toString(), IFC_SEC_DIR_VECTOR_Z_RATIO.toString()), result);
        JunitTestUtils.doesExpectedListExist(genExpectedPlacementStatements(BIM_SEC_PLACEMENT_INST, true, false), result);
    }

    @Test
    void testRetrieveOperatorInstancesForIfcPlacement2D() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_SEC_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_SEC_CART_POINT_X_COORD, IFC_SEC_CART_POINT_Y_COORD, IFC_SEC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_FIRST_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPointOrDirectionTriples(IFC_SEC_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO);
        addPlacementTriples(IFC_THIRD_PLACEMENT_INST, IFC_FIRST_CART_POINT_INST, IFC_FIRST_DIR_VECTOR_INST, null, 2);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        ModellingOperatorFacade.retrieveOperatorInstances(sampleModel);
        // Clean up results as one string
        operatorMappings.constructAllStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedPointStatements(IFC_CART_POINT_X_COORD.toString(), IFC_CART_POINT_Y_COORD.toString(), IFC_CART_POINT_Z_COORD.toString()), result);
        JunitTestUtils.doesExpectedListExist(genExpectedDirectionStatements(IFC_DIR_VECTOR_X_RATIO.toString(), IFC_DIR_VECTOR_Y_RATIO.toString(), IFC_DIR_VECTOR_Z_RATIO.toString()), result);
        JunitTestUtils.doesExpectedListExist(genExpectedPlacementStatements(BIM_THIRD_PLACEMENT_INST, true, false), result);
    }

    @Test
    void testRetrieveOperatorInstancesNoDuplicate() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_SEC_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_FIRST_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPointOrDirectionTriples(IFC_SEC_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPlacementTriples(IFC_FIRST_PLACEMENT_INST, IFC_FIRST_CART_POINT_INST, IFC_FIRST_DIR_VECTOR_INST, null, 1);
        addPlacementTriples(IFC_SEC_PLACEMENT_INST, IFC_SEC_CART_POINT_INST, IFC_SEC_DIR_VECTOR_INST, null, 1);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        ModellingOperatorFacade.retrieveOperatorInstances(sampleModel);
        // Clean up results as one string
        operatorMappings.constructAllStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generate expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedPointStatements(IFC_CART_POINT_X_COORD.toString(), IFC_CART_POINT_Y_COORD.toString(), IFC_CART_POINT_Z_COORD.toString()), result);
        JunitTestUtils.doesExpectedListExist(genExpectedDirectionStatements(IFC_DIR_VECTOR_X_RATIO.toString(), IFC_DIR_VECTOR_Y_RATIO.toString(), IFC_DIR_VECTOR_Z_RATIO.toString()), result);
        JunitTestUtils.doesExpectedListExist(genExpectedPlacementStatements(BIM_FIRST_PLACEMENT_INST, false, false), result);
        JunitTestUtils.doesExpectedListExist(genExpectedPlacementStatements(BIM_SEC_PLACEMENT_INST, false, false), result);
        // Verify the duplicate statements are not generated
        JunitTestUtils.doesExpectedListNotExist(genExpectedPointStatements(IFC_SEC_CART_POINT_X_COORD.toString(), IFC_SEC_CART_POINT_Y_COORD.toString(), IFC_SEC_CART_POINT_Z_COORD.toString()), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedDirectionStatements(IFC_SEC_DIR_VECTOR_X_RATIO.toString(), IFC_SEC_DIR_VECTOR_Y_RATIO.toString(), IFC_SEC_DIR_VECTOR_Z_RATIO.toString()), result);
    }

    @Test
    void testRetrieveOperatorInstancesNoPlacement() {
        // Set up
        addPointOrDirectionTriples(IFC_FIRST_CART_POINT_INST, IFC_CART_POINT, "coordinates_IfcCartesianPoint", IFC_CART_POINT_X_COORD, IFC_CART_POINT_Y_COORD, IFC_CART_POINT_Z_COORD);
        addPointOrDirectionTriples(IFC_FIRST_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_DIR_VECTOR_X_RATIO, IFC_DIR_VECTOR_Y_RATIO, IFC_DIR_VECTOR_Z_RATIO);
        addPointOrDirectionTriples(IFC_SEC_DIR_VECTOR_INST, IFC_DIRECTION, "directionRatios_IfcDirection", IFC_SEC_DIR_VECTOR_X_RATIO, IFC_SEC_DIR_VECTOR_Y_RATIO, IFC_SEC_DIR_VECTOR_Z_RATIO);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        ModellingOperatorFacade.retrieveOperatorInstances(sampleModel);
        // Construct the statements required
        operatorMappings.constructAllStatements(sampleSet);
        // Verify that no point and direction vector statements exist when there is no local placement
        assertEquals(0, sampleSet.size());
    }

    private void addPointOrDirectionTriples(String iri, Resource ifcClass, String property, Double xVal, Double yVal, Double zVal) {
        sampleModel.createResource(iri)
                .addProperty(RDF.type, ifcClass)
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
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcLocalPlacement"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relativePlacement_IfcLocalPlacement"), axisPlacementNode
                        .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "location_IfcPlacement"),
                                sampleModel.getResource(pointIri).addProperty(RDF.type, IFC_CART_POINT))
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


    private List<String> genExpectedPointStatements(String xCoord, String yCoord, String zCoord) {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasXCoordinate, \"" + xCoord);
        expected.add(TEST_BASE_URI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasYCoordinate, \"" + yCoord);
        if (zCoord != null) {
            expected.add(TEST_BASE_URI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasZCoordinate, \"" + zCoord);
        }
        return expected;
    }

    private List<String> genExpectedDirectionStatements(String xDir, String yDir, String zDir) {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasXDirectionRatio, \"" + xDir);
        expected.add(TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasYDirectionRatio, \"" + yDir);
        if (zDir != null) {
            expected.add(TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasZDirectionRatio, \"" + zDir);
        }
        return expected;
    }

    private List<String> genExpectedPlacementStatements(String placementInst, boolean hasRefDir, boolean hasAxisDir) {
        List<String> expected = new ArrayList<>();
        expected.add(placementInst + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(placementInst + ", http://www.theworldavatar.com/kg/ontobim/hasRefPoint, " + TEST_BASE_URI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/CartesianPoint");
        if (hasRefDir) {
            expected.add(placementInst + ", http://www.theworldavatar.com/kg/ontobim/hasRefDirection, " + TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
            expected.add(TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/DirectionVector");
        }
        if (hasAxisDir) {
            expected.add(placementInst + ", http://www.theworldavatar.com/kg/ontobim/hasAxisDirection, " + TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
            expected.add(TEST_BASE_URI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/DirectionVector");
        }
        return expected;
    }
}