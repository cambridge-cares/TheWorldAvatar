package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.base.Sys;
import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcStoreyRepresentation;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

class BuildingStructureFacadeTest {
    private static Model sampleModel;
    private static final String TEST_BASE_URI = "http://www.theworldavatar.com/test/";
    private static final String DOOR_INST = TEST_BASE_URI + "IfcDoor_5232";
    private static final String STOREY_INST = TEST_BASE_URI + "IfcBuildingStorey_3294";
    private static final String REL_AGG_INST = TEST_BASE_URI + "IfcRelAggregate_29214";
    private static final String DOOR_ID = "01294juas";
    private static final String DOOR_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_1041";
    private static final String DOOR_NAME = "Iron door";
    private static final String IFC_PRODUCT_DEF_SHAPE_INST = TEST_BASE_URI + "IfcProductDefinitionShape_3140";
    private static final String IFC_SHAPE_REP_INST = TEST_BASE_URI + "IfcShapeRepresentation_5108";
    private static final String IFC_MAPPED_SHAPE_REP = TEST_BASE_URI + "IfcShapeRepresentation_8672";
    private static final String IFC_GEOM_SUB_CONTEXT_INST = TEST_BASE_URI + "IfcGeometricRepresentationSubContext_5108";
    private static final String IFC_FACETED_BREP_INST = TEST_BASE_URI + "IfcFacetedBrep_726358";
    private static final String IFC_REP_TYPE_VAL = "FacetedBrep";
    private static final String IFC_MAPPED_REP_TYPE_VAL = "MappedRepresentation";
    private static final String IFC_SOURCE_PLACEMENT_INST = TEST_BASE_URI + "IfcLocalPlacement_571261";
    private static final String IFC_TARGET_TRANSFORMATION_OPERATOR_INST = TEST_BASE_URI + "IfcCartesianTransformationOperator3D_3098157";
    private static final String IFC_MAPPED_ITEM_INST = TEST_BASE_URI + "IfcMappedItem_185218";
    private static final String IFC_REP_MAP_INST = TEST_BASE_URI + "IfcRepresentationMap_3213";
    // Properties
    private static final Property hasString = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasString");
    private static final Property hasContents = ResourceFactory.createProperty(JunitTestUtils.listUri + "hasContents");
    private static final Property hasName = ResourceFactory.createProperty(JunitTestUtils.IFC2X3_NAME_PROPERTY);
    private static final Property hasId = ResourceFactory.createProperty(JunitTestUtils.IFC2X3_ID_PROPERTY);
    private static final Property objectPlacement = ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "objectPlacement_IfcProduct");
    // Repeated classes
    private static final Resource localPlacement = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcLocalPlacement");
    private static final Resource productDefShape = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcProductDefinitionShape");
    private static final Resource shapeRep = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcShapeRepresentation");
    private static final Resource geomRepSubContext = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcGeometricRepresentationSubContext");
    private static final Resource facetedBrep = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcFacetedBrep");

    @BeforeAll
    static void addTestZoneMappings() {
        // Create a new storey instance, which does not require any values except for the IRI
        // This IRI is necessary to generate the Storey IRI within the element class
        IfcStoreyRepresentation storey = new IfcStoreyRepresentation(STOREY_INST, null, null, null, null, null);
        // Add the storey to the singleton
        SpatialZoneStorage zoneMappings = SpatialZoneStorage.Singleton();
        zoneMappings.add(STOREY_INST, storey);
    }

    @BeforeEach
    void genSampleStatements() {
        sampleModel = ModelFactory.createDefaultModel();
        Resource doorNameBlankNode = sampleModel.createResource();
        Resource doorIDBlankNode = sampleModel.createResource();
        Resource doorPositionNode = sampleModel.createResource(DOOR_POSITION_INST).addProperty(RDF.type, localPlacement);
        Resource doorInst = sampleModel.createResource(DOOR_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcDoor"))
                .addProperty(hasName, doorNameBlankNode)
                .addProperty(hasId, doorIDBlankNode)
                .addProperty(objectPlacement, doorPositionNode);
        Resource storeyInst = sampleModel.createResource(STOREY_INST)
                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcBuildingStorey"));
        sampleModel.add(doorNameBlankNode, hasString, ResourceFactory.createPlainLiteral(DOOR_NAME));
        sampleModel.add(doorIDBlankNode, hasString, ResourceFactory.createPlainLiteral(DOOR_ID));
        sampleModel.createResource(REL_AGG_INST)
                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelContainedInSpatialStructure"))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.IFC2X3_HOST_ZONE_PROPERTY), storeyInst)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.IFC2X3_CONTAIN_ELEMENT_PROPERTY), doorInst);
    }

    @AfterAll
    static void resetZoneMappingsForOtherTests() {
        SpatialZoneStorage.resetSingleton();
    }

    @Test
    void testGenZoneTriplesNonMappedGeometryRepresentation() {
        // Set up
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(DOOR_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        BuildingStructureFacade.genZoneTriples(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(), result);
        // The following statements are optional and should not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testGenZoneTriplesMappedGeometryRepresentation() {
        // Set up
        // Generate the alternate mapped geometry representation triples
        addMappedGeometryTriples(sampleModel.getResource(DOOR_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        BuildingStructureFacade.genZoneTriples(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalGeomStatements(), result);
    }

    private List<String> genExpectedStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "Storey_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://w3id.org/bot#containsElement, " + TEST_BASE_URI + "Door_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Door_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobuildingstructure/Door");
        expected.add(TEST_BASE_URI + "Door_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/IfcModelRepresentation");
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + DOOR_NAME);
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + DOOR_ID);
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + DOOR_POSITION_INST);
        expected.add(DOOR_POSITION_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        // Geometry representation triples
        expected.add(TEST_BASE_URI + "Door_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasGeometricRepresentation, " + TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/ModelRepresentation3D");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSubContext, " + IFC_GEOM_SUB_CONTEXT_INST);
        expected.add(IFC_GEOM_SUB_CONTEXT_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/GeometricRepresentationSubContext");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + IFC_FACETED_BREP_INST);
        expected.add(IFC_FACETED_BREP_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, " + facetedBrep.toString());
        return expected;
    }

    private List<String> genExpectedGeomRepTypeStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationType, \"" + IFC_REP_TYPE_VAL);
        return expected;
    }

    private List<String> genExpectedOptionalGeomStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSourcePlacement, " + IFC_SOURCE_PLACEMENT_INST);
        expected.add(IFC_SOURCE_PLACEMENT_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasTargetPlacement, " + IFC_TARGET_TRANSFORMATION_OPERATOR_INST);
        expected.add(IFC_TARGET_TRANSFORMATION_OPERATOR_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/CartesianTransformationOperator");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationType, \"" + IFC_MAPPED_REP_TYPE_VAL);
        return expected;
    }

    private void addGeometryTriples(Resource element) {
        Resource shapeRepBlankNode = sampleModel.createResource();
        sampleModel.add(element, ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representation_IfcProduct"),
                // Create object resource and its properties
                sampleModel.createResource(IFC_PRODUCT_DEF_SHAPE_INST)
                        .addProperty(RDF.type, productDefShape)
                        .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representations_IfcProductRepresentation"), shapeRepBlankNode));
        sampleModel.add(shapeRepBlankNode, hasContents,
                // Create object resource and its properties
                sampleModel.createResource(IFC_SHAPE_REP_INST)
                        .addProperty(RDF.type, shapeRep)
                        .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representationType_IfcRepresentation"), // Blank Node
                                sampleModel.createResource().addProperty(hasString, IFC_REP_TYPE_VAL)) // Blank Node's properties
                        .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "contextOfItems_IfcRepresentation"),
                                sampleModel.createResource(IFC_GEOM_SUB_CONTEXT_INST).addProperty(RDF.type, geomRepSubContext))
                        .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "items_IfcRepresentation"),
                                sampleModel.createResource(IFC_FACETED_BREP_INST).addProperty(RDF.type, facetedBrep)));
    }

    private void addMappedGeometryTriples(Resource element) {
        Resource shapeRepBlankNode = sampleModel.createResource();
        sampleModel.add(element, ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representation_IfcProduct"),
                // Create object resource and its properties
                sampleModel.createResource(IFC_PRODUCT_DEF_SHAPE_INST)
                        .addProperty(RDF.type, productDefShape)
                        .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representations_IfcProductRepresentation"), shapeRepBlankNode));
        sampleModel.add(shapeRepBlankNode, hasContents,
                // Create object resource and its properties
                sampleModel.createResource(IFC_MAPPED_SHAPE_REP)
                        .addProperty(RDF.type, shapeRep)
                        .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representationType_IfcRepresentation"), // Blank Node
                                sampleModel.createResource().addProperty(hasString, IFC_MAPPED_REP_TYPE_VAL)) // Blank Node's properties
                        .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "items_IfcRepresentation"), sampleModel.createResource(IFC_MAPPED_ITEM_INST)));
        sampleModel.getResource(IFC_MAPPED_ITEM_INST)
                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcMappedItem"))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "mappingTarget_IfcMappedItem"), sampleModel.createResource(IFC_TARGET_TRANSFORMATION_OPERATOR_INST))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "mappingSource_IfcMappedItem"), sampleModel.createResource(IFC_REP_MAP_INST));
        sampleModel.getResource(IFC_TARGET_TRANSFORMATION_OPERATOR_INST).addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcCartesianTransformationOperator3D"));
        sampleModel.getResource(IFC_REP_MAP_INST).addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRepresentationMap"))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "mappingOrigin_IfcRepresentationMap"), sampleModel.createResource(IFC_SOURCE_PLACEMENT_INST))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "mappedRepresentation_IfcRepresentationMap"), sampleModel.createResource(IFC_SHAPE_REP_INST));
        sampleModel.getResource(IFC_SHAPE_REP_INST)
                .addProperty(RDF.type, shapeRep)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representationType_IfcRepresentation"), // Blank Node
                        sampleModel.createResource().addProperty(hasString, IFC_MAPPED_REP_TYPE_VAL)) // Blank Node's properties
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "contextOfItems_IfcRepresentation"),
                        sampleModel.createResource(IFC_GEOM_SUB_CONTEXT_INST).addProperty(RDF.type, geomRepSubContext))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "items_IfcRepresentation"),
                        sampleModel.createResource(IFC_FACETED_BREP_INST).addProperty(RDF.type, facetedBrep));
    }
}