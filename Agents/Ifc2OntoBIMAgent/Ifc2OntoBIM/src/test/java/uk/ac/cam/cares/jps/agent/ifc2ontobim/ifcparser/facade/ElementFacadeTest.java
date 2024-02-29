package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.facade;

import org.apache.jena.rdf.model.*;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcRoomRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.ElementStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.SpatialZoneStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

class ElementFacadeTest {
    private static Model sampleModel;
    private static final String TEST_BASE_URI = "https://www.theworldavatar.com/test/";
    private static final String TEST_ZONE_PLACEMENT_IRI = TEST_BASE_URI + JunitTestUtils.BIM_PLACEMENT_CLASS + "_385";
    private static final String ROOM_INST = TEST_BASE_URI + "Room_281";
    private static final String BUILDING_ELEMENT_PROXY_CLASS = "IfcBuildingElementProxy";
    private static final String FLOW_SEGMENT_CLASS = "IfcFlowSegment";
    private static final String FLOW_TERMINAL_CLASS = "IfcFlowTerminal";
    private static final String FURNISHING_ELEMENT_CLASS = "IfcFurnishingElement";
    private static final String SUB_CONTEXT_INST = TEST_BASE_URI + "GeometricRepresentationSubContext_18";
    private static final String REP_TYPE = "Faceted brep";
    // Properties
    private static final Property hasString = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasString");
    private static final Property hasContents = ResourceFactory.createProperty(JunitTestUtils.listUri + "hasContents");
    private static final Property hasName = ResourceFactory.createProperty(JunitTestUtils.IFC2X3_NAME_PROPERTY);
    private static final Property hasId = ResourceFactory.createProperty(JunitTestUtils.IFC2X3_ID_PROPERTY);
    private static final Property objectPlacement = ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "objectPlacement_IfcProduct");
    // Element 1 fields
    private static final String TABLE_INST = TEST_BASE_URI + "Element_325";
    private static final String TABLE_NAME = "Wooden table";
    private static final String TABLE_UID = "e01835hg";
    private static final String TABLE_PLACEMENT_INST = TEST_BASE_URI + "LocalPlacement_30158";
    private static final String TABLE_SHAPE_REP_INST = TEST_BASE_URI + "ModelRepresentation_5312";
    private static final String TABLE_GEOM_INST = TEST_BASE_URI + "IfcFacetedBrep_296358";
    private static final String TABLE_GEOM_BIM_INST = TEST_BASE_URI + "FacetedBrep_296358";
    // Element 2 fields
    private static final String ELEC_METER_INST = TEST_BASE_URI + "Element_3525";
    private static final String ELEC_METER_NAME = "Electrical meter";
    private static final String ELEC_METER_UID = "vas8134ras";
    private static final String ELEC_METER_PLACEMENT_INST = TEST_BASE_URI + "LocalPlacement_3158";
    private static final String ELEC_METER_SHAPE_REP_INST = TEST_BASE_URI + "ModelRepresentation_25312";
    private static final String ELEC_METER_GEOM_INST = TEST_BASE_URI + "IfcFacetedBrep_2958";
    private static final String ELEC_METER_GEOM_BIM_INST = TEST_BASE_URI + "FacetedBrep_2958";

    // Element 3 fields
    private static final String CHEMICAL_CONTAINER_INST = TEST_BASE_URI + "Element_625";
    private static final String CHEMICAL_CONTAINER_NAME = "Explosive precursor bottle";
    private static final String CHEMICAL_CONTAINER_UID = "a618a37hb";
    private static final String CHEMICAL_CONTAINER_PLACEMENT_INST = TEST_BASE_URI + "LocalPlacement_6758";
    private static final String CHEMICAL_CONTAINER_SHAPE_REP_INST = TEST_BASE_URI + "ModelRepresentation_3312";
    private static final String CHEMICAL_CONTAINER_GEOM_INST = TEST_BASE_URI + "IfcFacetedBrep_29558";
    private static final String CHEMICAL_CONTAINER_GEOM_BIM_INST = TEST_BASE_URI + "FacetedBrep_29558";

    // Element 4 Pipe fields
    private static final String PIPE_INST = TEST_BASE_URI + "Element_325";
    private static final String PIPE_NAME = "Sewage pipe";
    private static final String PIPE_UID = "921safl921";
    private static final String PIPE_PLACEMENT_INST = TEST_BASE_URI + "LocalPlacement_252";
    private static final String PIPE_SHAPE_REP_INST = TEST_BASE_URI + "ModelRepresentation_8881";
    private static final String PIPE_GEOM_INST = TEST_BASE_URI + "IfcFacetedBrep_7218";
    private static final String PIPE_GEOM_BIM_INST = TEST_BASE_URI + "FacetedBrep_7218";

    // Geometry classes
    private static final String FACETED_BREP_CLASS = JunitTestUtils.bimUri + "FacetedBrep";
    // Element Classes
    private static final String ELEMENT_CLASS = JunitTestUtils.botUri + "Element";
    private static final String ELEC_METER_CLASS = "https://www.theworldavatar.com/kg/ontodevice/ElectricityMeter";
    private static final String CHEMICAL_CONTAINER_CLASS = "https://www.theworldavatar.com/kg/ontolab/ChemicalContainer";

    @BeforeAll
    static void addTestZoneMappings() {
        // Set base name space
        NamespaceMapper.setBaseNameSpace(TEST_BASE_URI);
        // Create a new storey instance, which does not require any values except for the IRI
        // This IRI is necessary to generate the Storey IRI within the element class
        IfcRoomRepresentation room = new IfcRoomRepresentation(null, null, TEST_ZONE_PLACEMENT_IRI, null);
        // Add the storey to the singleton
        SpatialZoneStorage zoneMappings = SpatialZoneStorage.Singleton();
        zoneMappings.add(ROOM_INST, room.getBimRoomIRI());
    }

    @BeforeEach
    void genSampleStatements() {
        sampleModel = ModelFactory.createDefaultModel();
    }

    @AfterAll
    static void resetMappingsForOtherTests() {
        SpatialZoneStorage.resetSingleton();
        ElementStorage.resetSingleton();
        // Reset base name space
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testAddElementStatementsForGenericElement() {
        // Set up
        addBaseTriples(TABLE_INST, BUILDING_ELEMENT_PROXY_CLASS, TABLE_NAME, TABLE_UID, TABLE_PLACEMENT_INST);
        addHostZoneTriples(TABLE_INST);
        addGeometryTriples(TABLE_INST, TABLE_SHAPE_REP_INST, SUB_CONTEXT_INST, TABLE_GEOM_INST, REP_TYPE);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        ElementFacade.addElementStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Element", ELEMENT_CLASS, TABLE_NAME, TABLE_UID, TABLE_PLACEMENT_INST), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(TABLE_GEOM_BIM_INST, REP_TYPE), result);
    }

    @Test
    void testAddElementStatementsForElectricityMeter() {
        // Set up
        addBaseTriples(ELEC_METER_INST, FLOW_TERMINAL_CLASS, ELEC_METER_NAME, ELEC_METER_UID, ELEC_METER_PLACEMENT_INST);
        addHostZoneTriples(ELEC_METER_INST);
        addGeometryTriples(ELEC_METER_INST, ELEC_METER_SHAPE_REP_INST, SUB_CONTEXT_INST, ELEC_METER_GEOM_INST, REP_TYPE);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        ElementFacade.addElementStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("ElectricityMeter", ELEC_METER_CLASS, ELEC_METER_NAME, ELEC_METER_UID, ELEC_METER_PLACEMENT_INST), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(ELEC_METER_GEOM_BIM_INST, REP_TYPE), result);
    }

    @Test
    void testAddElementStatementsForPipes() {
        // Set up
        addBaseTriples(PIPE_INST, FLOW_SEGMENT_CLASS, PIPE_NAME, PIPE_UID, PIPE_PLACEMENT_INST);
        addHostZoneTriples(PIPE_INST);
        addGeometryTriples(PIPE_INST, PIPE_SHAPE_REP_INST, SUB_CONTEXT_INST, PIPE_GEOM_INST, REP_TYPE);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        ElementFacade.addElementStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Element", ELEMENT_CLASS, PIPE_NAME, PIPE_UID, PIPE_PLACEMENT_INST), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(PIPE_GEOM_BIM_INST, REP_TYPE), result);
    }

    @Test
    void testAddElementStatementsForChemicalContainer() {
        // Set up
        addBaseTriples(CHEMICAL_CONTAINER_INST, FURNISHING_ELEMENT_CLASS, CHEMICAL_CONTAINER_NAME, CHEMICAL_CONTAINER_UID, CHEMICAL_CONTAINER_PLACEMENT_INST);
        addHostZoneTriples(CHEMICAL_CONTAINER_INST);
        addGeometryTriples(CHEMICAL_CONTAINER_INST, CHEMICAL_CONTAINER_SHAPE_REP_INST, SUB_CONTEXT_INST, CHEMICAL_CONTAINER_GEOM_INST, REP_TYPE);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        ElementFacade.addElementStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("ChemicalContainer", CHEMICAL_CONTAINER_CLASS, CHEMICAL_CONTAINER_NAME, CHEMICAL_CONTAINER_UID, CHEMICAL_CONTAINER_PLACEMENT_INST), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(CHEMICAL_CONTAINER_GEOM_BIM_INST, REP_TYPE), result);
    }

    private void addBaseTriples(String elementIRI, String elementClass, String name, String uid, String placementInst) {
        sampleModel.createResource(elementIRI)
                .addProperty(JunitTestUtils.RDF_TYPE,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + elementClass))
                .addProperty(hasName, sampleModel.createResource()
                        .addProperty(hasString, name))
                .addProperty(hasId, sampleModel.createResource()
                        .addProperty(hasString, uid))
                .addProperty(objectPlacement, sampleModel.createResource(placementInst)
                        .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcLocalPlacement")));
    }

    private void addHostZoneTriples(String elementIRI) {
        sampleModel.createResource()
                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelContainedInSpatialStructure"))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.IFC2X3_HOST_ZONE_PROPERTY),
                        sampleModel.createResource(ROOM_INST)
                                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSpace")))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.IFC2X3_CONTAIN_ELEMENT_PROPERTY), sampleModel.getResource(elementIRI));
    }

    private void addGeometryTriples(String elementIri, String shapeRepInst, String subContextInst, String geomInst, String shapeRepType) {
        sampleModel.getResource(elementIri).addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representation_IfcProduct"),
                sampleModel.createResource()
                        .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcProductDefinitionShape"))
                        .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representations_IfcProductRepresentation"),
                                sampleModel.createResource().addProperty(hasContents,
                                        sampleModel.createResource(shapeRepInst)
                                                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcShapeRepresentation"))
                                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representationType_IfcRepresentation"),
                                                        sampleModel.createResource().addProperty(hasString, shapeRepType))
                                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "contextOfItems_IfcRepresentation"),
                                                        sampleModel.createResource(subContextInst)
                                                                .addProperty(JunitTestUtils.RDF_TYPE, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcGeometricRepresentationSubContext")))
                                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "items_IfcRepresentation"),
                                                        sampleModel.createResource(geomInst).addProperty(JunitTestUtils.RDF_TYPE,
                                                                sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcFacetedBrep"))))));

    }

    private List<String> genExpectedBaseStatements(String bimElementName, String elementClass, String name, String id, String placementInst) {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://w3id.org/bot#containsElement, " + TEST_BASE_URI + bimElementName + "_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + bimElementName + "_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDF_TYPE + ", " + elementClass);
        expected.add(TEST_BASE_URI + bimElementName + "_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDF_TYPE + ", https://www.theworldavatar.com/kg/ontobim/IfcModelRepresentation");
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDFS_LABEL + ", \"" + name);
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + id);
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + placementInst);
        // Geometry representation triples
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasGeometricRepresentation, " + TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, " + JunitTestUtils.RDF_TYPE + ", https://www.theworldavatar.com/kg/ontobim/ModelRepresentation3D");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasSubContext, " + SUB_CONTEXT_INST);
        return expected;
    }

    private List<String> genExpectedModelRepGeometryItemStatements(String geomInst, String repTypeValue) {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + geomInst);
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRepresentationType, \"" + repTypeValue);
        return expected;
    }
}