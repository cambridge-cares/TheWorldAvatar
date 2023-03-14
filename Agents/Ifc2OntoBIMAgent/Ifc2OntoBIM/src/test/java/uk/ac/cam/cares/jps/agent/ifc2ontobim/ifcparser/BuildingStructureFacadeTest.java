package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Wall;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcStoreyRepresentation;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

class BuildingStructureFacadeTest {
    private static Model sampleModel;
    private static final String TEST_BASE_URI = "http://www.theworldavatar.com/test/";
    // Element Instances
    private static final String CEILING_CLASS = "IfcCovering";
    private static final String CEILING_INST = TEST_BASE_URI + CEILING_CLASS + "_106932";
    private static final String CEILING_NAME = "Stone ceiling";
    private static final String COLUMN_CLASS = "IfcColumn";
    private static final String COLUMN_INST = TEST_BASE_URI + COLUMN_CLASS + "_18571";
    private static final String COLUMN_NAME = "Stone column";
    private static final String DOOR_CLASS = "IfcDoor";
    private static final String DOOR_INST = TEST_BASE_URI + DOOR_CLASS + "_5232";
    private static final String DOOR_NAME = "Iron door";
    private static final String FLOOR_CLASS = "IfcSlab";
    private static final String FLOOR_INST = TEST_BASE_URI + FLOOR_CLASS + "_32194";
    private static final String FLOOR_NAME = "Glass floor";
    private static final String ROOF_CLASS = "IfcSlab";
    private static final String ROOF_INST = TEST_BASE_URI + ROOF_CLASS + "_5132";
    private static final String ROOF_NAME = "Stone roof";
    private static final String STAIR_CLASS = "IfcStair";
    private static final String STAIR_INST = TEST_BASE_URI + STAIR_CLASS + "_15189";
    private static final String STAIR_NAME = "Assembled stairs";
    private static final String STAIR_LANDING_CLASS = "IfcSlab";
    private static final String STAIR_LANDING_INST = TEST_BASE_URI + STAIR_LANDING_CLASS + "_10938";
    private static final String STAIR_LANDING_NAME = "Wood landing";
    private static final String STAIR_LANDING_ID = "e19jfa9";
    private static final String STAIR_LANDING_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_591092";
    private static final String STAIR_RAILING_CLASS = "IfcRailing";
    private static final String STAIR_RAILING_INST = TEST_BASE_URI + STAIR_RAILING_CLASS + "_3378";
    private static final String STAIR_RAILING_NAME = "Metal railings";
    private static final String STAIR_RAILING_ID = "masu81z7";
    private static final String STAIR_RAILING_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_310409";
    private static final String STAIR_FLIGHT_CLASS = "IfcStairFlight";
    private static final String STAIR_FLIGHT_INST = TEST_BASE_URI + STAIR_FLIGHT_CLASS + "_41750";
    private static final String STAIR_FLIGHT_NAME = "Concrete stairs";
    private static final String STAIR_FLIGHT_ID = "e19jpasf91urjkfa9";
    private static final String STAIR_FLIGHT_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_571572";
    private static final String STAIR_STRUCT_COMP_CLASS = "IfcMember";
    private static final String STAIR_STRUCT_COMP_INST = TEST_BASE_URI + STAIR_STRUCT_COMP_CLASS + "_15189";
    private static final String STAIR_STRUCT_COMP_NAME = "Stringer";
    private static final String STAIR_STRUCT_COMP_ID = "vaski31ra";
    private static final String STAIR_STRUCT_COMP_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_8884918";
    private static final String WALL_CLASS = "IfcWall";
    private static final String WALL_INST = TEST_BASE_URI + WALL_CLASS + "_6165";
    private static final String WALL_NAME = "Brickwall";
    private static final String WINDOW_CLASS = "IfcWindow";
    private static final String WINDOW_INST = TEST_BASE_URI + WINDOW_CLASS + "_81571";
    private static final String WINDOW_NAME = "Steel windows";
    // Generic element instances
    private static final String STOREY_INST = TEST_BASE_URI + "IfcBuildingStorey_3294";
    private static final String REL_TYPE_DEF_INST = TEST_BASE_URI + "IfcRelDefinesByType_51062";
    private static final String COVERING_TYPE_INST = TEST_BASE_URI + "IfcCoveringType_37775";
    private static final String ELEMENT_ID = "01294juas";
    // Generic element geometry instances
    private static final String ELEMENT_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_1041";
    private static final String IFC_SHAPE_REP_INST = TEST_BASE_URI + "IfcShapeRepresentation_5108";
    private static final String IFC_SHAPE_REP_LIST_INST = TEST_BASE_URI + "IfcShapeRepresentation_List_5108";
    private static final String IFC_MAPPED_SHAPE_REP = TEST_BASE_URI + "IfcShapeRepresentation_8672";
    private static final String IFC_GEOM_SUB_CONTEXT_INST = TEST_BASE_URI + "IfcGeometricRepresentationSubContext_5108";
    private static final String FACETED_BREP_CLASS = "FacetedBrep";
    private static final String EXTRUDED_AREA_SOLID_CLASS = "ExtrudedAreaSolid";
    private static final String IFC_FACETED_BREP_INST = TEST_BASE_URI + FACETED_BREP_CLASS + "_726358";
    private static final String IFC_FACETED_BREP_SEC_INST = TEST_BASE_URI + FACETED_BREP_CLASS + "_18517";
    private static final String IFC_REP_TYPE_VAL = "Faceted Brep";
    private static final String IFC_MAPPED_REP_TYPE_VAL = "MappedRepresentation";
    private static final String IFC_SOURCE_PLACEMENT_INST = TEST_BASE_URI + "IfcLocalPlacement_571261";
    private static final String IFC_TARGET_TRANSFORMATION_OPERATOR_INST = TEST_BASE_URI + "IfcCartesianTransformationOperator3D_3098157";
    private static final String IFC_MAPPED_ITEM_INST = TEST_BASE_URI + "IfcMappedItem_185218";
    private static final String IFC_REP_MAP_INST = TEST_BASE_URI + "IfcRepresentationMap_3213";
    // Stair geometry properties
    private static final String STAIR_FLIGHT_SHAPE_REP_INST = TEST_BASE_URI + "IfcShapeRepresentation_6817";
    private static final String STAIR_FLIGHT_GEOM_INST = TEST_BASE_URI + EXTRUDED_AREA_SOLID_CLASS + "_296358";
    private static final String STAIR_FLIGHT_REP_TYPE_VAL = "Swept Solid";
    private static final Integer STAIR_FLIGHT_RISER_NUM_VAL = 13;
    private static final Integer STAIR_FLIGHT_TREAD_NUM_VAL = 6;
    private static final Double STAIR_FLIGHT_RISER_HEIGHT_VAL = 1.10;
    private static final Double STAIR_FLIGHT_TREAD_LENGTH_VAL = 0.15;
    private static final String STAIR_LANDING_SHAPE_REP_INST = TEST_BASE_URI + "IfcShapeRepresentation_6912";
    private static final String STAIR_LANDING_GEOM_INST = TEST_BASE_URI + EXTRUDED_AREA_SOLID_CLASS + "_7810";
    private static final String STAIR_LANDING_REP_TYPE_VAL = "Swept Solid";
    private static final String STAIR_RAILING_SHAPE_REP_INST = TEST_BASE_URI + "IfcShapeRepresentation_6955";
    private static final String STAIR_RAILING_GEOM_INST = TEST_BASE_URI + FACETED_BREP_CLASS + "_187513";
    private static final String STAIR_RAILING_REP_TYPE_VAL = "Faceted Brep";
    private static final String STAIR_STRUCT_COMP_SHAPE_REP_INST = TEST_BASE_URI + "IfcShapeRepresentation_7010";
    private static final String STAIR_STRUCT_COMP_GEOM_INST = TEST_BASE_URI + FACETED_BREP_CLASS + "_71728";
    private static final String STAIR_STRUCT_COMP_REP_TYPE_VAL = "Faceted Brep";
    // Second geometry type for walls
    private static final String IFC_SEC_SHAPE_REP_INST = TEST_BASE_URI + "IfcShapeRepresentation_91871";
    private static final String POLYLINE_CLASS = "Polyline";
    private static final String IFC_POLYLINE_INST = TEST_BASE_URI + POLYLINE_CLASS + "_726358";
    private static final String IFC_SEC_REP_TYPE_VAL = "Curve2D";
    // Void representation
    private static final String IFC_GEOMETRIC_VOID_INST = TEST_BASE_URI + "GeometricVoid_31296";
    private static final String IFC_VOID_TYPE = "Opening";
    private static final String IFC_VOID_SHAPE_REP_TYPE = "Swept Solid";
    private static final String IFC_VOID_PLACEMENT_INST = TEST_BASE_URI + "LocalPlacement_312965";
    private static final String IFC_VOID_SHAPE_REP_INST = TEST_BASE_URI + "ModelRepresentation3D_1753";
    private static final String IFC_VOID_SUB_CONTEXT_INST = TEST_BASE_URI + "IfcGeometricRepresentationSubContext_451852";
    private static final String IFC_VOID_GEOM_INST = TEST_BASE_URI + "ExtrudedAreaSolid_8152";
    // Properties
    private static final Property hasString = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasString");
    private static final Property hasDouble = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasDouble");
    private static final Property hasInteger = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasInteger");
    private static final Property hasContents = ResourceFactory.createProperty(JunitTestUtils.listUri + "hasContents");
    private static final Property hasNext = ResourceFactory.createProperty(JunitTestUtils.listUri + "hasNext");
    private static final Property hasName = ResourceFactory.createProperty(JunitTestUtils.IFC2X3_NAME_PROPERTY);
    private static final Property hasId = ResourceFactory.createProperty(JunitTestUtils.IFC2X3_ID_PROPERTY);
    private static final Property objectPlacement = ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "objectPlacement_IfcProduct");
    // Repeated classes
    private static final Resource localPlacement = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcLocalPlacement");
    private static final Resource productDefShape = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcProductDefinitionShape");
    private static final Resource shapeRep = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcShapeRepresentation");
    private static final Resource geomRepSubContext = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcGeometricRepresentationSubContext");
    private static final Resource facetedBrep = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcFacetedBrep");
    private static final Resource polyline = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcPolyline");
    private static final Resource extrudedAreaSolid = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcExtrudedAreaSolid");
    private static final Resource ifcTypeRel = ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelDefinesByType");

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
    }

    @AfterAll
    static void resetZoneMappingsForOtherTests() {
        SpatialZoneStorage.resetSingleton();
    }

    @Test
    void testAddCeilingStatementsNonMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(CEILING_INST, CEILING_CLASS, CEILING_NAME);
        addHostZoneTriples(CEILING_INST, STOREY_INST);
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(CEILING_INST));
        sampleModel.createResource(REL_TYPE_DEF_INST).addProperty(RDF.type, ifcTypeRel)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDefines"), sampleModel.getResource(CEILING_INST))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatingType_IfcRelDefinesByType"),
                        sampleModel.createResource(COVERING_TYPE_INST)
                                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcCoveringType"))
                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "predefinedType_IfcCoveringType"), sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "CEILING"))
                );
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addCeilingStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Ceiling", CEILING_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(IFC_REP_TYPE_VAL), result);
        // The following statements are optional and should not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddCeilingStatementsMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(CEILING_INST, CEILING_CLASS, CEILING_NAME);
        addHostZoneTriples(CEILING_INST, STOREY_INST);
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addMappedGeometryTriples(sampleModel.getResource(CEILING_INST));
        sampleModel.createResource(REL_TYPE_DEF_INST).addProperty(RDF.type, ifcTypeRel)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDefines"), sampleModel.getResource(CEILING_INST))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatingType_IfcRelDefinesByType"),
                        sampleModel.createResource(COVERING_TYPE_INST)
                                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcCoveringType"))
                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "predefinedType_IfcCoveringType"), sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "CEILING"))
                );
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addCeilingStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Ceiling", CEILING_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddColumnStatementsNonMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(COLUMN_INST, COLUMN_CLASS, COLUMN_NAME);
        addHostZoneTriples(COLUMN_INST, STOREY_INST);
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(COLUMN_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addColumnStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Column", COLUMN_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(IFC_REP_TYPE_VAL), result);
        // The following statements are optional and should not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddColumnStatementsMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(COLUMN_INST, COLUMN_CLASS, COLUMN_NAME);
        addHostZoneTriples(COLUMN_INST, STOREY_INST);
        // Generate the alternate mapped geometry representation triples
        addMappedGeometryTriples(sampleModel.getResource(COLUMN_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addColumnStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Column", COLUMN_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddDoorStatementsNonMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(DOOR_INST, DOOR_CLASS, DOOR_NAME);
        addHostZoneTriples(DOOR_INST, STOREY_INST);
        addWallAssemblyTriples(DOOR_INST);
        // Get the singleton to add the assembly inst
        ElementStorage elementMappings = ElementStorage.Singleton();
        Wall wall = new Wall(WALL_INST, WALL_NAME, null, null, null, null, null);
        elementMappings.add(WALL_INST, wall);
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(DOOR_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addDoorStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Door", DOOR_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAssemblyStatements("Door"), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(IFC_REP_TYPE_VAL), result);
        // The following statements are optional and should not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddDoorStatementsMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(DOOR_INST, DOOR_CLASS, DOOR_NAME);
        addHostZoneTriples(DOOR_INST, STOREY_INST);
        addWallAssemblyTriples(DOOR_INST);
        // Get the singleton to add the assembly inst
        ElementStorage elementMappings = ElementStorage.Singleton();
        Wall wall = new Wall(WALL_INST, WALL_NAME, null, null, null, null, null);
        elementMappings.add(WALL_INST, wall);
        // Generate the alternate mapped geometry representation triples
        addMappedGeometryTriples(sampleModel.getResource(DOOR_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addDoorStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Door", DOOR_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAssemblyStatements("Door"), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddFloorStatementsNonMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(FLOOR_INST, FLOOR_CLASS, FLOOR_NAME);
        addHostZoneTriples(FLOOR_INST, STOREY_INST);
        addSlabTriples(FLOOR_INST, "FLOOR");
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(FLOOR_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addFloorStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Floor", FLOOR_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(IFC_REP_TYPE_VAL), result);
        // The following statements are optional and should not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddFloorStatementsAlternateEnumName() {
        // Set up
        addBaseTriples(FLOOR_INST, FLOOR_CLASS, FLOOR_NAME);
        addHostZoneTriples(FLOOR_INST, STOREY_INST);
        addSlabTriples(FLOOR_INST, "BASESLAB");
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(FLOOR_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addFloorStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Floor", FLOOR_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(IFC_REP_TYPE_VAL), result);
        // The following statements are optional and should not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddFloorStatementsWithVoid() {
        // Set up
        addBaseTriples(FLOOR_INST, FLOOR_CLASS, FLOOR_NAME);
        addHostZoneTriples(FLOOR_INST, STOREY_INST);
        addSlabTriples(FLOOR_INST, "BASESLAB");
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(FLOOR_INST));
        addVoidRepresentationTriples(sampleModel.getResource(FLOOR_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addFloorStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Floor", FLOOR_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(IFC_REP_TYPE_VAL), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeometricVoidStatements(), result);
        // The following statements are optional and should not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddFloorStatementsMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(FLOOR_INST, FLOOR_CLASS, FLOOR_NAME);
        addHostZoneTriples(FLOOR_INST, STOREY_INST);
        addSlabTriples(FLOOR_INST, "FLOOR");
        // Generate the alternate mapped geometry representation triples
        addMappedGeometryTriples(sampleModel.getResource(FLOOR_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addFloorStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Floor", FLOOR_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddRoofStatementsNonMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(ROOF_INST, ROOF_CLASS, ROOF_NAME);
        addHostZoneTriples(ROOF_INST, STOREY_INST, true);
        addSlabTriples(ROOF_INST, "ROOF");
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(ROOF_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addRoofStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Roof", ROOF_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(IFC_REP_TYPE_VAL), result);
        // The following statements are optional and should not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddRoofStatementsWithVoid() {
        // Set up
        addBaseTriples(ROOF_INST, ROOF_CLASS, ROOF_NAME);
        addHostZoneTriples(ROOF_INST, STOREY_INST, true);
        addSlabTriples(ROOF_INST, "ROOF");
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(ROOF_INST));
        addVoidRepresentationTriples(sampleModel.getResource(ROOF_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addRoofStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Roof", ROOF_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(IFC_REP_TYPE_VAL), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeometricVoidStatements(), result);
        // The following statements are optional and should not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddRoofStatementsMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(ROOF_INST, ROOF_CLASS, ROOF_NAME);
        addHostZoneTriples(ROOF_INST, STOREY_INST, true);
        addSlabTriples(ROOF_INST, "ROOF");
        // Generate the alternate mapped geometry representation triples
        addMappedGeometryTriples(sampleModel.getResource(ROOF_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addRoofStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Roof", ROOF_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddStairStatements() {
        // Set up
        addBaseTriples(STAIR_INST, STAIR_CLASS, STAIR_NAME);
        addHostZoneTriples(STAIR_INST, STOREY_INST);
        addStairAssemblyTriples(STAIR_INST);
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addStairStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Stair", STAIR_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(IFC_REP_TYPE_VAL), result);
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("StairFlight", STAIR_FLIGHT_NAME, STAIR_FLIGHT_ID, STAIR_FLIGHT_POSITION_INST), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(STAIR_FLIGHT_GEOM_INST, EXTRUDED_AREA_SOLID_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(STAIR_FLIGHT_REP_TYPE_VAL), result);
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Landing", STAIR_LANDING_NAME, STAIR_LANDING_ID, STAIR_LANDING_POSITION_INST), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(STAIR_LANDING_GEOM_INST, EXTRUDED_AREA_SOLID_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(STAIR_LANDING_REP_TYPE_VAL), result);
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Railing", STAIR_RAILING_NAME, STAIR_RAILING_ID, STAIR_RAILING_POSITION_INST), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(STAIR_RAILING_GEOM_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(STAIR_RAILING_REP_TYPE_VAL), result);
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("StairStructuralComponent", STAIR_STRUCT_COMP_NAME, STAIR_STRUCT_COMP_ID, STAIR_STRUCT_COMP_POSITION_INST), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(STAIR_STRUCT_COMP_GEOM_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(STAIR_STRUCT_COMP_REP_TYPE_VAL), result);
        JunitTestUtils.doesExpectedListExist(genExpectedStoreyStatements(), result);

        // Maybe the above will fail for geom rep side
        // Still to test
        // Generate expected for each stair subtype, their model and consists of
    }

    @Test
    void testAddWallStatementsNonMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(WALL_INST, WALL_CLASS, WALL_NAME);
        addHostZoneTriples(WALL_INST, STOREY_INST);
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(WALL_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addWallStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Wall", WALL_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(IFC_REP_TYPE_VAL), result);
        // The following statements are optional and should not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddWallStatementsMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(WALL_INST, WALL_CLASS, WALL_NAME);
        addHostZoneTriples(WALL_INST, STOREY_INST);
        // Generate the alternate mapped geometry representation triples
        addMappedGeometryTriples(sampleModel.getResource(WALL_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addWallStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Wall", WALL_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddWallStatementsWithTwoGeometries() {
        // Set up
        addBaseTriples(WALL_INST, WALL_CLASS, WALL_NAME);
        addHostZoneTriples(WALL_INST, STOREY_INST);
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(WALL_INST));
        addMultipleGeometryTypeTriples();
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addWallStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Wall", WALL_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedMultipleGeometricRepresentationStatements(), result);
    }

    @Test
    void testAddWindowStatementsNonMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(WINDOW_INST, WINDOW_CLASS, WINDOW_NAME);
        addHostZoneTriples(WINDOW_INST, STOREY_INST);
        addWallAssemblyTriples(WINDOW_INST);
        // Get the singleton to add the assembly inst
        ElementStorage elementMappings = ElementStorage.Singleton();
        Wall wall = new Wall(WALL_INST, WALL_NAME, null, null, null, null, null);
        elementMappings.add(WALL_INST, wall);
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(WINDOW_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addWindowStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Window", WINDOW_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAssemblyStatements("Window"), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(IFC_REP_TYPE_VAL), result);
        // The following statements are optional and should not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddWindowStatementsMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(WINDOW_INST, WINDOW_CLASS, WINDOW_NAME);
        addHostZoneTriples(WINDOW_INST, STOREY_INST);
        addWallAssemblyTriples(WINDOW_INST);
        // Get the singleton to add the assembly inst
        ElementStorage elementMappings = ElementStorage.Singleton();
        Wall wall = new Wall(WALL_INST, WALL_NAME, null, null, null, null, null);
        elementMappings.add(WALL_INST, wall);
        // Generate the alternate mapped geometry representation triples
        addMappedGeometryTriples(sampleModel.getResource(WINDOW_INST));
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addWindowStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Window", WINDOW_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAssemblyStatements("Window"), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalGeomStatements(), result);
    }

    @Test
    void testAddStatementsMoreThanOneNonMappedGeometryRepresentation() {
        // Set up
        addBaseTriples(COLUMN_INST, COLUMN_CLASS, COLUMN_NAME);
        addHostZoneTriples(COLUMN_INST, STOREY_INST);
        // Generate the triples that is applicable for all generic geometry representation except mapped representation
        addGeometryTriples(sampleModel.getResource(COLUMN_INST));
        addSecondGeometryTriples();
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        BuildingStructureFacade sample = new BuildingStructureFacade();
        // Execute method
        sample.addColumnStatements(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedBaseStatements("Column", COLUMN_NAME), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_INST, FACETED_BREP_CLASS), result);
        JunitTestUtils.doesExpectedListExist(genExpectedGeomRepTypeStatements(IFC_REP_TYPE_VAL), result);
        JunitTestUtils.doesExpectedListExist(genExpectedModelRepGeometryItemStatements(IFC_FACETED_BREP_SEC_INST, FACETED_BREP_CLASS), result);
        // The following statements are optional and should not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalGeomStatements(), result);
    }

    private void addBaseTriples(String elementIRI, String elementClass, String name) {
        addBaseTriples(elementIRI, elementClass, name, ELEMENT_ID, ELEMENT_POSITION_INST);
        sampleModel.createResource(STOREY_INST)
                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcBuildingStorey"));
    }

    private void addBaseTriples(String elementIRI, String elementClass, String name, String uid, String placementInst) {
        sampleModel.createResource(elementIRI)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + elementClass))
                .addProperty(hasName, sampleModel.createResource()
                        .addProperty(hasString, name))
                .addProperty(hasId, sampleModel.createResource()
                        .addProperty(hasString, uid))
                .addProperty(objectPlacement, sampleModel.createResource(placementInst)
                        .addProperty(RDF.type, localPlacement));
    }

    private void addHostZoneTriples(String elementIRI, String zoneIRI) {
        addHostZoneTriples(elementIRI, zoneIRI, false);
    }

    private void addHostZoneTriples(String elementIRI, String zoneIRI, boolean isRoof) {
        if (isRoof){
            Resource ifcRoofNode = sampleModel.createResource().addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRoof"));
            sampleModel.createResource()
                    .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelAggregates"))
                    .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDecomposes"), sampleModel.getResource(elementIRI))
                    .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatingObject_IfcRelDecomposes"), ifcRoofNode);
            sampleModel.createResource()
                    .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelContainedInSpatialStructure"))
                    .addProperty(ResourceFactory.createProperty(JunitTestUtils.IFC2X3_HOST_ZONE_PROPERTY), sampleModel.getResource(zoneIRI))
                    .addProperty(ResourceFactory.createProperty(JunitTestUtils.IFC2X3_CONTAIN_ELEMENT_PROPERTY), ifcRoofNode);
        } else {
            sampleModel.createResource()
                    .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelContainedInSpatialStructure"))
                    .addProperty(ResourceFactory.createProperty(JunitTestUtils.IFC2X3_HOST_ZONE_PROPERTY), sampleModel.getResource(zoneIRI))
                    .addProperty(ResourceFactory.createProperty(JunitTestUtils.IFC2X3_CONTAIN_ELEMENT_PROPERTY), sampleModel.getResource(elementIRI));
        }
    }

    private void addSlabTriples(String elementIRI, String elementEnum) {
        sampleModel.createResource(REL_TYPE_DEF_INST).addProperty(RDF.type, ifcTypeRel)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDefines"), sampleModel.getResource(elementIRI))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatingType_IfcRelDefinesByType"),
                        sampleModel.createResource()
                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "predefinedType_IfcSlabType"),
                                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + elementEnum))
                );
    }


    private void addWallAssemblyTriples(String elementIRI) {
        Resource openingElementNode = sampleModel.createResource().addProperty(RDF.type, ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcOpeningElement"));
        sampleModel.createResource(TEST_BASE_URI + "IfcRelFillsElement_314")
                .addProperty(RDF.type, ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelFillsElement"))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatedBuildingElement_IfcRelFillsElement"), sampleModel.getResource(elementIRI))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatingOpeningElement_IfcRelFillsElement"), openingElementNode);
        sampleModel.createResource(TEST_BASE_URI + "IfcRelVoidsElement_6171")
                .addProperty(RDF.type, ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelVoidsElement"))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatedOpeningElement_IfcRelVoidsElement"), openingElementNode)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatingBuildingElement_IfcRelVoidsElement"), sampleModel.createResource(WALL_INST));
    }

    private void addStairAssemblyTriples(String elementIRI) {
        addBaseTriples(STAIR_FLIGHT_INST, STAIR_FLIGHT_CLASS, STAIR_FLIGHT_NAME, STAIR_FLIGHT_ID, STAIR_FLIGHT_POSITION_INST);
        addGeometryTriples(STAIR_FLIGHT_INST, STAIR_FLIGHT_SHAPE_REP_INST, IFC_GEOM_SUB_CONTEXT_INST, STAIR_FLIGHT_GEOM_INST, STAIR_FLIGHT_REP_TYPE_VAL);
        addBaseTriples(STAIR_LANDING_INST, STAIR_LANDING_CLASS, STAIR_LANDING_NAME, STAIR_LANDING_ID, STAIR_LANDING_POSITION_INST);
        addGeometryTriples(STAIR_LANDING_INST, STAIR_LANDING_SHAPE_REP_INST, IFC_GEOM_SUB_CONTEXT_INST, STAIR_LANDING_GEOM_INST, STAIR_LANDING_REP_TYPE_VAL);
        addBaseTriples(STAIR_RAILING_INST, STAIR_RAILING_CLASS, STAIR_RAILING_NAME, STAIR_RAILING_ID, STAIR_RAILING_POSITION_INST);
        addGeometryTriples(STAIR_RAILING_INST, STAIR_RAILING_SHAPE_REP_INST, IFC_GEOM_SUB_CONTEXT_INST, STAIR_RAILING_GEOM_INST, STAIR_RAILING_REP_TYPE_VAL);
        addBaseTriples(STAIR_STRUCT_COMP_INST, STAIR_STRUCT_COMP_CLASS, STAIR_STRUCT_COMP_NAME, STAIR_STRUCT_COMP_ID, STAIR_STRUCT_COMP_POSITION_INST);
        addGeometryTriples(STAIR_STRUCT_COMP_INST, STAIR_STRUCT_COMP_SHAPE_REP_INST, IFC_GEOM_SUB_CONTEXT_INST, STAIR_STRUCT_COMP_GEOM_INST, STAIR_STRUCT_COMP_REP_TYPE_VAL);
        sampleModel.createResource()
                .addProperty(RDF.type, ResourceFactory.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelAggregates"))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatingObject_IfcRelDecomposes"), sampleModel.getResource(elementIRI))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDecomposes"), sampleModel.getResource(STAIR_FLIGHT_INST))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDecomposes"), sampleModel.getResource(STAIR_LANDING_INST))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDecomposes"), sampleModel.getResource(STAIR_RAILING_INST))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDecomposes"), sampleModel.getResource(STAIR_STRUCT_COMP_INST));
        sampleModel.getResource(STAIR_FLIGHT_INST)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "numberOfRiser_IfcStairFlight"),
                        sampleModel.createResource().addProperty(hasInteger, sampleModel.createTypedLiteral(STAIR_FLIGHT_RISER_NUM_VAL)))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "numberOfTreads_IfcStairFlight"),
                        sampleModel.createResource().addProperty(hasInteger, sampleModel.createTypedLiteral(STAIR_FLIGHT_TREAD_NUM_VAL)))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "riserHeight_IfcStairFlight"),
                        sampleModel.createResource().addProperty(hasDouble, sampleModel.createTypedLiteral(STAIR_FLIGHT_RISER_HEIGHT_VAL)))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "treadLength_IfcStairFlight"),
                        sampleModel.createResource().addProperty(hasDouble, sampleModel.createTypedLiteral(STAIR_FLIGHT_TREAD_LENGTH_VAL)));
    }

    private void addGeometryTriples(Resource element) {
        Resource shapeRepBlankNode = sampleModel.createResource(IFC_SHAPE_REP_LIST_INST);
        sampleModel.add(element, ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representation_IfcProduct"),
                // Create object resource and its properties
                sampleModel.createResource()
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

    private void addGeometryTriples(String elementIri, String shapeRepInst, String subContextInst, String geomInst, String shapeRepType) {
        sampleModel.getResource(elementIri).addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representation_IfcProduct"),
                // Create object resource and its properties
                sampleModel.createResource().addProperty(RDF.type, productDefShape)
                        .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representations_IfcProductRepresentation"),
                                sampleModel.createResource().addProperty(hasContents,
                                        // Create object resource and its properties
                                        sampleModel.createResource(shapeRepInst)
                                                .addProperty(RDF.type, shapeRep)
                                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representationType_IfcRepresentation"), // Blank Node
                                                        sampleModel.createResource().addProperty(hasString, shapeRepType)) // Blank Node's properties
                                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "contextOfItems_IfcRepresentation"),
                                                        sampleModel.createResource(subContextInst).addProperty(RDF.type, geomRepSubContext))
                                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "items_IfcRepresentation"),
                                                        sampleModel.createResource(geomInst).addProperty(RDF.type, facetedBrep)))));

    }

    private void addSecondGeometryTriples() {
        sampleModel.getResource(IFC_SHAPE_REP_INST)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "items_IfcRepresentation"),
                        sampleModel.createResource(IFC_FACETED_BREP_SEC_INST).addProperty(RDF.type, facetedBrep));
    }

    private void addMultipleGeometryTypeTriples() {
        Resource shapeRepBlankNode = sampleModel.createResource();
        sampleModel.getResource(IFC_SHAPE_REP_LIST_INST)
                .addProperty(hasNext, shapeRepBlankNode);
        sampleModel.add(shapeRepBlankNode, hasContents,
                // Create object resource and its properties
                sampleModel.createResource(IFC_SEC_SHAPE_REP_INST)
                        .addProperty(RDF.type, shapeRep)
                        .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representationType_IfcRepresentation"), // Blank Node
                                sampleModel.createResource().addProperty(hasString, IFC_SEC_REP_TYPE_VAL)) // Blank Node's properties
                        .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "contextOfItems_IfcRepresentation"),
                                sampleModel.createResource(IFC_GEOM_SUB_CONTEXT_INST).addProperty(RDF.type, geomRepSubContext))
                        .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "items_IfcRepresentation"),
                                sampleModel.createResource(IFC_POLYLINE_INST).addProperty(RDF.type, polyline)));
    }

    private void addMappedGeometryTriples(Resource element) {
        Resource shapeRepBlankNode = sampleModel.createResource();
        sampleModel.add(element, ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representation_IfcProduct"),
                // Create object resource and its properties
                sampleModel.createResource()
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

    private void addVoidRepresentationTriples(Resource element) {
        sampleModel.createResource()
                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelVoidsElement"))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatingBuildingElement_IfcRelVoidsElement"), element)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "relatedOpeningElement_IfcRelVoidsElement"),
                        sampleModel.createResource(IFC_GEOMETRIC_VOID_INST)
                                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcOpeningElement"))
                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "objectType_IfcObject"),
                                        sampleModel.createResource().addProperty(hasString, IFC_VOID_TYPE))
                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "objectPlacement_IfcProduct"),
                                        sampleModel.createResource(IFC_VOID_PLACEMENT_INST)
                                                .addProperty(RDF.type, localPlacement)
                                )
                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representation_IfcProduct"),
                                        sampleModel.createResource()
                                                .addProperty(RDF.type, productDefShape)
                                                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representations_IfcProductRepresentation"),
                                                        sampleModel.createResource()
                                                                .addProperty(hasContents, sampleModel.createResource(IFC_VOID_SHAPE_REP_INST)))
                                )
                );
        sampleModel.getResource(IFC_VOID_SHAPE_REP_INST)
                .addProperty(RDF.type, shapeRep)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representationType_IfcRepresentation"),
                        sampleModel.createResource().addProperty(hasString, IFC_VOID_SHAPE_REP_TYPE))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "contextOfItems_IfcRepresentation"),
                        sampleModel.createResource(IFC_VOID_SUB_CONTEXT_INST).addProperty(RDF.type, geomRepSubContext))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "items_IfcRepresentation"),
                        sampleModel.createResource(IFC_VOID_GEOM_INST).addProperty(RDF.type, extrudedAreaSolid));
    }

    private List<String> genExpectedBaseStatements(String bimElementClass, String name) {
        List<String> expected  = genExpectedBaseStatements(bimElementClass, name, ELEMENT_ID, ELEMENT_POSITION_INST);
        expected.add(TEST_BASE_URI + "Storey_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://w3id.org/bot#containsElement, " + TEST_BASE_URI + bimElementClass + "_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }

    private List<String> genExpectedBaseStatements(String bimElementClass, String name, String id, String placementInst) {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + bimElementClass + "_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobuildingstructure/" + bimElementClass);
        expected.add(TEST_BASE_URI + bimElementClass + "_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/IfcModelRepresentation");
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + name);
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + id);
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + placementInst);
        expected.add(placementInst + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        // Geometry representation triples
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasGeometricRepresentation, " + TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/ModelRepresentation3D");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSubContext, " + IFC_GEOM_SUB_CONTEXT_INST);
        expected.add(IFC_GEOM_SUB_CONTEXT_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/GeometricRepresentationSubContext");
        return expected;
    }

    private List<String> genExpectedModelRepGeometryItemStatements(String geomInst, String geomClass) {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + geomInst);
        expected.add(geomInst + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/" + geomClass);
        return expected;
    }

    private List<String> genExpectedAssemblyStatements(String elementClass) {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "Wall_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/consistsOf, " + TEST_BASE_URI + elementClass + "_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }

    private List<String> genExpectedGeomRepTypeStatements(String repTypeValue) {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationType, \"" + repTypeValue);
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

    private List<String> genExpectedMultipleGeometricRepresentationStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSubContext, " + IFC_GEOM_SUB_CONTEXT_INST);
        expected.add(IFC_GEOM_SUB_CONTEXT_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/GeometricRepresentationSubContext");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + IFC_POLYLINE_INST);
        expected.add(IFC_POLYLINE_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/" + POLYLINE_CLASS);
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationType, \"" + IFC_REP_TYPE_VAL);
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationType, \"" + IFC_SEC_REP_TYPE_VAL);
        return expected;
    }

    private List<String> genExpectedGeometricVoidStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasVoid, " + TEST_BASE_URI + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/GeometricVoid");
        expected.add(TEST_BASE_URI + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasGeometricRepresentation, " + TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasVoidType, \"" + IFC_VOID_TYPE);
        expected.add(TEST_BASE_URI + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + IFC_VOID_PLACEMENT_INST);
        expected.add(IFC_VOID_PLACEMENT_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/ModelRepresentation3D");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSubContext, " + IFC_VOID_SUB_CONTEXT_INST);
        expected.add(IFC_VOID_SUB_CONTEXT_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/GeometricRepresentationSubContext");
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationType, \"" + IFC_VOID_SHAPE_REP_TYPE);
        expected.add(TEST_BASE_URI + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + IFC_VOID_GEOM_INST);
        expected.add(IFC_VOID_GEOM_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/ExtrudedAreaSolid");
        return expected;
    }

    private List<String> genExpectedStoreyStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "Stair_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/consistsOf, " + TEST_BASE_URI + "Landing_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Stair_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/consistsOf, " + TEST_BASE_URI + "Railing_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Stair_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/consistsOf, " + TEST_BASE_URI + "StairFlight_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Stair_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/consistsOf, " + TEST_BASE_URI + "StairStructuralComponent_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "StairFlight_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/hasNumOfRiser, \"" + STAIR_FLIGHT_RISER_NUM_VAL);
        expected.add(TEST_BASE_URI + "StairFlight_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/hasNumOfTread, \"" + STAIR_FLIGHT_TREAD_NUM_VAL);
        expected.add(TEST_BASE_URI + "StairFlight_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/hasRiserHeight, \"" + STAIR_FLIGHT_RISER_HEIGHT_VAL);
        expected.add(TEST_BASE_URI + "StairFlight_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobuildingstructure/hasTreadLength, \"" + STAIR_FLIGHT_TREAD_LENGTH_VAL);
        return expected;
    }
}