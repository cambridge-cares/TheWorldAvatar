package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.base.Sys;
import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

class SpatialZoneFacadeTest {
    private static Model sampleModel;
    private static final String TEST_BASE_URI = "http://www.theworldavatar.com/test/";
    // Project and context
    private static final String PROJECT_INST = TEST_BASE_URI + "IfcProject_1";
    private static final String PROJECT_NAME = "New Project";
    private static final String PROJECT_PHASE = "Construction";
    private static final String REP_CONTEXT_INST = TEST_BASE_URI + "IfcGeometricRepresentationContext_352";
    private static final String WCS_INST = TEST_BASE_URI + "LocalPlacement_2531";
    private static final String TRUE_NORTH_INST = TEST_BASE_URI + "DirectionVector_5513";
    // Spatial zones
    private static final String SITE_INST = TEST_BASE_URI + "IfcSite_16";
    private static final String SITE_ID = "1204avak981";
    private static final String SITE_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_1041";
    private static final String SITE_NAME = "Land boundary";
    private static final String BUILDING_INST = TEST_BASE_URI + "IfcBuilding_32";
    private static final String BUILDING_ID = "maso9127";
    private static final String BUILDING_NAME = "Host building";
    private static final String BUILDING_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_1052";
    private static final String STOREY_INST = TEST_BASE_URI + "IfcBuildingStorey_48";
    private static final String STOREY_ID = "kja184he";
    private static final String STOREY_NAME = "First storey";
    private static final String STOREY_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_343";
    private static final String STOREY_INST2 = TEST_BASE_URI + "IfcBuildingStorey_52";
    private static final String STOREY_2_ID = "mh1br";
    private static final String STOREY_2_NAME = "Second storey";
    private static final String STOREY_2_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_3453";
    private static final String LIVING_ROOM_INST = TEST_BASE_URI + "IfcSpace_215";
    private static final String LIVING_ROOM_ID = "la17246";
    private static final String LIVING_ROOM_NAME = "Living room";
    private static final String LIVING_ROOM_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_5525";
    private static final String BEDROOM_INST = TEST_BASE_URI + "IfcSpace_321";
    private static final String BEDROOM_ID = "eq9e71";
    private static final String BEDROOM_NAME = "Bedroom";
    private static final String BEDROOM_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_6521";
    private static final String KITCHEN_INST = TEST_BASE_URI + "IfcSpace_615";
    private static final String KITCHEN_ID = "0a126gr";
    private static final String KITCHEN_NAME = "Kitchen";
    private static final String KITCHEN_POSITION_INST = TEST_BASE_URI + "IfcLocalPlacement_7373";
    private static final String PROJECT_AGG_INST = TEST_BASE_URI + "IfcRelAggregates_5";
    private static final String BUILDING_AGG_INST = TEST_BASE_URI + "IfcRelAggregates_106";
    private static final String STOREY_AGG_INST = TEST_BASE_URI + "IfcRelAggregates_126";
    private static final String ROOM_AGG_INST = TEST_BASE_URI + "IfcRelAggregates_136";
    // Numerical literals
    private static final Double TEST_PROJECT_DIMENSIONS = 2.0;
    private static final Double TEST_PROJECT_PRECISION = 10.3;
    private static final Double TEST_SITE_DOUBLE = 25.0;
    private static final Double TEST_SITE_LAT_DEGREE = 10.21;
    private static final Double TEST_SITE_LAT_MINUTE = 10.0;
    private static final Double TEST_SITE_LAT_SEC = 1.35;
    private static final Double TEST_SITE_LAT_MIL_SEC = 0.0;
    private static final Double TEST_SITE_LONG_DEGREE = 21.5;
    private static final Double TEST_SITE_LONG_MINUTE = 20.0;
    private static final Double TEST_SITE_LONG_SEC = 3.15;
    private static final Double TEST_SITE_LONG_MIL_SEC = 5.6;
    private static final Double TEST_BUILDING_REF_ELEV_DOUBLE = 28.15;
    private static final Double TEST_BUILDING_TER_ELEV_DOUBLE = 3.6;
    private static final Double TEST_STOREY_DOUBLE = 1.2;
    private static final Double TEST_STOREY_DOUBLE2 = 3.4;
    // Properties
    private static final Property hasDouble = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasDouble");
    private static final Property hasString = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasString");
    private static final Property hasInteger = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasInteger");
    private static final Property hasContents = ResourceFactory.createProperty(JunitTestUtils.listUri + "hasContents");
    private static final Property hasNext = ResourceFactory.createProperty(JunitTestUtils.listUri + "hasNext");
    private static final Property hasName = ResourceFactory.createProperty(JunitTestUtils.IFC2X3_NAME_PROPERTY);
    private static final Property hasId = ResourceFactory.createProperty(JunitTestUtils.IFC2X3_ID_PROPERTY);
    private static final Property objectPlacement = ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "objectPlacement_IfcProduct");
    // Repeated classes
    private static Resource localPlacement;

    @BeforeEach
    void genSampleStatements() {
        sampleModel = ModelFactory.createDefaultModel();
        // Generate the IfcOwl statements in the model
        localPlacement = sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcLocalPlacement");
        // For IfcProject context properties
        Resource projectContextDimBlankNode = sampleModel.createResource();
        Resource projectContextPrecisionBlankNode = sampleModel.createResource();
        Resource projectContext= sampleModel.createResource(REP_CONTEXT_INST)
                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcGeometricRepresentationContext"))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "coordinateSpaceDimension_IfcGeometricRepresentationContext"), projectContextDimBlankNode)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "precision_IfcGeometricRepresentationContext"), projectContextPrecisionBlankNode)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "worldCoordinateSystem_IfcGeometricRepresentationContext"), ResourceFactory.createResource(WCS_INST));
        sampleModel.add(projectContextDimBlankNode, hasInteger, ResourceFactory.createTypedLiteral(TEST_PROJECT_DIMENSIONS));
        sampleModel.add(projectContextPrecisionBlankNode, hasDouble, ResourceFactory.createTypedLiteral(TEST_PROJECT_PRECISION));
        // For generic IfcProject properties
        Resource projectNameBlankNode = sampleModel.createResource();
        Resource projectPhaseBlankNode = sampleModel.createResource();
        sampleModel.createResource(PROJECT_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcProject"))
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "longName_IfcProject"), projectNameBlankNode)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "phase_IfcProject"), projectPhaseBlankNode)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "representationContexts_IfcProject"), projectContext);
        sampleModel.add(projectNameBlankNode, hasString, ResourceFactory.createPlainLiteral(PROJECT_NAME));
        sampleModel.add(projectPhaseBlankNode, hasString, ResourceFactory.createPlainLiteral(PROJECT_PHASE));
        // For IfcSite
        Resource siteBlankNode = sampleModel.createResource();
        Resource siteNameBlankNode = sampleModel.createResource();
        Resource siteIDBlankNode = sampleModel.createResource();
        Resource sitePositionNode = sampleModel.createResource(SITE_POSITION_INST).addProperty(RDF.type, localPlacement);
        sampleModel.createResource(SITE_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSite"))
                .addProperty(hasName, siteNameBlankNode)
                .addProperty(hasId,siteIDBlankNode)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "refElevation_IfcSite"),
                        siteBlankNode)
                .addProperty(objectPlacement, sitePositionNode);
        sampleModel.add(siteNameBlankNode, hasString, ResourceFactory.createPlainLiteral(SITE_NAME));
        sampleModel.add(siteIDBlankNode, hasString, ResourceFactory.createPlainLiteral(SITE_ID));
        sampleModel.add(siteBlankNode, hasDouble, ResourceFactory.createTypedLiteral(String.valueOf(TEST_SITE_DOUBLE)));
        // For IfcBuilding
        Resource buildingBlankNode = sampleModel.createResource();
        Resource buildingNameBlankNode = sampleModel.createResource();
        Resource buildingIDBlankNode = sampleModel.createResource();;
        Resource buildingPositionNode = sampleModel.createResource(BUILDING_POSITION_INST).addProperty(RDF.type, localPlacement);
        sampleModel.createResource(BUILDING_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcBuilding"))
                .addProperty(hasName, buildingNameBlankNode)
                .addProperty(hasId, buildingIDBlankNode)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "elevationOfRefHeight_IfcBuilding"),
                        buildingBlankNode)
                .addProperty(objectPlacement, buildingPositionNode);
        sampleModel.add(buildingNameBlankNode, hasString, ResourceFactory.createPlainLiteral(BUILDING_NAME));
        sampleModel.add(buildingIDBlankNode, hasString, ResourceFactory.createPlainLiteral(BUILDING_ID));
        sampleModel.add(buildingBlankNode, hasDouble, ResourceFactory.createTypedLiteral(String.valueOf(TEST_BUILDING_REF_ELEV_DOUBLE)));
        sampleModel.createResource(BUILDING_AGG_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelAggregates"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relatingObject_IfcRelDecomposes"),
                        sampleModel.getResource(SITE_INST))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDecomposes"),
                        sampleModel.getResource(BUILDING_INST));
    }

    @Test
    void testGenZoneTriplesSimpleModel() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        SpatialZoneFacade.genZoneTriples(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        // Ensure that the extra triples are not generated
        JunitTestUtils.doesExpectedListNotExist(genProjectRootZoneStatements(true), result);
        JunitTestUtils.doesExpectedListNotExist(genProjectRootZoneStatements(false), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedComplexModelStatements(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedLatLongStatements(), result);
    }

    @Test
    void testGenZoneTriplesSimpleModelSiteRoot() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        addProjectRootZoneTriples(SITE_INST);
        // Execute method
        SpatialZoneFacade.genZoneTriples(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        JunitTestUtils.doesExpectedListExist(genProjectRootZoneStatements(true), result);
        // Ensure that the extra triples are not generated
        JunitTestUtils.doesExpectedListNotExist(genProjectRootZoneStatements(false), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedComplexModelStatements(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedLatLongStatements(), result);
    }

    @Test
    void testGenZoneTriplesSimpleModelBuildingRoot() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        addProjectRootZoneTriples(BUILDING_INST);
        // Execute method
        SpatialZoneFacade.genZoneTriples(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        JunitTestUtils.doesExpectedListExist(genProjectRootZoneStatements(false), result);
        // Ensure that the extra triples are not generated
        JunitTestUtils.doesExpectedListNotExist(genProjectRootZoneStatements(true), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedComplexModelStatements(), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedLatLongStatements(), result);
    }

    @Test
    void testGenZoneTriplesNoLatLong() {
        // Set up
        addComplexModelTriples();
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        SpatialZoneFacade.genZoneTriples(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedComplexModelStatements(), result);
        // Ensure that latitude/longitude triples are not generated
        JunitTestUtils.doesExpectedListNotExist(genProjectRootZoneStatements(true), result);
        JunitTestUtils.doesExpectedListNotExist(genProjectRootZoneStatements(false), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedLatLongStatements(), result);
    }

    @Test
    void testGenZoneTriplesSimpleModelWithLatLong() {
        // Set up
        addLatLongTriples();
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        SpatialZoneFacade.genZoneTriples(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedLatLongStatements(), result);
        // Ensure that complex model and project triples are not generated
        JunitTestUtils.doesExpectedListNotExist(genProjectRootZoneStatements(true), result);
        JunitTestUtils.doesExpectedListNotExist(genProjectRootZoneStatements(false), result);
        JunitTestUtils.doesExpectedListNotExist(genExpectedComplexModelStatements(), result);
    }

    @Test
    void testGenZoneTriples() {
        // Set up
        addComplexModelTriples();
        addProjectRootZoneTriples(SITE_INST);
        addLatLongTriples();
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        SpatialZoneFacade.genZoneTriples(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedComplexModelStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedLatLongStatements(), result);
        JunitTestUtils.doesExpectedListExist(genProjectRootZoneStatements(true), result);
    }

    private void addComplexModelTriples() {
        sampleModel.createResource(REP_CONTEXT_INST)
                .addProperty(ResourceFactory.createProperty(JunitTestUtils.ifc2x3Uri + "trueNorth_IfcGeometricRepresentationContext"),
                        ResourceFactory.createResource(TRUE_NORTH_INST));
        Resource buildingBlankNode = sampleModel.createResource();
        Resource storeyBlankNode = sampleModel.createResource();
        Resource storeyNameBlankNode = sampleModel.createResource();
        Resource storeyIDBlankNode = sampleModel.createResource();
        Resource secStoreyBlankNode = sampleModel.createResource();
        Resource secStoreyNameBlankNode = sampleModel.createResource();
        Resource secStoreyIDBlankNode = sampleModel.createResource();
        // For IfcBuilding
        sampleModel.createResource(BUILDING_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcBuilding"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "elevationOfTerrain_IfcBuilding"),
                        buildingBlankNode);
        sampleModel.add(buildingBlankNode, hasDouble, ResourceFactory.createTypedLiteral(String.valueOf(TEST_BUILDING_TER_ELEV_DOUBLE)));
        // For IfcBuildingStorey
        Resource storeyPositionNode = sampleModel.createResource(STOREY_POSITION_INST).addProperty(RDF.type, localPlacement);
        sampleModel.createResource(STOREY_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcBuildingStorey"))
                .addProperty(hasName, storeyNameBlankNode)
                .addProperty(hasId, storeyIDBlankNode)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "elevation_IfcBuildingStorey"),
                        storeyBlankNode)
                .addProperty(objectPlacement, storeyPositionNode);
        sampleModel.add(storeyNameBlankNode, hasString, ResourceFactory.createPlainLiteral(STOREY_NAME));
        sampleModel.add(storeyIDBlankNode, hasString, ResourceFactory.createPlainLiteral(STOREY_ID));
        sampleModel.add(storeyBlankNode, hasDouble, ResourceFactory.createTypedLiteral(String.valueOf(TEST_STOREY_DOUBLE)));
        // For second storey
        Resource storeySecPositionNode = sampleModel.createResource(STOREY_2_POSITION_INST).addProperty(RDF.type, localPlacement);
        sampleModel.createResource(STOREY_INST2)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcBuildingStorey"))
                .addProperty(hasName, secStoreyNameBlankNode)
                .addProperty(hasId, secStoreyIDBlankNode)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "elevation_IfcBuildingStorey"),
                        secStoreyBlankNode)
                .addProperty(objectPlacement, storeySecPositionNode);
        sampleModel.add(secStoreyNameBlankNode, hasString, ResourceFactory.createPlainLiteral(STOREY_2_NAME));
        sampleModel.add(secStoreyIDBlankNode, hasString, ResourceFactory.createPlainLiteral(STOREY_2_ID));
        sampleModel.add(secStoreyBlankNode, hasDouble, ResourceFactory.createTypedLiteral(String.valueOf(TEST_STOREY_DOUBLE2)));
        sampleModel.createResource(STOREY_AGG_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelAggregates"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relatingObject_IfcRelDecomposes"),
                        sampleModel.getResource(BUILDING_INST))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDecomposes"),
                        sampleModel.getResource(STOREY_INST))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDecomposes"),
                        sampleModel.getResource(STOREY_INST2));
        // For IfcSpaces of living room
        Resource livingNameBlankNode = sampleModel.createResource();
        Resource livingIDBlankNode = sampleModel.createResource();
        Resource livingPositionNode = sampleModel.createResource(LIVING_ROOM_POSITION_INST).addProperty(RDF.type, localPlacement);
        sampleModel.createResource(LIVING_ROOM_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSpace"))
                .addProperty(hasName, livingNameBlankNode)
                .addProperty(hasId, livingIDBlankNode)
                .addProperty(objectPlacement, livingPositionNode);
        sampleModel.add(livingNameBlankNode, hasString, ResourceFactory.createPlainLiteral(LIVING_ROOM_NAME));
        sampleModel.add(livingIDBlankNode, hasString, ResourceFactory.createPlainLiteral(LIVING_ROOM_ID));
        // For IfcSpaces of bedroom
        Resource bedNameBlankNode = sampleModel.createResource();
        Resource bedIDBlankNode = sampleModel.createResource();
        Resource bedPositionNode = sampleModel.createResource(BEDROOM_POSITION_INST).addProperty(RDF.type, localPlacement);
        sampleModel.createResource(BEDROOM_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSpace"))
                .addProperty(hasName, bedNameBlankNode)
                .addProperty(hasId, bedIDBlankNode)
                .addProperty(objectPlacement, bedPositionNode);
        sampleModel.add(bedNameBlankNode, hasString, ResourceFactory.createPlainLiteral(BEDROOM_NAME));
        sampleModel.add(bedIDBlankNode, hasString, ResourceFactory.createPlainLiteral(BEDROOM_ID));
        // For IfcSpaces of kitchen
        Resource kitchenNameBlankNode = sampleModel.createResource();
        Resource kitchenIDBlankNode = sampleModel.createResource();
        Resource kitchenPositionNode = sampleModel.createResource(KITCHEN_POSITION_INST).addProperty(RDF.type, localPlacement);
        sampleModel.createResource(KITCHEN_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSpace"))
                .addProperty(hasName, kitchenNameBlankNode)
                .addProperty(hasId, kitchenIDBlankNode)
                .addProperty(objectPlacement, kitchenPositionNode);
        sampleModel.add(kitchenNameBlankNode, hasString, ResourceFactory.createPlainLiteral(KITCHEN_NAME));
        sampleModel.add(kitchenIDBlankNode, hasString, ResourceFactory.createPlainLiteral(KITCHEN_ID));
        sampleModel.createResource(ROOM_AGG_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelAggregates"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relatingObject_IfcRelDecomposes"),
                        sampleModel.getResource(STOREY_INST))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDecomposes"),
                        sampleModel.getResource(LIVING_ROOM_INST))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDecomposes"),
                        sampleModel.getResource(BEDROOM_INST))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDecomposes"),
                        sampleModel.getResource(KITCHEN_INST));
    }

    private void addProjectRootZoneTriples(String root) {
        sampleModel.createResource(PROJECT_AGG_INST)
                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcRelAggregates"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relatingObject_IfcRelDecomposes"),
                        sampleModel.getResource(PROJECT_INST))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "relatedObjects_IfcRelDecomposes"),
                        sampleModel.getResource(root));
    }

    private void addLatLongTriples() {
        Resource latitudeNode = sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcCompoundPlaneAngleMeasure_321");
        Resource longitudeNode = sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcCompoundPlaneAngleMeasure_532");
        sampleModel.createResource(SITE_INST)
                .addProperty(RDF.type, sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcCompoundPlaneAngleMeasure"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "refLatitude_IfcSite"), latitudeNode)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "refLongitude_IfcSite"), longitudeNode);
        // Latitude statements
        Resource latDegBlankNode = sampleModel.createResource();
        Resource latMinBlankNode = sampleModel.createResource();
        Resource latMinValBlankNode = sampleModel.createResource();
        Resource latSecBlankNode = sampleModel.createResource();
        Resource latSecValBlankNode = sampleModel.createResource();
        Resource latMilSecBlankNode = sampleModel.createResource();
        Resource latMilSecValBlankNode = sampleModel.createResource();
        sampleModel.add(latitudeNode, hasContents, latDegBlankNode);
        sampleModel.add(latDegBlankNode, hasInteger, ResourceFactory.createTypedLiteral(String.valueOf(TEST_SITE_LAT_DEGREE)));
        sampleModel.add(latitudeNode, hasNext, latMinBlankNode);
        sampleModel.add(latMinBlankNode, hasContents, latMinValBlankNode);
        sampleModel.add(latMinValBlankNode, hasInteger, ResourceFactory.createTypedLiteral(String.valueOf(TEST_SITE_LAT_MINUTE)));
        sampleModel.add(latMinBlankNode, hasNext, latSecBlankNode);
        sampleModel.add(latSecBlankNode, hasContents, latSecValBlankNode);
        sampleModel.add(latSecValBlankNode, hasInteger, ResourceFactory.createTypedLiteral(String.valueOf(TEST_SITE_LAT_SEC)));
        sampleModel.add(latSecBlankNode, hasNext, latMilSecBlankNode);
        sampleModel.add(latMilSecBlankNode, hasContents, latMilSecValBlankNode);
        sampleModel.add(latMilSecValBlankNode, hasInteger, ResourceFactory.createTypedLiteral(String.valueOf(TEST_SITE_LAT_MIL_SEC)));
        // Longitude statements
        Resource longDegBlankNode = sampleModel.createResource();
        Resource longMinBlankNode = sampleModel.createResource();
        Resource longMinValBlankNode = sampleModel.createResource();
        Resource longSecBlankNode = sampleModel.createResource();
        Resource longSecValBlankNode = sampleModel.createResource();
        Resource longMilSecBlankNode = sampleModel.createResource();
        Resource longMilSecValBlankNode = sampleModel.createResource();
        sampleModel.add(longitudeNode, hasContents, longDegBlankNode);
        sampleModel.add(longDegBlankNode, hasInteger, ResourceFactory.createTypedLiteral(String.valueOf(TEST_SITE_LONG_DEGREE)));
        sampleModel.add(longitudeNode, hasNext, longMinBlankNode);
        sampleModel.add(longMinBlankNode, hasContents, longMinValBlankNode);
        sampleModel.add(longMinValBlankNode, hasInteger, ResourceFactory.createTypedLiteral(String.valueOf(TEST_SITE_LONG_MINUTE)));
        sampleModel.add(longMinBlankNode, hasNext, longSecBlankNode);
        sampleModel.add(longSecBlankNode, hasContents, longSecValBlankNode);
        sampleModel.add(longSecValBlankNode, hasInteger, ResourceFactory.createTypedLiteral(String.valueOf(TEST_SITE_LONG_SEC)));
        sampleModel.add(longSecBlankNode, hasNext, longMilSecBlankNode);
        sampleModel.add(longMilSecBlankNode, hasContents, longMilSecValBlankNode);
        sampleModel.add(longMilSecValBlankNode, hasInteger, ResourceFactory.createTypedLiteral(String.valueOf(TEST_SITE_LONG_MIL_SEC)));    }

    private List<String> genExpectedStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/IfcProjectRepresentation");
        expected.add(TEST_BASE_URI + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + PROJECT_NAME);
        expected.add(TEST_BASE_URI + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasPhase, \"" + PROJECT_PHASE);
        expected.add(TEST_BASE_URI + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasContext, " + TEST_BASE_URI + "GeometricRepresentationContext_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "GeometricRepresentationContext_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/GeometricRepresentationContext");
        expected.add(TEST_BASE_URI + "GeometricRepresentationContext_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSpaceDimensions, \"" + TEST_PROJECT_DIMENSIONS);
        expected.add(TEST_BASE_URI + "GeometricRepresentationContext_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasPrecision, \"" + TEST_PROJECT_PRECISION);
        expected.add(TEST_BASE_URI + "GeometricRepresentationContext_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasWorldCoordinateSystem, " + WCS_INST);
        expected.add(TEST_BASE_URI + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + SITE_NAME);
        expected.add(TEST_BASE_URI + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + SITE_ID);
        expected.add(TEST_BASE_URI + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + SITE_POSITION_INST);
        expected.add(TEST_BASE_URI + "Site_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://w3id.org/bot#Site");
        expected.add(TEST_BASE_URI + "Site_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + TEST_BASE_URI + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Height");
        expected.add(TEST_BASE_URI + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue, " + TEST_BASE_URI + "Measure_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Measure");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_SITE_DOUBLE);
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit, " + TEST_BASE_URI + "Length_");
        expected.add(TEST_BASE_URI + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Length");
        expected.add(TEST_BASE_URI + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2004/02/skos/core#notation, \"m\"");
        expected.add(TEST_BASE_URI + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + BUILDING_NAME);
        expected.add(TEST_BASE_URI + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + BUILDING_ID);
        expected.add(TEST_BASE_URI + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + BUILDING_POSITION_INST);
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_BUILDING_REF_ELEV_DOUBLE);
        expected.add(TEST_BASE_URI + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://w3id.org/bot#Building");
        expected.add(TEST_BASE_URI + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + TEST_BASE_URI + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Site_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://w3id.org/bot#hasBuilding, " + TEST_BASE_URI + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(SITE_POSITION_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(BUILDING_POSITION_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        return expected;
    }

    private List<String> genProjectRootZoneStatements(boolean isSite) {
        List<String> expected = new ArrayList<>();
        if (isSite) {
            expected.add(TEST_BASE_URI + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRootZone, " + TEST_BASE_URI + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        } else {
            expected.add(TEST_BASE_URI + "IfcProjectRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRootZone, " + TEST_BASE_URI + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        }
        return expected;
    }

    private List<String> genExpectedLatLongStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefLatitude, " + TEST_BASE_URI + "Latitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Latitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/CompoundPlaneAngle");
        expected.add(TEST_BASE_URI + "Latitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasDegree, \"" + TEST_SITE_LAT_DEGREE);
        expected.add(TEST_BASE_URI + "Latitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasMinute, \"" + TEST_SITE_LAT_MINUTE);
        expected.add(TEST_BASE_URI + "Latitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSecond, \"" + TEST_SITE_LAT_SEC);
        expected.add(TEST_BASE_URI + "Latitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasMillionthSecond, \"" + TEST_SITE_LAT_MIL_SEC);
        expected.add(TEST_BASE_URI + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefLongitude, " + TEST_BASE_URI + "Longitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Longitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasDegree, \"" + TEST_SITE_LONG_DEGREE);
        expected.add(TEST_BASE_URI + "Longitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasMinute, \"" + TEST_SITE_LONG_MINUTE);
        expected.add(TEST_BASE_URI + "Longitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasSecond, \"" + TEST_SITE_LONG_SEC);
        expected.add(TEST_BASE_URI + "Longitude_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasMillionthSecond, \"" + TEST_SITE_LONG_MIL_SEC);
        return expected;
    }

    private List<String> genExpectedComplexModelStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "GeometricRepresentationContext_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasTrueNorth, " + TRUE_NORTH_INST);
        expected.add(TEST_BASE_URI + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasTerrainElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_BUILDING_TER_ELEV_DOUBLE);
        expected.add(TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + STOREY_NAME);
        expected.add(TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + STOREY_ID);
        expected.add(TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + STOREY_2_POSITION_INST);
        expected.add(TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + STOREY_2_NAME);
        expected.add(TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + STOREY_2_ID);
        expected.add(TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + STOREY_POSITION_INST);
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_STOREY_DOUBLE);
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_STOREY_DOUBLE2);
        expected.add(TEST_BASE_URI + "Storey_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://w3id.org/bot#Storey");
        expected.add(TEST_BASE_URI + "Storey_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://w3id.org/bot#hasStorey, " + TEST_BASE_URI + "Storey_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/Room");
        expected.add(TEST_BASE_URI + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Storey_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRoom, " + TEST_BASE_URI + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + LIVING_ROOM_NAME);
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + LIVING_ROOM_ID);
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + LIVING_ROOM_POSITION_INST);
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + BEDROOM_NAME);
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + BEDROOM_ID);
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + BEDROOM_POSITION_INST);
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + KITCHEN_NAME);
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + KITCHEN_ID);
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + KITCHEN_POSITION_INST);
        expected.add(STOREY_POSITION_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(STOREY_2_POSITION_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(LIVING_ROOM_POSITION_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(BEDROOM_POSITION_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        expected.add(KITCHEN_POSITION_INST + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LocalPlacement");
        return expected;
    }
}