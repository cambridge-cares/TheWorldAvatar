package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

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
    private static final String SITE_INST = TEST_BASE_URI + "IfcSite_16";
    private static final String SITE_ID = "1204avak981";
    private static final String SITE_NAME = "Land boundary";
    private static final String BUILDING_INST = TEST_BASE_URI + "IfcBuilding_32";
    private static final String BUILDING_ID = "maso9127";
    private static final String BUILDING_NAME = "Host building";
    private static final String STOREY_INST = TEST_BASE_URI + "IfcBuildingStorey_48";
    private static final String STOREY_ID = "kja184he";
    private static final String STOREY_NAME = "First storey";
    private static final String STOREY_INST2 = TEST_BASE_URI + "IfcBuildingStorey_52";
    private static final String STOREY_2_ID = "mh1br";
    private static final String STOREY_2_NAME = "Second storey";
    private static final String LIVING_ROOM_INST = TEST_BASE_URI + "IfcSpace_215";
    private static final String LIVING_ROOM_ID = "la17246";
    private static final String LIVING_ROOM_NAME = "Living room";
    private static final String BEDROOM_INST = TEST_BASE_URI + "IfcSpace_321";
    private static final String BEDROOM_ID = "eq9e71";
    private static final String BEDROOM_NAME = "Bedroom";
    private static final String KITCHEN_INST = TEST_BASE_URI + "IfcSpace_615";
    private static final String KITCHEN_ID = "0a126gr";
    private static final String KITCHEN_NAME = "Kitchen";
    private static final String BUILDING_AGG_INST = TEST_BASE_URI + "IfcRelAggregates_106";
    private static final String STOREY_AGG_INST = TEST_BASE_URI + "IfcRelAggregates_126";
    private static final String ROOM_AGG_INST = TEST_BASE_URI + "IfcRelAggregates_136";
    private static final Double TEST_SITE_DOUBLE = 25.0;
    private static final Double TEST_BUILDING_REF_ELEV_DOUBLE = 28.15;
    private static final Double TEST_BUILDING_TER_ELEV_DOUBLE = 3.6;
    private static final Double TEST_STOREY_DOUBLE = 1.2;
    private static final Double TEST_STOREY_DOUBLE2 = 3.4;
    private static final Property hasDouble = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasDouble");
    private static final Property hasString = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasString");
    private static final Property hasName = ResourceFactory.createProperty(JunitTestUtils.IFC2X3_NAME_PROPERTY);
    private static final Property hasId = ResourceFactory.createProperty(JunitTestUtils.IFC2X3_ID_PROPERTY);


    @BeforeEach
    void genSampleStatements() {
        sampleModel = ModelFactory.createDefaultModel();
        Resource siteBlankNode = sampleModel.createResource();
        Resource siteNameBlankNode = sampleModel.createResource();
        Resource siteIDBlankNode = sampleModel.createResource();
        Resource buildingBlankNode = sampleModel.createResource();
        Resource buildingNameBlankNode = sampleModel.createResource();
        Resource buildingIDBlankNode = sampleModel.createResource();;
        // Generate the IfcOwl statements in the model
        // For IfcSite
        sampleModel.createResource(SITE_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSite"))
                .addProperty(hasName, siteNameBlankNode)
                .addProperty(hasId,siteIDBlankNode)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "refElevation_IfcSite"),
                        siteBlankNode);
        sampleModel.add(siteNameBlankNode, hasString, ResourceFactory.createPlainLiteral(SITE_NAME));
        sampleModel.add(siteIDBlankNode, hasString, ResourceFactory.createPlainLiteral(SITE_ID));
        sampleModel.add(siteBlankNode, hasDouble, ResourceFactory.createTypedLiteral(String.valueOf(TEST_SITE_DOUBLE)));
        // For IfcBuilding
        sampleModel.createResource(BUILDING_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcBuilding"))
                .addProperty(hasName, buildingNameBlankNode)
                .addProperty(hasId, buildingIDBlankNode)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "elevationOfRefHeight_IfcBuilding"),
                        buildingBlankNode);
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
    void testGenZoneTriplesOptionalValuesNotAvailable() {
        // Set up
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        SpatialZoneFacade.genZoneTriples(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        // Ensure that the building terrain elevation triples are not generated
        JunitTestUtils.doesExpectedListNotExist(genExpectedAdditionalStatements(), result);
    }

    @Test
    void testGenZoneTriples() {
        // Set up
        addAdditionalTriples();
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Execute method
        SpatialZoneFacade.genZoneTriples(sampleModel, sampleSet);
        // Clean up results as one string
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAdditionalStatements(), result);
    }

    private void addAdditionalTriples() {
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
        sampleModel.createResource(STOREY_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcBuildingStorey"))
                .addProperty(hasName, storeyNameBlankNode)
                .addProperty(hasId, storeyIDBlankNode)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "elevation_IfcBuildingStorey"),
                        storeyBlankNode);
        sampleModel.add(storeyNameBlankNode, hasString, ResourceFactory.createPlainLiteral(STOREY_NAME));
        sampleModel.add(storeyIDBlankNode, hasString, ResourceFactory.createPlainLiteral(STOREY_ID));
        sampleModel.add(storeyBlankNode, hasDouble, ResourceFactory.createTypedLiteral(String.valueOf(TEST_STOREY_DOUBLE)));
        sampleModel.createResource(STOREY_INST2)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcBuildingStorey"))
                .addProperty(hasName, secStoreyNameBlankNode)
                .addProperty(hasId, secStoreyIDBlankNode)
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "elevation_IfcBuildingStorey"),
                        secStoreyBlankNode);
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
        // For IfcSpaces
        Resource livingNameBlankNode = sampleModel.createResource();
        Resource livingIDBlankNode = sampleModel.createResource();
        Resource bedNameBlankNode = sampleModel.createResource();
        Resource bedIDBlankNode = sampleModel.createResource();
        Resource kitchenNameBlankNode = sampleModel.createResource();
        Resource kitchenIDBlankNode = sampleModel.createResource();
        sampleModel.createResource(LIVING_ROOM_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSpace"))
                .addProperty(hasName, livingNameBlankNode)
                .addProperty(hasId, livingIDBlankNode);
        sampleModel.add(livingNameBlankNode, hasString, ResourceFactory.createPlainLiteral(LIVING_ROOM_NAME));
        sampleModel.add(livingIDBlankNode, hasString, ResourceFactory.createPlainLiteral(LIVING_ROOM_ID));
        sampleModel.createResource(BEDROOM_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSpace"))
                .addProperty(hasName, bedNameBlankNode)
                .addProperty(hasId, bedIDBlankNode);
        sampleModel.add(bedNameBlankNode, hasString, ResourceFactory.createPlainLiteral(BEDROOM_NAME));
        sampleModel.add(bedIDBlankNode, hasString, ResourceFactory.createPlainLiteral(BEDROOM_ID));
        sampleModel.createResource(KITCHEN_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSpace"))
                .addProperty(hasName, kitchenNameBlankNode)
                .addProperty(hasId, kitchenIDBlankNode);
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

    private List<String> genExpectedStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + SITE_NAME);
        expected.add(TEST_BASE_URI + "IfcSiteRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + SITE_ID);
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
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_BUILDING_REF_ELEV_DOUBLE);
        expected.add(TEST_BASE_URI + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://w3id.org/bot#Building");
        expected.add(TEST_BASE_URI + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + TEST_BASE_URI + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Site_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://w3id.org/bot#hasBuilding, " + TEST_BASE_URI + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }

    private List<String> genExpectedAdditionalStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(TEST_BASE_URI + "IfcBuildingRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasTerrainElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_BUILDING_TER_ELEV_DOUBLE);
        expected.add(TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + STOREY_NAME);
        expected.add(TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + STOREY_ID);
        expected.add(TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + STOREY_2_NAME);
        expected.add(TEST_BASE_URI + "IfcStoreyRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + STOREY_2_ID);
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
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + BEDROOM_NAME);
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + BEDROOM_ID);
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2000/01/rdf-schema#label, \"" + KITCHEN_NAME);
        expected.add(TEST_BASE_URI + "IfcRoomRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcId, \"" + KITCHEN_ID);
        return expected;
    }
}