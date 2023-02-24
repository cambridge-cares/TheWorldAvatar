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
    private static final String BUILDING_INST = TEST_BASE_URI + "IfcBuilding_32";
    private static final String STOREY_INST = TEST_BASE_URI + "IfcBuildingStorey_48";
    private static final String STOREY_INST2 = TEST_BASE_URI + "IfcBuildingStorey_52";
    private static final String LIVING_ROOM_INST = TEST_BASE_URI + "IfcSpace_215";
    private static final String BEDROOM_INST = TEST_BASE_URI + "IfcSpace_321";
    private static final String KITCHEN_INST = TEST_BASE_URI + "IfcSpace_615";
    private static final String BUILDING_AGG_INST = TEST_BASE_URI + "IfcRelAggregates_106";
    private static final String STOREY_AGG_INST = TEST_BASE_URI + "IfcRelAggregates_126";
    private static final String ROOM_AGG_INST = TEST_BASE_URI + "IfcRelAggregates_136";
    private static final Double TEST_SITE_DOUBLE = 25.0;
    private static final Double TEST_BUILDING_REF_ELEV_DOUBLE = 28.15;
    private static final Double TEST_BUILDING_TER_ELEV_DOUBLE = 3.6;
    private static final Double TEST_STOREY_DOUBLE = 1.2;
    private static final Double TEST_STOREY_DOUBLE2 = 3.4;

    @BeforeEach
    void genSampleStatements() {
        sampleModel = ModelFactory.createDefaultModel();
        Property hasDouble = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasDouble");
        Resource siteBlankNode = sampleModel.createResource();
        Resource buildingBlankNode = sampleModel.createResource();
        // Generate the IfcOwl statements in the model
        // For IfcSite
        sampleModel.createResource(SITE_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSite"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "refElevation_IfcSite"),
                        siteBlankNode);
        sampleModel.add(siteBlankNode, hasDouble, ResourceFactory.createTypedLiteral(String.valueOf(TEST_SITE_DOUBLE)));
        // For IfcBuilding
        sampleModel.createResource(BUILDING_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcBuilding"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "elevationOfRefHeight_IfcBuilding"),
                        buildingBlankNode);
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
        Property hasDouble = ResourceFactory.createProperty(JunitTestUtils.expressUri + "hasDouble");
        Resource buildingBlankNode = sampleModel.createResource();
        Resource storeyBlankNode = sampleModel.createResource();
        Resource secStoreyBlankNode = sampleModel.createResource();
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
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "elevation_IfcBuildingStorey"),
                        storeyBlankNode);
        sampleModel.add(storeyBlankNode, hasDouble, ResourceFactory.createTypedLiteral(String.valueOf(TEST_STOREY_DOUBLE)));
        sampleModel.createResource(STOREY_INST2)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcBuildingStorey"))
                .addProperty(sampleModel.createProperty(JunitTestUtils.ifc2x3Uri + "elevation_IfcBuildingStorey"),
                        secStoreyBlankNode);
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
        sampleModel.createResource(LIVING_ROOM_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSpace"));
        sampleModel.createResource(BEDROOM_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSpace"));
        sampleModel.createResource(KITCHEN_INST)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.ifc2x3Uri + "IfcSpace"));
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
        expected.add(SITE_INST + ", http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "Site_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://w3id.org/bot#Site");
        expected.add(TEST_BASE_URI + "Site_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + SITE_INST);
        expected.add(TEST_BASE_URI + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Height");
        expected.add(TEST_BASE_URI + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue, " + TEST_BASE_URI + "Measure_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Measure");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_SITE_DOUBLE);
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit, " + TEST_BASE_URI + "Length_");
        expected.add(TEST_BASE_URI + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Length");
        expected.add(TEST_BASE_URI + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2004/02/skos/core#notation, \"m\"");
        expected.add(BUILDING_INST + ", http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_BUILDING_REF_ELEV_DOUBLE);
        expected.add(TEST_BASE_URI + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://w3id.org/bot#Building");
        expected.add(TEST_BASE_URI + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + BUILDING_INST);
        expected.add(TEST_BASE_URI + "Site_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://w3id.org/bot#hasBuilding, " + TEST_BASE_URI + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }

    private List<String> genExpectedAdditionalStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(BUILDING_INST + ", http://www.theworldavatar.com/kg/ontobim/hasTerrainElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_BUILDING_TER_ELEV_DOUBLE);
        expected.add(STOREY_INST + ", http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_STOREY_DOUBLE);
        expected.add(STOREY_INST2 + ", http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_STOREY_DOUBLE2);
        expected.add(TEST_BASE_URI + "Storey_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://w3id.org/bot#Storey");
        expected.add(TEST_BASE_URI + "Storey_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + STOREY_INST);
        expected.add(TEST_BASE_URI + "Storey_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + STOREY_INST2);
        expected.add(TEST_BASE_URI + "Building_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://w3id.org/bot#hasStorey, " + TEST_BASE_URI + "Storey_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(TEST_BASE_URI + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/Room");
        expected.add(TEST_BASE_URI + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + LIVING_ROOM_INST);
        expected.add(TEST_BASE_URI + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + BEDROOM_INST);
        expected.add(TEST_BASE_URI + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasIfcRepresentation, " + KITCHEN_INST);
        expected.add(TEST_BASE_URI + "Storey_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.theworldavatar.com/kg/ontobim/hasRoom, " + TEST_BASE_URI + "Room_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        return expected;
    }
}