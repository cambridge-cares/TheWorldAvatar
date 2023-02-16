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
    private static final String BUILDING_INST = TEST_BASE_URI + "IfcBuilding_16";

    private static final String STOREY_INST = TEST_BASE_URI + "IfcBuildingStorey_16";
    private static final String STOREY_INST2 = TEST_BASE_URI + "IfcBuildingStorey_42";
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
        JunitTestUtils.doesExpectedListNotExist(genExpectedAdditionalElevationStatements(), result);
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
        JunitTestUtils.doesExpectedListExist(genExpectedAdditionalElevationStatements(), result);
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
    }

    private List<String> genExpectedStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(SITE_INST + ", http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Height");
        expected.add(TEST_BASE_URI + "Height_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue, " + TEST_BASE_URI + "Measure_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Measure");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_SITE_DOUBLE);
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit, " + TEST_BASE_URI + "Length_");
        expected.add(TEST_BASE_URI + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.ontology-of-units-of-measure.org/resource/om-2/Length");
        expected.add(TEST_BASE_URI + "Length_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.w3.org/2004/02/skos/core#notation, \"m\"");
        expected.add(BUILDING_INST + ", http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_BUILDING_REF_ELEV_DOUBLE);
        return expected;
    }

    private List<String> genExpectedAdditionalElevationStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(BUILDING_INST + ", http://www.theworldavatar.com/kg/ontobim/hasTerrainElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_BUILDING_TER_ELEV_DOUBLE);
        expected.add(STOREY_INST + ", http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_STOREY_DOUBLE);
        expected.add(STOREY_INST2 + ", http://www.theworldavatar.com/kg/ontobim/hasRefElevation, " + TEST_BASE_URI + "Height_");
        expected.add(TEST_BASE_URI + "Measure_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue, \"" + TEST_STOREY_DOUBLE2);
        return expected;
    }
}