package uk.ac.cam.cares.jps.agent.ifc2ontobim.utils;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.SelectBuilder;

import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.QuerySolutionMap;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Floor;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.ModelRepresentation3D;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcRoomRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.IfcStoreyRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.ElementStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.SpatialZoneStorage;


import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class QueryHandlerTest {
    private static final String inst = "Storey_514";
    private static final String secondInst = "Storey_654";
    private static final String testClass = "Storey";
    private static final String testConstructClass = "Building";
    private static final String testVar = "name";
    private static final String testLiteral = "Building1";
    private static final String testInchNameVar = "inchname";
    private static final String testLiteralWithInch = "Wall:2\"x2\":test";
    private static final String testLiteralWithInchResult = "Wall:2\\\"x2\\\":test";
    private static final String testHeightVar = "height";
    private static final String testDoubleLiteral = "102";
    private static final String testIriVar = "IRI";
    private static final String testBaseUri = "https://www.example.org/";
    private static final String testPlacementIri = testBaseUri + JunitTestUtils.BIM_PLACEMENT_CLASS + "_1041";
    private static final String testIri = testBaseUri + "Test_124";
    private static final String testParentZoneVar = "subzone";
    private static final String testParentStoreyIri = testBaseUri + "IfcBuildingStorey_51076";
    private static final String testParentRoomIri = testBaseUri + "IfcRoom_51076";
    private static final String testShapeRepVar = "instshaperep";
    private static final String testShapeRepIri = testBaseUri + "IfcShapeRepresentation_51076";
    private static final String testSubContextVar = "subcontext";
    private static final String testSubContextIri = testBaseUri + "GeometricRepresentationSubContext_5151";
    private static final String testGeomVar = "geometry";
    private static final String testGeomIri = testBaseUri + "IfcFacetedBrep_32516";
    private static final String testGeomBimIri = testBaseUri + "FacetedBrep_32516";
    private static final String testShapeRepTypeVar = "shapereptype";
    private static final String testShapeRepType = "Faceted Brep";
    private static final String testSourcePlacementVar = "geomaxisplacement";
    private static final String testSourcePlacementIri = testBaseUri + "LocalPlacement_33352";
    private static final String testTargetPlacementVar = "cartesiantransformer";
    private static final String testTargetPlacementIri = testBaseUri + "CartesianTransformationOperator_610618";
    private static final String testSecShapeRepVar = "secondinstshaperep";
    private static final String testSecShapeRepIri = testBaseUri + "IfcShapeRepresentation_151523";
    private static final String testSecSubContextVar = "secondsubcontext";
    private static final String testSecSubContextIri = testBaseUri + "GeometricRepresentationSubContext_3209";
    private static final String testSecGeomVar = "secgeometry";
    private static final String testSecGeomIri = testBaseUri + "IfcPolyline_8771";
    private static final String testSecGeomBimIri = testBaseUri + "Polyline_8771";
    private static final String testSecShapeRepTypeVar = "secshapereptype";
    private static final String testSecShapeRepType = "Curve2D";
    // For testing Void shape rep
    private static final String testVoidShapeRepVar = "voidshaperep";
    private static final String testVoidShapeRepIri = testBaseUri + "IfcShapeRepresentation_11532";
    private static final String testVoidPlacementVar = "voidplacement";
    private static final String testVoidPlacementIri = testBaseUri + "LocalPlacement_3182";
    private static final String testVoidTypeVar = "voidtype";
    private static final String testVoidType = "Opening";
    private static final String testVoidSubContextVar = "voidsubcontext";
    private static final String testVoidSubContextIri = testBaseUri + "GeometricRepresentationSubContext_3209";
    private static final String testVoidGeomVar = "voidgeometry";
    private static final String testVoidGeomIri = testBaseUri + "IfcExtrudedAreaSolid_3151";
    private static final String testVoidGeomBimIri = testBaseUri + "ExtrudedAreaSolid_3151";
    private static final String testVoidShapeRepTypeVar = "voidshapereptype";
    private static final String testVoidShapeRepType = "SweptSolid";
    private static final boolean testBooleanLiteral = true;
    private static SpatialZoneStorage zoneMappings;
    private static IfcStoreyRepresentation storey;
    private static IfcRoomRepresentation room;

    @BeforeAll
    static void addTestZoneMappings() {
        NamespaceMapper.setBaseNameSpace(testBaseUri);
        // Create a new storey and room instance, which does not require any values except for the IRI
        // This IRI is necessary to generate the respective zone IRI within the class
        storey = new IfcStoreyRepresentation(null, null, testPlacementIri, null, null, null);
        room = new IfcRoomRepresentation(null, null, testPlacementIri, null);
        // Add the storey and room to the singleton
        zoneMappings = SpatialZoneStorage.Singleton();
        zoneMappings.add(testParentStoreyIri, storey.getBotStoreyIRI());
        zoneMappings.add(testParentRoomIri, room.getBimRoomIRI());
    }
    @AfterAll
    static void resetNamespace(){
        NamespaceMapper.setBaseNameSpace("");
    }

    @Test
    void testInitSelectQueryBuilder() {
        SelectBuilder builder = QueryHandler.initSelectQueryBuilder();
        List<String> expected = this.genInitQuery();
        expected.forEach(line -> assertTrue(builder.buildString().contains(line)));
    }

    @Test
    void testExecSelectQuery() {
        Model sampleModel = this.genSampleModel();
        List<String> expected = new ArrayList<>();
        ResultSet results = QueryHandler.execSelectQuery(this.genSampleSelectQuery(), sampleModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            expected.add(soln.get("storey").toString());
        }
        assertTrue(expected.contains(JunitTestUtils.bimUri + inst));
        assertTrue(expected.contains(JunitTestUtils.bimUri + secondInst));
    }

    @Test
    void testRetrieveIri() {
        // Create a sample query solution for testing
        QuerySolutionMap solution = new QuerySolutionMap();
        solution.add(testIriVar, ResourceFactory.createResource(testIri));
        // Execute the method and ensure results are string
        assertEquals(testIri, QueryHandler.retrieveIri(solution, testIriVar));
        // If the variable does not exist, ensure that null is return
        assertNull(QueryHandler.retrieveIri(solution, "nonExisting"));
    }

    @Test
    void testRetrieveLiteral() {
        // Create a sample query solution for testing
        QuerySolutionMap solution = new QuerySolutionMap();
        solution.add(testVar, ResourceFactory.createPlainLiteral(testLiteral));
        solution.add(testInchNameVar, ResourceFactory.createPlainLiteral(testLiteralWithInch));
        solution.add(testHeightVar, ResourceFactory.createTypedLiteral(testDoubleLiteral));
        solution.add(testIriVar, ResourceFactory.createTypedLiteral(testBooleanLiteral));
        // Execute the method and ensure results are string
        assertEquals(testLiteral, QueryHandler.retrieveLiteral(solution, testVar));
        assertEquals(testLiteralWithInchResult, QueryHandler.retrieveLiteral(solution, testInchNameVar));
        assertEquals(testDoubleLiteral, QueryHandler.retrieveLiteral(solution, testHeightVar));
        assertEquals(testBooleanLiteral, Boolean.parseBoolean(QueryHandler.retrieveLiteral(solution, testIriVar)));
        // If the variable does not exist, ensure that null is return
        assertNull(QueryHandler.retrieveLiteral(solution, "nonExisting"));
    }

    @Test
    void testRetrieveHostZoneForStorey() {
        // Create a sample query solution for testing
        QuerySolutionMap solution = new QuerySolutionMap();
        solution.add(testParentZoneVar, ResourceFactory.createResource(testParentStoreyIri));
        // Execute the method and ensure the expected host zone IRI belongs to the sample storey instance
        assertEquals(storey.getBotStoreyIRI(), QueryHandler.retrieveHostZone(solution, zoneMappings));
    }

    @Test
    void testRetrieveHostZoneForRoom() {
        // Create a sample query solution for testing
        QuerySolutionMap solution = new QuerySolutionMap();
        solution.add(testParentZoneVar, ResourceFactory.createResource(testParentRoomIri));
        // Execute the method and ensure the expected host zone IRI belongs to the sample room instance
        assertEquals(room.getBimRoomIRI(), QueryHandler.retrieveHostZone(solution, zoneMappings));
    }

    @Test
    void testRetrieveModelRepresentation3D() {
        // Set up a sampleSet
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Create a sample query solution for testing
        QuerySolutionMap solution = new QuerySolutionMap();
        solution.add(testShapeRepVar, ResourceFactory.createResource(testShapeRepIri));
        solution.add(testSubContextVar, ResourceFactory.createResource(testSubContextIri));
        solution.add(testGeomVar, ResourceFactory.createResource(testGeomIri));
        solution.add(testShapeRepTypeVar, ResourceFactory.createPlainLiteral(testShapeRepType));
        solution.add(testSourcePlacementVar, ResourceFactory.createResource(testSourcePlacementIri));
        solution.add(testTargetPlacementVar, ResourceFactory.createResource(testTargetPlacementIri));
        // Execute the method and extract the result statements into a string
        ModelRepresentation3D resultModel = QueryHandler.retrieveModelRepresentation3D(solution);
        resultModel.addModelRepresentation3DStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonModelRep3DStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedOptionalModelRep3DStatements(), result);
    }

    @Test
    void testRetrieveModelRepresentation3DNoOptionalValues() {
        // Set up a sampleSet
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Create a sample query solution for testing
        QuerySolutionMap solution = new QuerySolutionMap();
        solution.add(testShapeRepVar, ResourceFactory.createResource(testShapeRepIri));
        solution.add(testSubContextVar, ResourceFactory.createResource(testSubContextIri));
        solution.add(testGeomVar, ResourceFactory.createResource(testGeomIri));
        // Execute the method and extract the result statements into a string
        ModelRepresentation3D resultModel = QueryHandler.retrieveModelRepresentation3D(solution);
        resultModel.addModelRepresentation3DStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedCommonModelRep3DStatements(), result);
        // Verify that the following statements do not exist
        JunitTestUtils.doesExpectedListNotExist(genExpectedOptionalModelRep3DStatements(), result);
    }

    @Test
    void testRetrieveModelRepresentation3DForSecondRep() {
        // Set up a sampleSet
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        // Create a sample query solution for testing
        QuerySolutionMap solution = new QuerySolutionMap();
        solution.add(testSecShapeRepVar, ResourceFactory.createResource(testSecShapeRepIri));
        solution.add(testSecSubContextVar, ResourceFactory.createResource(testSecSubContextIri));
        solution.add(testSecGeomVar, ResourceFactory.createResource(testSecGeomIri));
        solution.add(testSecShapeRepTypeVar, ResourceFactory.createPlainLiteral(testSecShapeRepType));
        // Execute the method and extract the result statements into a string
        ModelRepresentation3D resultModel = QueryHandler.retrieveModelRepresentation3D(solution, testSecSubContextVar, testSecGeomVar, testSecShapeRepTypeVar);
        resultModel.addModelRepresentation3DStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedAdditionalModelRep3DStatements(testSecSubContextIri, testSecShapeRepType, testSecGeomBimIri), result);
    }

    @Test
    void testAddVoidGeometryStatements() {
        // Set up required objects
        LinkedHashSet<Statement> sampleSet = new LinkedHashSet<>();
        Floor sampleFloor = new Floor(null, null, testPlacementIri, null, null);
        ElementStorage sampleElementMappings = ElementStorage.Singleton();
        // Create a sample query solution for testing
        QuerySolutionMap solution = new QuerySolutionMap();
        solution.add(testVoidShapeRepVar, ResourceFactory.createResource(testVoidShapeRepIri));
        solution.add(testVoidPlacementVar, ResourceFactory.createResource(testVoidPlacementIri));
        solution.add(testVoidTypeVar, ResourceFactory.createPlainLiteral(testVoidType));
        solution.add(testVoidSubContextVar, ResourceFactory.createResource(testVoidSubContextIri));
        solution.add(testVoidGeomVar, ResourceFactory.createResource(testVoidGeomIri));
        solution.add(testVoidShapeRepTypeVar, ResourceFactory.createPlainLiteral(testVoidShapeRepType));
        // Execute the method and extract the result statements into a string
        QueryHandler.addVoidGeometryStatements(solution, sampleFloor.getIfcRepIri(), sampleSet, sampleElementMappings);
        // Verify that the void's model representation have been added to mappings
        assertTrue(sampleElementMappings.containsModelRepIri(testVoidShapeRepIri));
        // Generate all the result statements
        sampleElementMappings.constructModelRepStatements(sampleSet);
        String result = JunitTestUtils.appendStatementsAsString(sampleSet);
        // Generated expected statement lists and verify their existence
        JunitTestUtils.doesExpectedListExist(genExpectedGeometricVoidStatements(), result);
        JunitTestUtils.doesExpectedListExist(genExpectedAdditionalModelRep3DStatements(testVoidSubContextIri, testVoidShapeRepType, testVoidGeomBimIri), result);
    }

    private List<String> genInitQuery() {
        List<String> expected = new ArrayList<>();
        expected.add("PREFIX  rdf:  <https://www.w3.org/1999/02/22-rdf-syntax-ns#>");
        expected.add("PREFIX  bot:  <https://w3id.org/bot#>");
        expected.add("PREFIX  bim:  <https://www.theworldavatar.com/kg/ontobim/>");
        expected.add("PREFIX  ifc:  <https://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#>");
        expected.add("PREFIX  rdfs: <https://www.w3.org/2000/01/rdf-schema#>");
        expected.add("PREFIX  express: <https://w3id.org/express#>");
        expected.add("PREFIX  list: <https://w3id.org/list#>");
        expected.add("PREFIX  om:   <https://www.ontology-of-units-of-measure.org/resource/om-2/>");
        expected.add("SELECT DISTINCT  *");
        expected.add("WHERE");
        return expected;
    }

    private Model genSampleModel() {
        Model sampleModel = ModelFactory.createDefaultModel();
        sampleModel.createResource(JunitTestUtils.bimUri + inst)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.bimUri + testClass));
        sampleModel.createResource(JunitTestUtils.bimUri + secondInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.bimUri + testClass));
        return sampleModel;
    }

    private String genSampleSelectQuery() {
        SelectBuilder builder = new SelectBuilder();
        builder.addPrefix("bim", JunitTestUtils.bimUri);
        builder.addVar("?storey").addWhere("?storey", RDF.type, "bim:" + testClass);
        return builder.buildString();
    }

    private String genSampleConstructQuery() {
        ConstructBuilder builder = new ConstructBuilder();
        builder.addPrefix("bim", JunitTestUtils.bimUri);
        builder.addConstruct("?storey", RDF.type, "bim:" + testConstructClass)
                .addWhere("?storey", RDF.type, "bim:" + testClass);
        return builder.buildString();
    }

    private List<String> genExpectedCommonModelRep3DStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/ModelRepresentation3D");
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasSubContext, " + testSubContextIri);
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + testGeomBimIri);
        return expected;
    }

    private List<String> genExpectedOptionalModelRep3DStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRepresentationType, \"" + testShapeRepType);
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasSourcePlacement, " + testSourcePlacementIri);
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasTargetPlacement, " + testTargetPlacementIri);
        return expected;
    }

    private List<String> genExpectedAdditionalModelRep3DStatements(String subContextIRI, String shapeRepType, String geomIRI) {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/ModelRepresentation3D");
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasSubContext, " + subContextIRI);
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRepresentationType, \"" + shapeRepType);
        expected.add(testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasRepresentationItem, " + geomIRI);
        return expected;
    }

    private List<String> genExpectedGeometricVoidStatements() {
        List<String> expected = new ArrayList<>();
        expected.add(testBaseUri + "IfcModelRepresentation_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasVoid, " + testBaseUri + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/GeometricVoid");
        expected.add(testBaseUri + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasGeometricRepresentation, " + testBaseUri + "ModelRepresentation3D_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(testBaseUri + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasVoidType, \"" + testVoidType);
        expected.add(testBaseUri + "GeometricVoid_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + testVoidPlacementIri);
        return expected;
    }
}