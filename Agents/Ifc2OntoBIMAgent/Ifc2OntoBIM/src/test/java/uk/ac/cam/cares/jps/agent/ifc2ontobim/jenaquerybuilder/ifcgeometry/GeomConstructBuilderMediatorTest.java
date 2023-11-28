package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcgeometry;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

class GeomConstructBuilderMediatorTest {
    private static ConstructBuilder builder;
    private static GeomConstructBuilderMediator mediator;
    private static Model sampleModel;
    private static List<RDFNode> nodeList;
    private static final String ifcClass = "Ifc";// This class does not matter, it's only a placeholder
    private static final String bimUri = "http://www.theworldavatar.com/ontology/ontobim/ontoBIM#";
    private static final String booleanClippingResInst = "IfcBooleanClippingResult_50140";
    private static final String polyHalfSpaceInst = "IfcPolygonalBoundedHalfSpace_150";
    private static final String facetedBrepInst = "IfcFacetedBrep_992";
    private static final String extrudedAreaSolidInst = "IfcExtrudedAreaSolid_2140";
    private static final String polylineInst = "IfcPolyline_3151";
    private static final String geomRepSubContextInst = "IfcGeometricRepresentationSubContext_333";
    private static final String localPlacementInst = "IfcLocalPlacement_1236";
    private static final String cartTransformerInst = "IfcCartesianTransformationOperator3D_1236";
    private static final String directionVectorInst = "IfcDirection_5836";
    private static final String cartPointInst = "IfcCartesianPoint_720";

    @BeforeEach
    void initialiseTest() {
        mediator = new GeomConstructBuilderMediator();
        builder = new ConstructBuilder();
        JunitTestUtils.addPrefix(builder);
        sampleModel = ModelFactory.createDefaultModel();
    }

    @Test
    void testCreateSparqlQueryForBooleanClippingResult() {
        genSampleRdfNodeForBooleanClippingResult();
        // sample IRI must contain IfcBooleanClippingResult
        String query = mediator.createSparqlQuery(builder, nodeList);
        String expected = genExpectedResultsForBooleanResultClipper();
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryForPolygonalBoundedHalfSpace() {
        genSampleRdfNodeForPolygonalBoundedHalfSpace();
        // sample IRI must contain IfcPolygonalBoundedHalfSpace
        String query = mediator.createSparqlQuery(builder, nodeList);
        String expected = genExpectedResultsForPolygonalBoundedHalfSpace();
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryForFacetedBrep() {
        genSampleRdfNodeForFacetedBrep();
        // sample IRI must contain IfcFacetedBrep
        String query = mediator.createSparqlQuery(builder, nodeList);
        String expected = genExpectedResultsForFacetedBrep();
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryForExtrudedAreaSolid() {
        genSampleRdfNodeForExtrudedSolidArea();
        // sample IRI must contain IfcExtrudedAreaSolid
        String query = mediator.createSparqlQuery(builder, nodeList);
        String expected = genExpectedResultsForExtrudedAreaSolid();
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryForPolyline() {
        genSampleRdfNodeForPolyline();
        // sample IRI must contain IfcPolyline
        String query = mediator.createSparqlQuery(builder, nodeList);
        String expected = genExpectedResultsForPolyline();
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryForGeometricRepresentationSubContext() {
        genSampleRdfNodeForGeometricRepresentationSubContext();
        // sample IRI must contain IfcGeometricRepresentationSubContext
        String query = mediator.createSparqlQuery(builder, nodeList);
        String expected = genExpectedResultsForGeomRepSubContext();
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryForLocalPlacement() {
        genSampleRdfNodeForLocalPlacement();
        // sample IRI must contain IfcLocalPlacement
        String query = mediator.createSparqlQuery(builder, nodeList);
        String expected = genExpectedResultsForLocalPlacement();
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryForCartesianTransformer() {
        genSampleRdfNodeForCartesianTransformer();
        // sample IRI must contain IfcDirection
        String query = mediator.createSparqlQuery(builder, nodeList);
        String expected = genExpectedResultsForCartTransformer();
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryForDirectionVector() {
        genSampleRdfNodeForDirectionVector();
        // sample IRI must contain IfcDirection
        String query = mediator.createSparqlQuery(builder, nodeList);
        String expected = genExpectedResultsForDirectionVector();
        assertTrue(query.contains(expected));
    }

    @Test
    void testCreateSparqlQueryForCartesianPoint() {
        genSampleRdfNodeForCartesianPoint();
        // sample IRI must contain IfcCartesianPoint
        String query = mediator.createSparqlQuery(builder, nodeList);
        String expected = genExpectedResultsForCartPoint();
        assertTrue(query.contains(expected));
    }

    private void genSampleRdfNodeForBooleanClippingResult() {
        // In generating test nodes, ensure that the Object node must include the test IfcClass
        sampleModel.createResource(bimUri + ifcClass)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + booleanClippingResInst));
        retrieveNodeFromModel(sampleModel);
    }

    private void genSampleRdfNodeForPolygonalBoundedHalfSpace() {
        // In generating test nodes, ensure that the Object node must include the test IfcClass
        sampleModel.createResource(bimUri + ifcClass)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + polyHalfSpaceInst));
        retrieveNodeFromModel(sampleModel);
    }

    private void genSampleRdfNodeForFacetedBrep() {
        // In generating test nodes, ensure that the Object node must include the test IfcClass
        sampleModel.createResource(bimUri + ifcClass)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + facetedBrepInst));
        retrieveNodeFromModel(sampleModel);
    }

    private void genSampleRdfNodeForExtrudedSolidArea() {
        // In generating test nodes, ensure that the Object node must include the test IfcClass
        sampleModel.createResource(bimUri + ifcClass)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + extrudedAreaSolidInst));
        retrieveNodeFromModel(sampleModel);
    }

    private void genSampleRdfNodeForPolyline() {
        // In generating test nodes, ensure that the Object node must include the test IfcClass
        sampleModel.createResource(bimUri + ifcClass)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + polylineInst));
        retrieveNodeFromModel(sampleModel);
    }

    private void genSampleRdfNodeForGeometricRepresentationSubContext() {
        // In generating test nodes, ensure that the Object node must include the test IfcClass
        sampleModel.createResource(bimUri + ifcClass)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + geomRepSubContextInst));
        retrieveNodeFromModel(sampleModel);
    }

    private void genSampleRdfNodeForLocalPlacement() {
        // In generating test nodes, ensure that the Object node must include the test IfcClass
        sampleModel.createResource(bimUri + ifcClass)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + localPlacementInst));
        retrieveNodeFromModel(sampleModel);
    }

    private void genSampleRdfNodeForCartesianTransformer() {
        // In generating test nodes, ensure that the Object node must include the test IfcClass
        sampleModel.createResource(bimUri + ifcClass)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + cartTransformerInst));
        retrieveNodeFromModel(sampleModel);
    }

    private void genSampleRdfNodeForDirectionVector() {
        // In generating test nodes, ensure that the Object node must include the test IfcClass
        sampleModel.createResource(bimUri + ifcClass)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + directionVectorInst));
        retrieveNodeFromModel(sampleModel);
    }

    private void genSampleRdfNodeForCartesianPoint() {
        // In generating test nodes, ensure that the Object node must include the test IfcClass
        sampleModel.createResource(bimUri + ifcClass)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + cartPointInst));
        retrieveNodeFromModel(sampleModel);
    }

    private void retrieveNodeFromModel(Model sampleModel) {
        // Add Object RDFNode into nodelist
        nodeList = new ArrayList<>();
        StmtIterator iter = sampleModel.listStatements();
        while (iter.hasNext()) {
            Statement stmt = iter.nextStatement();
            nodeList.add(stmt.getObject());
        }
    }

    private String genExpectedResultsForBooleanResultClipper() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?clippingres rdf:type bim:BooleanClippingResult .\n")
                .append("    ?clippingres bim:hasBooleanOperator ?booleanoperator .\n")
                .append("    ?clippingres bim:hasFirstOperand ?operand1 .\n")
                .append("    ?operand1 rdf:type ?operand1class .\n")
                .append("    ?clippingres bim:hasSecondOperand ?operand2 .\n")
                .append("    ?operand2 rdf:type ?operand2class .\n")
                .append("    ?operand1 bim:hasBooleanOperator ?operand1operator .\n")
                .append("    ?operand1 bim:hasFirstOperand ?operand11 .\n")
                .append("    ?operand11 rdf:type ?operand11class .\n")
                .append("    ?operand1 bim:hasSecondOperand ?operand12 .\n")
                .append("    ?operand12 rdf:type ?operand12class .\n")
                .append("    ?operand2 bim:hasBooleanOperator ?operand2operator .\n")
                .append("    ?operand2 bim:hasFirstOperand ?operand21 .\n")
                .append("    ?operand21 rdf:type ?operand21class .\n")
                .append("    ?operand2 bim:hasSecondOperand ?operand22 .\n")
                .append("    ?operand22 rdf:type ?operand22class .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?clippingres  rdf:type          ifc:IfcBooleanClippingResult ;\n")
                .append("              ifc:operator_IfcBooleanResult  ?booleanoperator\n")
                .append("      { ?clippingres  ifc:firstOperand_IfcBooleanResult  ?operand1 .\n")
                .append("        ?operand1  rdf:type             ?operand1class\n")
                .append("        FILTER NOT EXISTS { ?operand1  rdf:type  ifc:IfcBooleanClippingResult }\n")
                .append("      }\n")
                .append("    UNION\n")
                .append("      { ?clippingres  ifc:firstOperand_IfcBooleanResult  ?operand1 .\n")
                .append("        ?operand1  rdf:type             ifc:IfcBooleanClippingResult\n")
                .append("        BIND(bim:BooleanClippingResult AS ?operand1class)\n")
                .append("        ?operand1  ifc:operator_IfcBooleanResult  ?operand1operator ;\n")
                .append("                  ifc:firstOperand_IfcBooleanResult  ?operand11 .\n")
                .append("        ?operand11  rdf:type            ?operand11class .\n")
                .append("        ?operand1  ifc:secondOperand_IfcBooleanResult  ?operand12 .\n")
                .append("        ?operand12  rdf:type            ?operand12class\n")
                .append("      }\n")
                .append("      { ?clippingres  ifc:secondOperand_IfcBooleanResult  ?operand2 .\n")
                .append("        ?operand2  rdf:type             ?operand2class\n")
                .append("        FILTER NOT EXISTS { ?operand2  rdf:type  ifc:IfcBooleanClippingResult }\n")
                .append("      }\n")
                .append("    UNION\n")
                .append("      { ?clippingres  ifc:secondOperand_IfcBooleanResult  ?operand2 .\n")
                .append("        ?operand2  rdf:type             ifc:IfcBooleanClippingResult\n")
                .append("        BIND(bim:BooleanClippingResult AS ?operand2class)\n")
                .append("        ?operand2  ifc:operator_IfcBooleanResult  ?operand2operator ;\n")
                .append("                  ifc:firstOperand_IfcBooleanResult  ?operand21 .\n")
                .append("        ?operand21  rdf:type            ?operand21class .\n")
                .append("        ?operand2  ifc:secondOperand_IfcBooleanResult  ?operand22 .\n")
                .append("        ?operand22  rdf:type            ?operand22class\n")
                .append("      }\n")
                .append("    VALUES ?clippingres { bim:" + booleanClippingResInst + " }\n")
                .append("  }");
        return expected.toString();
    }

    private String genExpectedResultsForPolygonalBoundedHalfSpace() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?polyspace rdf:type bim:PolygonalBoundedHalfSpace .\n")
                .append("    ?polyspace bim:hasAgreementFlag ?boolean .\n")
                .append("    ?polyspace bim:hasRefPoint ?spacecartesianpoint .\n")
                .append("    ?polyspace bim:hasAxisDirection ?axisdirection .\n")
                .append("    ?polyspace bim:hasRefDirection ?refdirection .\n")
                .append("    ?polyspace bim:hasBaseSurface ?plane .\n")
                .append("    ?plane rdf:type bim:SurfacePlane .\n")
                .append("    ?plane bim:hasRefPoint ?cartesianpointplane .\n")
                .append("    ?plane bim:hasAxisDirection ?axisdirectionplane .\n")
                .append("    ?plane bim:hasRefDirection ?refdirectionplane .\n")
                .append("    ?polyspace bim:hasPolygonalBoundary ?polyline .\n")
                .append("    ?polyline rdf:type ifc:IfcPolyline .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?polyspace  rdf:type  ifc:IfcPolygonalBoundedHalfSpace .\n")
                .append("    ?polyspace ifc:agreementFlag_IfcHalfSpaceSolid/express:hasBoolean ?boolean .\n")
                .append("    ?polyspace ifc:position_IfcPolygonalBoundedHalfSpace/ifc:location_IfcPlacement ?spacecartesianpoint\n")
                .append("    OPTIONAL\n")
                .append("      { ?polyspace ifc:position_IfcPolygonalBoundedHalfSpace/ifc:axis_IfcAxis2Placement3D ?axisdirection}\n")
                .append("    OPTIONAL\n")
                .append("      { ?polyspace ifc:position_IfcPolygonalBoundedHalfSpace/ifc:refDirection_IfcAxis2Placement3D ?refdirection}\n")
                .append("    ?polyspace  ifc:baseSurface_IfcHalfSpaceSolid  ?plane .\n")
                .append("    ?plane    rdf:type              ifc:IfcPlane .\n")
                .append("    ?plane ifc:position_IfcElementarySurface/ifc:location_IfcPlacement ?cartesianpointplane\n")
                .append("    OPTIONAL\n")
                .append("      { ?plane ifc:position_IfcElementarySurface/ifc:axis_IfcAxis2Placement3D ?axisdirectionplane}\n")
                .append("    OPTIONAL\n")
                .append("      { ?plane ifc:position_IfcElementarySurface/ifc:refDirection_IfcAxis2Placement3D ?refdirectionplane}\n")
                .append("    ?polyspace  ifc:polygonalBoundary_IfcPolygonalBoundedHalfSpace  ?polyline .\n")
                .append("    ?polyline  rdf:type             ifc:IfcPolyline\n")
                .append("    VALUES ?polyspace { bim:" + polyHalfSpaceInst + " }\n")
                .append("  }");
        return expected.toString();
    }

    private String genExpectedResultsForFacetedBrep() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?brep rdf:type bim:FacetedBrep .\n")
                .append("    ?brep bim:hasExteriorBoundary ?closedshell .\n")
                .append("    ?closedshell rdf:type bim:ClosedShell .\n")
                .append("    ?closedshell bim:hasConnectedFaces ?ifcface .\n")
                .append("    ?ifcface rdf:type bim:Face .\n")
                .append("    ?ifcface bim:hasBounds ?ifcfacebound .\n")
                .append("    ?ifcfacebound rdf:type bim:FaceOuterBound .\n")
                .append("    ?ifcfacebound bim:isLoopNonInversedOrientation ?boolean .\n")
                .append("    ?ifcfacebound bim:hasFaceBoundary ?polyloop .\n")
                .append("    ?polyloop rdf:type bim:PolyLoop .\n")
                .append("    ?polyloop bim:hasStartingVertex ?firstpointlist .\n")
                .append("    ?cartesianpointlists ?p ?o .\n")
                .append("    ?cartesianpointlists rdf:type bim:LineVertex .\n")
                .append("    ?cartesianpoint rdf:type bim:CartesianPoint .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?brep     rdf:type              ifc:IfcFacetedBrep ;\n")
                .append("              ifc:outer_IfcManifoldSolidBrep  ?closedshell .\n")
                .append("    ?closedshell  rdf:type          ifc:IfcClosedShell ;\n")
                .append("              ifc:cfsFaces_IfcConnectedFaceSet  ?ifcface .\n")
                .append("    ?ifcface  rdf:type              ifc:IfcFace ;\n")
                .append("              ifc:bounds_IfcFace    ?ifcfacebound .\n")
                .append("    ?ifcfacebound\n")
                .append("              rdf:type              ifc:IfcFaceOuterBound .\n")
                .append("    ?ifcfacebound ifc:orientation_IfcFaceBound/express:hasBoolean ?boolean .\n")
                .append("    ?ifcfacebound\n")
                .append("              ifc:bound_IfcFaceBound  ?polyloop .\n")
                .append("    ?polyloop  rdf:type             ifc:IfcPolyLoop ;\n")
                .append("              ifc:polygon_IfcPolyLoop  ?firstpointlist .\n")
                .append("    ?firstpointlist (list:hasNext)* ?cartesianpointlists .\n")
                .append("    ?cartesianpointlists\n")
                .append("              list:hasContents  ?cartesianpoint ;\n")
                .append("              ?p                ?o\n")
                .append("    VALUES ?brep { bim:" + facetedBrepInst + " }\n")
                .append("  }");
        return expected.toString();
    }

    private String genExpectedResultsForExtrudedAreaSolid() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?extrudedareasolid rdf:type bim:ExtrudedAreaSolid .\n")
                .append("    ?extrudedareasolid bim:hasRefPoint ?cartesianpoint .\n")
                .append("    ?extrudedareasolid bim:hasExtrusionDirection ?direction .\n")
                .append("    ?extrudedareasolid bim:hasExtrusionDepth ?depthvalue .\n")
                .append("    ?extrudedareasolid bim:hasExtrusionProfile ?rectangleprofile .\n")
                .append("    ?rectangleprofile rdf:type bim:RectangleProfileDefinition .\n")
                .append("    ?rectangleprofile bim:hasProfileType ?profiletype .\n")
                .append("    ?rectangleprofile bim:hasRefPoint ?profilecartesianpoint .\n")
                .append("    ?rectangleprofile bim:hasRefDirection ?profiledirectionvector .\n")
                .append("    ?rectangleprofile bim:hasXDimensionExtent ?xdimvalue .\n")
                .append("    ?rectangleprofile bim:hasYDimensionExtent ?ydimvalue .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?extrudedareasolid\n")
                .append("              rdf:type  ifc:IfcExtrudedAreaSolid .\n")
                .append("    ?extrudedareasolid ifc:position_IfcSweptAreaSolid/ifc:location_IfcPlacement ?cartesianpoint .\n")
                .append("    ?extrudedareasolid\n")
                .append("              ifc:extrudedDirection_IfcExtrudedAreaSolid  ?direction .\n")
                .append("    ?extrudedareasolid ifc:depth_IfcExtrudedAreaSolid/express:hasDouble ?depthvalue .\n")
                .append("    ?extrudedareasolid\n")
                .append("              ifc:sweptArea_IfcSweptAreaSolid  ?rectangleprofile .\n")
                .append("    ?rectangleprofile\n")
                .append("              rdf:type              ifc:IfcRectangleProfileDef ;\n")
                .append("              ifc:profileType_IfcProfileDef  ?profiletype .\n")
                .append("    ?rectangleprofile ifc:position_IfcParameterizedProfileDef/ifc:location_IfcPlacement ?profilecartesianpoint .\n")
                .append("    ?rectangleprofile ifc:position_IfcParameterizedProfileDef/ifc:refDirection_IfcAxis2Placement2D ?profiledirectionvector .\n")
                .append("    ?rectangleprofile ifc:xDim_IfcRectangleProfileDef/express:hasDouble ?xdimvalue .\n")
                .append("    ?rectangleprofile ifc:yDim_IfcRectangleProfileDef/express:hasDouble ?ydimvalue\n")
                .append("    VALUES ?extrudedareasolid { bim:" + extrudedAreaSolidInst + " }\n")
                .append("  }");
        return expected.toString();
    }

    private String genExpectedResultsForPolyline() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?polyline rdf:type bim:Polyline .\n")
                .append("    ?polyline bim:hasStartingVertex ?firstpointlist .\n")
                .append("    ?cartesianpointlists ?p ?o .\n")
                .append("    ?cartesianpointlists rdf:type bim:LineVertex .\n")
                .append("    ?cartesianpoint rdf:type bim:CartesianPoint .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?polyline  rdf:type             ifc:IfcPolyline ;\n")
                .append("              ifc:points_IfcPolyline  ?firstpointlist .\n")
                .append("    ?firstpointlist (list:hasNext)* ?cartesianpointlists .\n")
                .append("    ?cartesianpointlists\n")
                .append("              list:hasContents  ?cartesianpoint ;\n")
                .append("              ?p                ?o\n")
                .append("    VALUES ?polyline { bim:" + polylineInst + " }\n")
                .append("  }");
        return expected.toString();
    }

    private String genExpectedResultsForGeomRepSubContext() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?subcontext rdf:type bim:GeometricRepresentationSubContext .\n")
                .append("    ?subcontext bim:hasParentContext ?subContextParent .\n")
                .append("    ?subcontext bim:hasContextIdentifier ?subContextIdentifier .\n")
                .append("    ?subcontext bim:hasContextType ?subContextViewType .\n")
                .append("    ?subcontext bim:hasTargetView ?subContextTargetView .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?subcontext  rdf:type           ifc:IfcGeometricRepresentationSubContext ;\n")
                .append("              ifc:parentContext_IfcGeometricRepresentationSubContext  ?subContextParent .\n")
                .append("    ?subcontext ifc:contextIdentifier_IfcRepresentationContext/express:hasString ?subContextIdentifier .\n")
                .append("    ?subcontext ifc:contextType_IfcRepresentationContext/express:hasString ?subContextViewType .\n")
                .append("    ?subcontext  ifc:targetView_IfcGeometricRepresentationSubContext  ?subContextTargetView\n")
                .append("    VALUES ?subcontext { bim:" + geomRepSubContextInst + " }\n")
                .append("  }");
        return expected.toString();
    }

    private String genExpectedResultsForLocalPlacement() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?localplacement rdf:type bim:LocalPlacement .\n")
                .append("    ?localplacement bim:hasRelativePosition ?parentplacement .\n")
                .append("    ?localplacement bim:hasPosition ?coordinates .\n")
                .append("    ?coordinates rdf:type bim:CartesianPoint .\n")
                .append("    ?localplacement bim:hasRefDirection ?refdirection .\n")
                .append("    ?refdirection rdf:type bim:DirectionVector .\n")
                .append("    ?localplacement bim:hasAxisDirection ?axisdirection .\n")
                .append("    ?axisdirection rdf:type bim:DirectionVector .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?localplacement\n")
                .append("              rdf:type              ifc:IfcLocalPlacement ;\n")
                .append("              ifc:relativePlacement_IfcLocalPlacement  ?axisplacement .\n")
                .append("    ?axisplacement\n")
                .append("              ifc:location_IfcPlacement  ?coordinates .\n")
                .append("    ?coordinates  rdf:type          ifc:IfcCartesianPoint\n")
                .append("    OPTIONAL\n")
                .append("      { ?localplacement\n")
                .append("                  ifc:placementRelTo_IfcLocalPlacement  ?parentplacement}\n")
                .append("      { ?axisplacement\n")
                .append("                  ifc:axis_IfcAxis2Placement3D  ?axisdirection .\n")
                .append("        ?axisdirection\n")
                .append("                  rdf:type              ifc:IfcDirection .\n")
                .append("        ?axisplacement\n")
                .append("                  ifc:refDirection_IfcAxis2Placement3D  ?refdirection .\n")
                .append("        ?refdirection\n")
                .append("                  rdf:type              ifc:IfcDirection}\n")
                .append("    UNION\n")
                .append("      { ?axisplacement\n")
                .append("                  ifc:refDirection_IfcAxis2Placement2D  ?refdirection .\n")
                .append("        ?refdirection\n")
                .append("                  rdf:type              ifc:IfcDirection}\n")
                .append("    VALUES ?localplacement { bim:" + localPlacementInst + " }\n")
                .append("  }");
        return expected.toString();
    }

    private String genExpectedResultsForCartTransformer() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?operator rdf:type bim:CartesianTransformationOperator .\n")
                .append("    ?operator bim:hasLocalOrigin ?cartesianpoint .\n")
                .append("    ?cartesianpoint rdf:type bim:CartesianPoint .\n")
                .append("    ?operator bim:hasScale ?scalefactor .\n")
                .append("    ?operator bim:hasDerivedXAxis ?xdirection .\n")
                .append("    ?operator bim:hasDerivedYAxis ?ydirection .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?operator  rdf:type             ifc:IfcCartesianTransformationOperator3D ;\n")
                .append("              ifc:localOrigin_IfcCartesianTransformationOperator  ?cartesianpoint .\n")
                .append("    ?cartesianpoint\n")
                .append("              rdf:type              ifc:IfcCartesianPoint\n")
                .append("    OPTIONAL\n")
                .append("      { ?operator ifc:scale_IfcCartesianTransformationOperator/express:hasDouble ?scalefactor}\n")
                .append("    OPTIONAL\n")
                .append("      { ?operator  ifc:axis1_IfcCartesianTransformationOperator  ?xdirection}\n")
                .append("    OPTIONAL\n")
                .append("      { ?operator  ifc:axis2_IfcCartesianTransformationOperator  ?ydirection}\n")
                .append("    VALUES ?operator { bim:" + cartTransformerInst + " }\n")
                .append("  }");
        return expected.toString();
    }

    private String genExpectedResultsForDirectionVector() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?direction rdf:type bim:DirectionVector .\n")
                .append("    ?direction bim:hasXDirectionRatio ?xvalue .\n")
                .append("    ?direction bim:hasYDirectionRatio ?yvalue .\n")
                .append("    ?direction bim:hasZDirectionRatio ?zvalue .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?direction  rdf:type            ifc:IfcDirection ;\n")
                .append("              ifc:directionRatios_IfcDirection  ?xlist .\n")
                .append("    ?xlist list:hasContents/express:hasDouble ?xvalue .\n")
                .append("    ?xlist  list:hasNext  ?ylist .\n")
                .append("    ?ylist list:hasContents/express:hasDouble ?yvalue\n")
                .append("    OPTIONAL\n")
                .append("      { ?ylist (list:hasNext/list:hasContents)/express:hasDouble ?zvalue}\n")
                .append("    VALUES ?direction { bim:" + directionVectorInst + " }\n")
                .append("  }");
        return expected.toString();
    }

    private String genExpectedResultsForCartPoint() {
        StringBuilder expected = new StringBuilder();
        expected.append("CONSTRUCT \n")
                .append("  { \n")
                .append("    ?cartesianpoint rdf:type bim:CartesianPoint .\n")
                .append("    ?cartesianpoint bim:hasXCoordinate ?xvalue .\n")
                .append("    ?cartesianpoint bim:hasYCoordinate ?yvalue .\n")
                .append("    ?cartesianpoint bim:hasZCoordinate ?zvalue .\n")
                .append("  }\n")
                .append("WHERE\n")
                .append("  { ?cartesianpoint\n")
                .append("              rdf:type              ifc:IfcCartesianPoint ;\n")
                .append("              ifc:coordinates_IfcCartesianPoint  ?xlist .\n")
                .append("    ?xlist list:hasContents/express:hasDouble ?xvalue .\n")
                .append("    ?xlist  list:hasNext  ?ylist .\n")
                .append("    ?ylist list:hasContents/express:hasDouble ?yvalue\n")
                .append("    OPTIONAL\n")
                .append("      { ?ylist (list:hasNext/list:hasContents)/express:hasDouble ?zvalue}\n")
                .append("    VALUES ?cartesianpoint { bim:" + cartPointInst + " }\n");
        return expected.toString();
    }
}