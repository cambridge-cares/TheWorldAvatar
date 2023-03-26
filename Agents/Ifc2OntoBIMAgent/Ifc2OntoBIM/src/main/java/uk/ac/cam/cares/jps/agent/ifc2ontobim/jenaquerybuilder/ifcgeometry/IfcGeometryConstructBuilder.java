package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcgeometry;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.QueryHandler;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Provides query statements relevant to all geometries like FacetedBrep, BooleanClippingResult, ExtrudedSolidArea, and more.
 *
 * @author qhouyee
 */
class IfcGeometryConstructBuilder {
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    private static final String BREP_VAR = "?brep";
    private static final String CLOSEDSHELL_VAR = "?closedshell";
    private static final String FACE_VAR = "?ifcface";
    private static final String FACEBOUND_VAR = "?ifcfacebound";
    private static final String POLYLOOP_VAR = "?polyloop";
    private static final String POLYLINE_VAR = "?polyline";
    private static final String BOOLEAN_VAR = "?boolean";
    private static final String CARTPOINT_LIST_VAR = "?cartesianpointlists";
    private static final String FIRSTPOINT_LIST_VAR = "?firstpointlist";
    private static final String EXTRUDED_AREA_SOLID_VAR = "?extrudedareasolid";
    private static final String DEPTHVALUE_VAR = "?depthvalue";
    private static final String RECT_PROFILE_VAR = "?rectangleprofile";
    private static final String RECT_PROFILE_POINT_VAR = "?profilecartesianpoint";
    private static final String RECT_PROFILE_DIRECTION_VAR = "?profiledirectionvector";
    private static final String RECT_PROFILE_XDIM_VAR = "?xdimvalue";
    private static final String RECT_PROFILE_YDIM_VAR = "?ydimvalue";
    private static final String RECT_PROFILE_TYPE_VAR = "?profiletype";
    private static final String POLYSPACE_VAR = "?polyspace";
    private static final String SPACE_CARTPOINT_VAR = "?spacecartesianpoint";
    private static final String PLANE_VAR = "?plane";
    private static final String PLANE_CARTPOINT_VAR = "?cartesianpointplane";
    private static final String PLANE_DIRECTION_AXIS_VAR = "?axisdirectionplane";
    private static final String PLANE_DIRECTION_REF_VAR = "?refdirectionplane";
    private static final String CLIPPING_RESULT_VAR = "?clippingres";
    private static final String OPERAND1_VAR = "?operand1";
    private static final String OPERAND1_CLASS_VAR = "?operand1class";
    private static final String OPERAND2_VAR = "?operand2";
    private static final String OPERAND2_CLASS_VAR = "?operand2class";
    private static final String BOOLEAN_OPERATOR_VAR = "?booleanoperator";

    /**
     * Add the query statements for all faceted B-rep geometries and their properties.
     *
     * @param iriList List of IRI.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructFacetedBrepRepresentationTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        // Faceted B-rep geometry sub-graph
        builder.addConstruct(BREP_VAR, QueryHandler.RDF_TYPE, "bim:FacetedBrep")
                .addConstruct(BREP_VAR, "bim:hasExteriorBoundary", CLOSEDSHELL_VAR)
                .addConstruct(CLOSEDSHELL_VAR, QueryHandler.RDF_TYPE, "bim:ClosedShell")
                .addConstruct(CLOSEDSHELL_VAR, "bim:hasConnectedFaces", FACE_VAR)
                .addConstruct(FACE_VAR, QueryHandler.RDF_TYPE, "bim:Face")
                .addConstruct(FACE_VAR, "bim:hasBounds", FACEBOUND_VAR)
                .addConstruct(FACEBOUND_VAR, QueryHandler.RDF_TYPE, "bim:FaceOuterBound")
                .addConstruct(FACEBOUND_VAR, "bim:isLoopNonInversedOrientation", BOOLEAN_VAR)
                .addConstruct(FACEBOUND_VAR, "bim:hasFaceBoundary", POLYLOOP_VAR);
        // Query for b-rep geometry and their relationships
        Map<String, List<RDFNode>> varMap = new HashMap<>();// Add VALUES statement for all IRIs
        varMap.put(BREP_VAR, iriList);
        builder.addWhereValueVars(varMap)
                .addWhere(BREP_VAR, QueryHandler.RDF_TYPE, "ifc:IfcFacetedBrep")
                .addWhere(BREP_VAR, "ifc:outer_IfcManifoldSolidBrep", CLOSEDSHELL_VAR)
                .addWhere(CLOSEDSHELL_VAR, QueryHandler.RDF_TYPE, "ifc:IfcClosedShell")
                .addWhere(CLOSEDSHELL_VAR, "ifc:cfsFaces_IfcConnectedFaceSet", FACE_VAR)
                .addWhere(FACE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcFace")
                .addWhere(FACE_VAR, "ifc:bounds_IfcFace", FACEBOUND_VAR)
                .addWhere(FACEBOUND_VAR, QueryHandler.RDF_TYPE, "ifc:IfcFaceOuterBound")
                .addWhere(FACEBOUND_VAR, "ifc:orientation_IfcFaceBound/express:hasBoolean", BOOLEAN_VAR) // Skip intermediate class
                .addWhere(FACEBOUND_VAR, "ifc:bound_IfcFaceBound ", POLYLOOP_VAR);

        // Construct a sub-graph linking the Poly loop to its cartesian points in sequence
        builder.addConstruct(POLYLOOP_VAR, QueryHandler.RDF_TYPE, "bim:PolyLoop")
                .addConstruct(POLYLOOP_VAR, "bim:hasStartingVertex", FIRSTPOINT_LIST_VAR)
                .addConstruct(CARTPOINT_LIST_VAR, "?p", "?o")
                .addConstruct(CARTPOINT_LIST_VAR, QueryHandler.RDF_TYPE, "bim:LineVertex")
                .addConstruct(GeomConstructBuilderMediator.CARTPOINT_VAR, QueryHandler.RDF_TYPE, "bim:CartesianPoint");
        // IfcPolyLoop is defined through a sequence of cartesian points of undefined length
        // Each point is stored in an individual list (this holds only one point). The list is linked to other lists through the "list:hasNext" property
        builder.addWhere(POLYLOOP_VAR, QueryHandler.RDF_TYPE, "ifc:IfcPolyLoop")
                .addWhere(POLYLOOP_VAR, "ifc:polygon_IfcPolyLoop ", FIRSTPOINT_LIST_VAR) // Retrieve the starting list
                .addWhere(FIRSTPOINT_LIST_VAR, "list:hasNext*", CARTPOINT_LIST_VAR) // Retrieve all lists (including first) associated with the polygon
                .addWhere(CARTPOINT_LIST_VAR, "list:hasContents", GeomConstructBuilderMediator.CARTPOINT_VAR) // Retrieve the cartesian point to change their rdf:type to bim
                .addWhere(CARTPOINT_LIST_VAR, "?p", "?o"); // Retrieve all the related triples connect to a list
    }

    /**
     * Add the query statements for all extruded area solid geometries and their properties.
     *
     * @param iriList List of IRI.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructExtrudedAreaSolidRepresentationTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        // Extruded Area Solid geometry sub-graph
        builder.addConstruct(EXTRUDED_AREA_SOLID_VAR, QueryHandler.RDF_TYPE, "bim:ExtrudedAreaSolid")
                .addConstruct(EXTRUDED_AREA_SOLID_VAR, "bim:hasRefPoint", GeomConstructBuilderMediator.CARTPOINT_VAR)
                .addConstruct(EXTRUDED_AREA_SOLID_VAR, "bim:hasExtrusionDirection", GeomConstructBuilderMediator.DIRECTION_VAR)
                .addConstruct(EXTRUDED_AREA_SOLID_VAR, "bim:hasExtrusionDepth", DEPTHVALUE_VAR);
        // Query for extruded area solid geometries and their relationships
        Map<String, List<RDFNode>> varMap = new HashMap<>();// Add VALUES statement for all IRIs
        varMap.put(EXTRUDED_AREA_SOLID_VAR, iriList);
        builder.addWhereValueVars(varMap)
                .addWhere(EXTRUDED_AREA_SOLID_VAR, QueryHandler.RDF_TYPE, "ifc:IfcExtrudedAreaSolid")
                .addWhere(EXTRUDED_AREA_SOLID_VAR, "ifc:position_IfcSweptAreaSolid/ifc:location_IfcPlacement", GeomConstructBuilderMediator.CARTPOINT_VAR)
                .addWhere(EXTRUDED_AREA_SOLID_VAR, "ifc:extrudedDirection_IfcExtrudedAreaSolid", GeomConstructBuilderMediator.DIRECTION_VAR)
                .addWhere(EXTRUDED_AREA_SOLID_VAR, "ifc:depth_IfcExtrudedAreaSolid/express:hasDouble", DEPTHVALUE_VAR);

        // Rectangle Profile Definition sub-graph
        builder.addConstruct(EXTRUDED_AREA_SOLID_VAR, "bim:hasExtrusionProfile", RECT_PROFILE_VAR)
                .addConstruct(RECT_PROFILE_VAR, QueryHandler.RDF_TYPE, "bim:RectangleProfileDefinition")
                .addConstruct(RECT_PROFILE_VAR, "bim:hasProfileType", RECT_PROFILE_TYPE_VAR)
                .addConstruct(RECT_PROFILE_VAR, "bim:hasRefPoint", RECT_PROFILE_POINT_VAR)
                .addConstruct(RECT_PROFILE_VAR, "bim:hasRefDirection", RECT_PROFILE_DIRECTION_VAR)
                .addConstruct(RECT_PROFILE_VAR, "bim:hasXDimensionExtent", RECT_PROFILE_XDIM_VAR)
                .addConstruct(RECT_PROFILE_VAR, "bim:hasYDimensionExtent", RECT_PROFILE_YDIM_VAR);
        // Query for rectangle profile and their relationships
        builder.addWhere(EXTRUDED_AREA_SOLID_VAR, "ifc:sweptArea_IfcSweptAreaSolid", RECT_PROFILE_VAR)
                .addWhere(RECT_PROFILE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcRectangleProfileDef")
                .addWhere(RECT_PROFILE_VAR, "ifc:profileType_IfcProfileDef", RECT_PROFILE_TYPE_VAR)
                .addWhere(RECT_PROFILE_VAR, "ifc:position_IfcParameterizedProfileDef/ifc:location_IfcPlacement", RECT_PROFILE_POINT_VAR)
                .addWhere(RECT_PROFILE_VAR, "ifc:position_IfcParameterizedProfileDef/ifc:refDirection_IfcAxis2Placement2D", RECT_PROFILE_DIRECTION_VAR)
                .addWhere(RECT_PROFILE_VAR, "ifc:xDim_IfcRectangleProfileDef/express:hasDouble", RECT_PROFILE_XDIM_VAR) // Skip intermediate class
                .addWhere(RECT_PROFILE_VAR, "ifc:yDim_IfcRectangleProfileDef/express:hasDouble", RECT_PROFILE_YDIM_VAR); // Skip intermediate class
    }

    /**
     * Add the query statements for all polygonal bounded half-space geometries and their properties.
     *
     * @param iriList List of IRI.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructPolygonalBoundedHalfSpaceRepresentationTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        // Sub-graph construction for polygonal bounded half-space
        builder.addConstruct(POLYSPACE_VAR, QueryHandler.RDF_TYPE, "bim:PolygonalBoundedHalfSpace")
                .addConstruct(POLYSPACE_VAR, "bim:hasAgreementFlag", BOOLEAN_VAR)
                .addConstruct(POLYSPACE_VAR, "bim:hasRefPoint", SPACE_CARTPOINT_VAR)
                .addConstruct(POLYSPACE_VAR, "bim:hasAxisDirection", GeomConstructBuilderMediator.DIRECTION_AXIS_VAR)
                .addConstruct(POLYSPACE_VAR, "bim:hasRefDirection", GeomConstructBuilderMediator.DIRECTION_REF_VAR);
        // Query for properties
        Map<String, List<RDFNode>> varMap = new HashMap<>();// Add VALUES statement for all IRIs
        varMap.put(POLYSPACE_VAR, iriList);
        builder.addWhereValueVars(varMap)
                .addWhere(POLYSPACE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcPolygonalBoundedHalfSpace")
                .addWhere(POLYSPACE_VAR, "ifc:agreementFlag_IfcHalfSpaceSolid/express:hasBoolean", BOOLEAN_VAR)
                .addWhere(POLYSPACE_VAR, "ifc:position_IfcPolygonalBoundedHalfSpace/ifc:location_IfcPlacement", SPACE_CARTPOINT_VAR)
                .addOptional(POLYSPACE_VAR, "ifc:position_IfcPolygonalBoundedHalfSpace/ifc:axis_IfcAxis2Placement3D", GeomConstructBuilderMediator.DIRECTION_AXIS_VAR)
                .addOptional(POLYSPACE_VAR, "ifc:position_IfcPolygonalBoundedHalfSpace/ifc:refDirection_IfcAxis2Placement3D", GeomConstructBuilderMediator.DIRECTION_REF_VAR);

        // Sub-graph construction for base surface plane
        builder.addConstruct(POLYSPACE_VAR, "bim:hasBaseSurface", PLANE_VAR)
                .addConstruct(PLANE_VAR, QueryHandler.RDF_TYPE, "bim:SurfacePlane")
                .addConstruct(PLANE_VAR, "bim:hasRefPoint", PLANE_CARTPOINT_VAR)
                .addConstruct(PLANE_VAR, "bim:hasAxisDirection", PLANE_DIRECTION_AXIS_VAR)
                .addConstruct(PLANE_VAR, "bim:hasRefDirection", PLANE_DIRECTION_REF_VAR);
        builder.addWhere(POLYSPACE_VAR, "ifc:baseSurface_IfcHalfSpaceSolid", PLANE_VAR)
                .addWhere(PLANE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcPlane")
                .addWhere(PLANE_VAR, "ifc:position_IfcElementarySurface/ifc:location_IfcPlacement", PLANE_CARTPOINT_VAR)
                .addOptional(PLANE_VAR, "ifc:position_IfcElementarySurface/ifc:axis_IfcAxis2Placement3D", PLANE_DIRECTION_AXIS_VAR)
                .addOptional(PLANE_VAR, "ifc:position_IfcElementarySurface/ifc:refDirection_IfcAxis2Placement3D", PLANE_DIRECTION_REF_VAR);

        // Sub-graph construction for polyline
        builder.addConstruct(POLYSPACE_VAR, "bim:hasPolygonalBoundary", POLYLINE_VAR)
                .addConstruct(POLYLINE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcPolyline");
        builder.addWhere(POLYSPACE_VAR, "ifc:polygonalBoundary_IfcPolygonalBoundedHalfSpace", POLYLINE_VAR)
                .addWhere(POLYLINE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcPolyline");
    }

    /**
     * Add the query statements for all polyline geometries and their properties.
     *
     * @param iriList List of IRI.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructPolylineRepresentationTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        // Sub-graph construction for polyline
        builder.addConstruct(POLYLINE_VAR, QueryHandler.RDF_TYPE, "bim:Polyline")
                .addConstruct(POLYLINE_VAR, "bim:hasStartingVertex", FIRSTPOINT_LIST_VAR)
                .addConstruct(CARTPOINT_LIST_VAR, "?p", "?o")
                .addConstruct(CARTPOINT_LIST_VAR, QueryHandler.RDF_TYPE, "bim:LineVertex")
                .addConstruct(GeomConstructBuilderMediator.CARTPOINT_VAR, QueryHandler.RDF_TYPE, "bim:CartesianPoint");
        // Query for polyline that holds a sequence of cartesian points of undefined length
        Map<String, List<RDFNode>> varMap = new HashMap<>();// Add VALUES statement for all IRIs
        varMap.put(POLYLINE_VAR, iriList);
        builder.addWhereValueVars(varMap)
                .addWhere(POLYLINE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcPolyline")
                .addWhere(POLYLINE_VAR, "ifc:points_IfcPolyline ", FIRSTPOINT_LIST_VAR)  // Retrieve the starting list
                .addWhere(FIRSTPOINT_LIST_VAR, "list:hasNext*", CARTPOINT_LIST_VAR) // Retrieve all lists (including first) associated with the polygon
                .addWhere(CARTPOINT_LIST_VAR, "list:hasContents", GeomConstructBuilderMediator.CARTPOINT_VAR) // Retrieve the cartesian point to change their rdf:type to bim
                .addWhere(CARTPOINT_LIST_VAR, "?p", "?o"); // Retrieve all the related triples connect to a list
    }

    /**
     * Add the query statements for all Boolean clipping results and their properties.
     *
     * @param iriList List of IRI.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructBooleanClippingResultRepresentationTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        // Base query
        builder.addConstruct(CLIPPING_RESULT_VAR, QueryHandler.RDF_TYPE, "bim:BooleanClippingResult")
                .addConstruct(CLIPPING_RESULT_VAR, "bim:hasBooleanOperator", BOOLEAN_OPERATOR_VAR)
                .addConstruct(CLIPPING_RESULT_VAR, "bim:hasFirstOperand", OPERAND1_VAR)
                .addConstruct(OPERAND1_VAR, QueryHandler.RDF_TYPE, OPERAND1_CLASS_VAR)
                .addConstruct(CLIPPING_RESULT_VAR, "bim:hasSecondOperand", OPERAND2_VAR)
                .addConstruct(OPERAND2_VAR, QueryHandler.RDF_TYPE, OPERAND2_CLASS_VAR);
        // Add VALUES statement for all IRIs
        Map<String, List<RDFNode>> varMap = new HashMap<>();
        varMap.put(CLIPPING_RESULT_VAR, iriList);
        builder.addWhereValueVars(varMap)
                .addWhere(CLIPPING_RESULT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcBooleanClippingResult")
                .addWhere(CLIPPING_RESULT_VAR, "ifc:operator_IfcBooleanResult", BOOLEAN_OPERATOR_VAR); // May be DIFFERENCE, UNION or INTERSECTION

        try {
            addUnionTriplesForRecursiveBooleanClipping(builder, OPERAND1_VAR);
            addUnionTriplesForRecursiveBooleanClipping(builder, OPERAND2_VAR);
        } catch (ParseException e) {
            LOGGER.fatal("Union triples cannot be added. See message for more details: " + e);
            throw new JPSRuntimeException("Union triples cannot be added. See message for more details: " + e);
        }
    }

    /**
     * A utility method to add a union query of triples for the first and second operand of the BooleanClippingResult class.
     * statement. This method ensures that the union query comprises two subgroups, and is not linked to previous statements.
     * This query will only run if the operand of the initial Boolean Clipping Result has operands belonging to the same class.
     *
     * @param builder Construct Builder object to add the Optional statements.
     * @param operand A SPARQL string containing the operand variable. Default format: OPERAND1_VAR or "operand2".
     */
    private static void addUnionTriplesForRecursiveBooleanClipping(ConstructBuilder builder, String operand) throws ParseException {
        // Create variable resources based on inputs to prevent overlaps
        String operandClass = operand + "class";
        String nestedOperator = operand + "operator";
        String nestedOperand1 = operand + "1";
        String nestedOperand1Class = nestedOperand1 + "class";
        String nestedOperand2 = operand + "2";
        String nestedOperand2Class = nestedOperand2 + "class";

        // Add construct statements to builder
        builder.addConstruct(operand, "bim:hasBooleanOperator", nestedOperator)
                .addConstruct(operand, "bim:hasFirstOperand", nestedOperand1)
                .addConstruct(nestedOperand1, QueryHandler.RDF_TYPE, nestedOperand1Class)
                .addConstruct(operand, "bim:hasSecondOperand", nestedOperand2)
                .addConstruct(nestedOperand2, QueryHandler.RDF_TYPE, nestedOperand2Class);
        // Create the union builder comprising of a subgroup UNION with a subquery
        SelectBuilder unionBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(unionBuilder);
        if (operand.equals(OPERAND1_VAR)) {
            unionBuilder.addWhere(CLIPPING_RESULT_VAR, "ifc:firstOperand_IfcBooleanResult", operand);
        } else if (operand.equals(OPERAND2_VAR)) {
            unionBuilder.addWhere(CLIPPING_RESULT_VAR, "ifc:secondOperand_IfcBooleanResult", operand);
        }
        SelectBuilder subqueryBuilder = unionBuilder.clone(); // Both sub-queries should have his

        // Add first subgroup
        unionBuilder.addWhere(operand, QueryHandler.RDF_TYPE, operandClass)
                .addFilter("NOT EXISTS {" + operand + " rdf:type ifc:IfcBooleanClippingResult}");

        // Add sub-query in the event the operand is a nested ifcBooleanClippingResult
        subqueryBuilder.addWhere(operand, QueryHandler.RDF_TYPE, "ifc:IfcBooleanClippingResult")
                .addBind("bim:BooleanClippingResult", operandClass)
                .addWhere(operand, "ifc:operator_IfcBooleanResult", nestedOperator)
                // Assumption is nested operands will not be IfcBooleanClippingResult
                .addWhere(operand, "ifc:firstOperand_IfcBooleanResult", nestedOperand1)
                .addWhere(nestedOperand1, QueryHandler.RDF_TYPE, nestedOperand1Class)
                .addWhere(operand, "ifc:secondOperand_IfcBooleanResult", nestedOperand2)
                .addWhere(nestedOperand2, QueryHandler.RDF_TYPE, nestedOperand2Class);
        // Add the sub query builder back to the main builder as a multi-line Optional statement.
        unionBuilder.addUnion(subqueryBuilder);
        builder.addWhere(unionBuilder);
    }
}