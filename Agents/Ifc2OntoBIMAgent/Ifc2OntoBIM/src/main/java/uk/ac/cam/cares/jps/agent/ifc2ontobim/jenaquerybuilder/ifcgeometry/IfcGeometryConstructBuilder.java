package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcgeometry;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;
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
    /**
     * Add the query statements for all faceted B-rep geometries and their properties.
     *
     * @param iriList List of IRI.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructFacetedBrepRepresentationTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        // Faceted B-rep geometry sub-graph
        builder.addConstruct("?brep", "rdf:type", "bim:FacetedBrep")
                .addConstruct("?brep", "bim:hasExteriorBoundary", "?closedshell")
                .addConstruct("?closedshell", "rdf:type", "bim:ClosedShell")
                .addConstruct("?closedshell", "bim:hasConnectedFaces", "?ifcface")
                .addConstruct("?ifcface", "rdf:type", "bim:Face")
                .addConstruct("?ifcface", "bim:hasBounds", "?ifcfacebound")
                .addConstruct("?ifcfacebound", "rdf:type", "bim:FaceOuterBound")
                .addConstruct("?ifcfacebound", "bim:isLoopNonInversedOrientation", "?boolean")
                .addConstruct("?ifcfacebound", "bim:hasFaceBoundary", "?polyloop");
        // Query for b-rep geometry and their relationships
        Map<String, List<RDFNode>> varMap = new HashMap<>();// Add VALUES statement for all IRIs
        varMap.put("?brep", iriList);
        builder.addWhereValueVars(varMap)
                .addWhere("?brep", "rdf:type", "ifc:IfcFacetedBrep")
                .addWhere("?brep", "ifc:outer_IfcManifoldSolidBrep", "?closedshell")
                .addWhere("?closedshell", "rdf:type", "ifc:IfcClosedShell")
                .addWhere("?closedshell", "ifc:cfsFaces_IfcConnectedFaceSet", "?ifcface")
                .addWhere("?ifcface", "rdf:type", "ifc:IfcFace")
                .addWhere("?ifcface", "ifc:bounds_IfcFace", "?ifcfacebound")
                .addWhere("?ifcfacebound", "rdf:type", "ifc:IfcFaceOuterBound")
                .addWhere("?ifcfacebound", "ifc:orientation_IfcFaceBound/express:hasBoolean", "?boolean") // Skip intermediate class
                .addWhere("?ifcfacebound", "ifc:bound_IfcFaceBound ", "?polyloop");

        // Construct a sub-graph linking the Poly loop to its cartesian points in sequence
        builder.addConstruct("?polyloop", "rdf:type", "bim:PolyLoop")
                .addConstruct("?polyloop", "bim:hasStartingVertex", "?firstpointlist")
                .addConstruct("?cartesianpointlists", "?p", "?o")
                .addConstruct("?cartesianpointlists", "rdf:type", "bim:LineVertex")
                .addConstruct("?cartesianpoint", "rdf:type", "bim:CartesianPoint");
        // IfcPolyLoop is defined through a sequence of cartesian points of undefined length
        // Each point is stored in an individual list (this holds only one point). The list is linked to other lists through the "list:hasNext" property
        builder.addWhere("?polyloop", "rdf:type", "ifc:IfcPolyLoop")
                .addWhere("?polyloop", "ifc:polygon_IfcPolyLoop ", "?firstpointlist") // Retrieve the starting list
                .addWhere("?firstpointlist", "list:hasNext*", "?cartesianpointlists") // Retrieve all lists (including first) associated with the polygon
                .addWhere("?cartesianpointlists", "list:hasContents", "?cartesianpoint") // Retrieve the cartesian point to change their rdf:type to bim
                .addWhere("?cartesianpointlists", "?p", "?o"); // Retrieve all the related triples connect to a list
    }

    /**
     * Add the query statements for all extruded area solid geometries and their properties.
     *
     * @param iriList List of IRI.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructExtrudedAreaSolidRepresentationTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        // Extruded Area Solid geometry sub-graph
        builder.addConstruct("?extrudedareasolid", "rdf:type", "bim:ExtrudedAreaSolid")
                .addConstruct("?extrudedareasolid", "bim:hasPosition", "?cartesianpoint")
                .addConstruct("?extrudedareasolid", "bim:hasExtrusionDirection", "?directionvector")
                .addConstruct("?extrudedareasolid", "bim:hasExtrusionDepth", "?depthvalue");
        // Query for extruded area solid geometries and their relationships
        Map<String, List<RDFNode>> varMap = new HashMap<>();// Add VALUES statement for all IRIs
        varMap.put("?extrudedareasolid", iriList);
        builder.addWhereValueVars(varMap)
                .addWhere("?extrudedareasolid", "rdf:type", "ifc:IfcExtrudedAreaSolid")
                .addWhere("?extrudedareasolid", "ifc:position_IfcSweptAreaSolid/ifc:location_IfcPlacement", "?cartesianpoint")
                .addWhere("?extrudedareasolid", "ifc:extrudedDirection_IfcExtrudedAreaSolid", "?directionvector")
                .addWhere("?extrudedareasolid", "ifc:depth_IfcExtrudedAreaSolid/express:hasDouble", "?depthvalue");

        // Rectangle Profile Definition sub-graph
        builder.addConstruct("?extrudedareasolid", "bim:hasSweptArea", "?rectangleprofile")
                .addConstruct("?rectangleprofile", "rdf:type", "bim:RectangleProfileDefinition")
                .addConstruct("?rectangleprofile", "bim:hasProfileType", "?profiletype")
                .addConstruct("?rectangleprofile", "bim:hasPosition", "?profilecartesianpoint")
                .addConstruct("?rectangleprofile", "bim:hasRefDirectionForPosition", "?profiledirectionvector")
                .addConstruct("?rectangleprofile", "bim:hasXDimensionExtent", "?xdimvalue")
                .addConstruct("?rectangleprofile", "bim:hasYDimensionExtent", "?ydimvalue");
        // Query for rectangle profile and their relationships
        builder.addWhere("?extrudedareasolid", "ifc:sweptArea_IfcSweptAreaSolid", "?rectangleprofile")
                .addWhere("?rectangleprofile", "rdf:type", "ifc:IfcRectangleProfileDef")
                .addWhere("?rectangleprofile", "ifc:profileType_IfcProfileDef", "?profiletype")
                .addWhere("?rectangleprofile", "ifc:position_IfcParameterizedProfileDef/ifc:location_IfcPlacement", "?profilecartesianpoint")
                .addWhere("?rectangleprofile", "ifc:position_IfcParameterizedProfileDef/ifc:refDirection_IfcAxis2Placement2D", "?profiledirectionvector")
                .addWhere("?rectangleprofile", "ifc:xDim_IfcRectangleProfileDef/express:hasDouble", "?xdimvalue") // Skip intermediate class
                .addWhere("?rectangleprofile", "ifc:yDim_IfcRectangleProfileDef/express:hasDouble", "?ydimvalue"); // Skip intermediate class
    }

    /**
     * Add the query statements for all polygonal bounded half-space geometries and their properties.
     *
     * @param iriList List of IRI.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructPolygonalBoundedHalfSpaceRepresentationTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        // Sub-graph construction for polygonal bounded half-space
        builder.addConstruct("?polyspace", "rdf:type", "bim:PolygonalBoundedHalfSpace")
                .addConstruct("?polyspace", "bim:hasAgreementFlag", "?boolean")
                .addConstruct("?polyspace", "bim:hasPosition", "?spacecartesianpoint")
                .addConstruct("?polyspace", "bim:hasPositionAxis", "?axisdirection")
                .addConstruct("?polyspace", "bim:hasRefDirectionForPosition", "?refdirection");
        // Query for properties
        Map<String, List<RDFNode>> varMap = new HashMap<>();// Add VALUES statement for all IRIs
        varMap.put("?polyspace", iriList);
        builder.addWhereValueVars(varMap)
                .addWhere("?polyspace", "rdf:type", "ifc:IfcPolygonalBoundedHalfSpace")
                .addWhere("?polyspace", "ifc:agreementFlag_IfcHalfSpaceSolid/express:hasBoolean", "?boolean")
                .addWhere("?polyspace", "ifc:position_IfcPolygonalBoundedHalfSpace/ifc:location_IfcPlacement", "?spacecartesianpoint")
                .addOptional("?polyspace", "ifc:position_IfcPolygonalBoundedHalfSpace/ifc:axis_IfcAxis2Placement3D", "?axisdirection")
                .addOptional("?polyspace", "ifc:position_IfcPolygonalBoundedHalfSpace/ifc:refDirection_IfcAxis2Placement3D", "?refdirection");

        // Sub-graph construction for base surface plane
        builder.addConstruct("?polyspace", "bim:hasBaseSurface", "?plane")
                .addConstruct("?plane", "rdf:type", "bim:SurfacePlane")
                .addConstruct("?plane", "bim:hasPosition", "?cartesianpointplane")
                .addConstruct("?plane", "bim:hasPositionAxis", "?axisdirectionplane")
                .addConstruct("?plane", "bim:hasRefDirectionForPosition", "?refdirectionplane");
        builder.addWhere("?polyspace", "ifc:baseSurface_IfcHalfSpaceSolid", "?plane")
                .addWhere("?plane", "rdf:type", "ifc:IfcPlane")
                .addWhere("?plane", "ifc:position_IfcElementarySurface/ifc:location_IfcPlacement", "?cartesianpointplane")
                .addOptional("?plane", "ifc:position_IfcElementarySurface/ifc:axis_IfcAxis2Placement3D", "?axisdirectionplane")
                .addOptional("?plane", "ifc:position_IfcElementarySurface/ifc:refDirection_IfcAxis2Placement3D", "?refdirectionplane");

        // Sub-graph construction for polyline
        builder.addConstruct("?polyspace", "bim:hasPolygonalBoundary", "?polyline")
                .addConstruct("?polyline", "rdf:type", "ifc:IfcPolyline");
        builder.addWhere("?polyspace", "ifc:polygonalBoundary_IfcPolygonalBoundedHalfSpace", "?polyline")
                .addWhere("?polyline", "rdf:type", "ifc:IfcPolyline");
    }

    /**
     * Add the query statements for all polyline geometries and their properties.
     *
     * @param iriList List of IRI.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructPolylineRepresentationTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        // Sub-graph construction for polyline
        builder.addConstruct("?polyline", "rdf:type", "bim:Polyline")
                .addConstruct("?polyline", "bim:hasStartingVertex", "?firstpointlist")
                .addConstruct("?cartesianpointlists", "?p", "?o")
                .addConstruct("?cartesianpointlists", "rdf:type", "bim:LineVertex")
                .addConstruct("?cartesianpoint", "rdf:type", "bim:CartesianPoint");
        // Query for polyline that holds a sequence of cartesian points of undefined length
        Map<String, List<RDFNode>> varMap = new HashMap<>();// Add VALUES statement for all IRIs
        varMap.put("?polyline", iriList);
        builder.addWhereValueVars(varMap)
                .addWhere("?polyline", "rdf:type", "ifc:IfcPolyline")
                .addWhere("?polyline", "ifc:points_IfcPolyline ", "?firstpointlist")  // Retrieve the starting list
                .addWhere("?firstpointlist", "list:hasNext*", "?cartesianpointlists") // Retrieve all lists (including first) associated with the polygon
                .addWhere("?cartesianpointlists", "list:hasContents", "?cartesianpoint") // Retrieve the cartesian point to change their rdf:type to bim
                .addWhere("?cartesianpointlists", "?p", "?o"); // Retrieve all the related triples connect to a list
    }

    /**
     * Add the query statements for all Boolean clipping results and their properties.
     *
     * @param iriList List of IRI.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructBooleanClippingResultRepresentationTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        // Base query
        builder.addConstruct("?clippingres", "rdf:type", "bim:BooleanClippingResult")
                .addConstruct("?clippingres", "bim:hasBooleanOperator", "?booleanoperator")
                .addConstruct("?clippingres", "bim:hasFirstOperand", "?operand1")
                .addConstruct("?operand1", "rdf:type", "?operand1class")
                .addConstruct("?clippingres", "bim:hasSecondOperand", "?operand2")
                .addConstruct("?operand2", "rdf:type", "?operand2class");
        // Add VALUES statement for all IRIs
        Map<String, List<RDFNode>> varMap = new HashMap<>();
        varMap.put("?clippingres", iriList);
        builder.addWhereValueVars(varMap)
                .addWhere("?clippingres", "rdf:type", "ifc:IfcBooleanClippingResult")
                .addWhere("?clippingres", "ifc:operator_IfcBooleanResult", "?booleanoperator"); // May be DIFFERENCE, UNION or INTERSECTION

        try {
            addUnionTriplesForRecursiveBooleanClipping(builder, "?operand1");
            addUnionTriplesForRecursiveBooleanClipping(builder, "?operand2");
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
     * @param operand A SPARQL string containing the operand variable. Default format: "?operand1" or "operand2".
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
                .addConstruct(nestedOperand1, "rdf:type", nestedOperand1Class)
                .addConstruct(operand, "bim:hasSecondOperand", nestedOperand2)
                .addConstruct(nestedOperand2, "rdf:type", nestedOperand2Class);
        // Create the union builder comprising of a subgroup UNION with a subquery
        SelectBuilder unionBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(unionBuilder);
        if (operand.equals("?operand1")){
            unionBuilder.addWhere("?clippingres", "ifc:firstOperand_IfcBooleanResult", operand);
        } else if (operand.equals("?operand2")){
            unionBuilder.addWhere("?clippingres", "ifc:secondOperand_IfcBooleanResult", operand);
        }
        SelectBuilder subqueryBuilder = unionBuilder.clone(); // Both sub-queries should have his

        // Add first subgroup
        unionBuilder.addWhere(operand, "rdf:type", operandClass)
                        .addFilter("NOT EXISTS {" +operand + " rdf:type ifc:IfcBooleanClippingResult}");

        // Add sub-query in the event the operand is a nested ifcBooleanClippingResult
         subqueryBuilder.addWhere(operand, "rdf:type", "ifc:IfcBooleanClippingResult")
                .addBind("bim:BooleanClippingResult",operandClass)
                .addWhere(operand, "ifc:operator_IfcBooleanResult", nestedOperator)
                // Assumption is nested operands will not be IfcBooleanClippingResult
                .addWhere(operand, "ifc:firstOperand_IfcBooleanResult", nestedOperand1)
                .addWhere(nestedOperand1, "rdf:type", nestedOperand1Class)
                .addWhere(operand, "ifc:secondOperand_IfcBooleanResult", nestedOperand2)
                .addWhere(nestedOperand2, "rdf:type", nestedOperand2Class);
        // Add the sub query builder back to the main builder as a multi-line Optional statement.
        unionBuilder.addUnion(subqueryBuilder);
        builder.addWhere(unionBuilder);
    }
}