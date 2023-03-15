package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcgeometry;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.rdf.model.RDFNode;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Provides query statements relevant to all geospatial operators like GeometricRepresentationSubContext, DirectionVector, CartesianPoint, and more.
 *
 * @author qhouyee
 */
class IfcGeospatialOperatorConstructBuilder {
    public static final String PLACEMENT_VAR = "?localplacement";
    protected static final String DIRECTION_VAR = "?direction";
    protected static final String DIRECTION_AXIS_VAR = "?axisdirection";
    protected static final String DIRECTION_REF_VAR = "?refdirection";
    protected static final String CARTPOINT_VAR = "?cartesianpoint";
    private static final String PARENT_PLACEMENT_VAR = "?parentplacement";
    private static final String COORDS_VAR = "?coordinates";
    private static final String AXISPLACEMENT_VAR = "?axisplacement";
    private static final String CART_TRANSFORMER_OPERATOR_VAR = "?operator";
    private static final String SCALE_FACTOR_VAR = "?scalefactor";
    private static final String XDIRECTION_VAR = "?xdirection";
    private static final String YDIRECTION_VAR = "?ydirection";
    private static final String XVALUE_VAR = "?xvalue";
    private static final String XLIST_VAR = "?xlist";
    private static final String YVALUE_VAR = "?yvalue";
    private static final String YLIST_VAR = "?ylist";
    private static final String ZVALUE_VAR = "?zvalue";
    private static final String SUBCONTEXT_VAR = "?subcontext";

    /**
     * Add the query statements for all geometric representation sub-context and their properties.
     *
     * @param iriList List of IRI to geometric representation sub-context.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructGeometricRepresentationSubContextTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        builder.addConstruct(SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, "bim:GeometricRepresentationSubContext")
                .addConstruct(SUBCONTEXT_VAR, "bim:hasParentContext", "?subContextParent")
                .addConstruct(SUBCONTEXT_VAR, "bim:hasContextIdentifier", "?subContextIdentifier")
                .addConstruct(SUBCONTEXT_VAR, "bim:hasContextType", "?subContextViewType")
                .addConstruct(SUBCONTEXT_VAR, "bim:hasTargetView", "?subContextTargetView");

        //IFC query structure
        builder.addWhere(SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcGeometricRepresentationSubContext")
                .addWhere(SUBCONTEXT_VAR, "ifc:parentContext_IfcGeometricRepresentationSubContext", "?subContextParent")
                .addWhere(SUBCONTEXT_VAR, "ifc:contextIdentifier_IfcRepresentationContext/express:hasString", "?subContextIdentifier")
                .addWhere(SUBCONTEXT_VAR, "ifc:contextType_IfcRepresentationContext/express:hasString", "?subContextViewType")
                .addWhere(SUBCONTEXT_VAR, "ifc:targetView_IfcGeometricRepresentationSubContext", "?subContextTargetView");
        // Add VALUES statement for all IRIs
        Map<String, List<RDFNode>> varMap = new HashMap<>();
        varMap.put(SUBCONTEXT_VAR, iriList);
        builder.addWhereValueVars(varMap);
    }

    /**
     * Add the query statements for all local placement and their properties.
     *
     * @param iriList List of IRI to geometric representation sub-context.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructLocalPlacementTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        ConstructBuilder subgroupBuilder = builder.clone();
        ConstructBuilder unionBuilder = builder.clone();
        builder.addConstruct(PLACEMENT_VAR, QueryHandler.RDF_TYPE, "bim:LocalPlacement")
                .addConstruct(PLACEMENT_VAR, "bim:hasRelativePosition", PARENT_PLACEMENT_VAR)
                .addConstruct(PLACEMENT_VAR, "bim:hasPosition", COORDS_VAR)
                .addConstruct(COORDS_VAR, QueryHandler.RDF_TYPE, "bim:CartesianPoint")
                .addConstruct(PLACEMENT_VAR, "bim:hasRefDirection", DIRECTION_REF_VAR)
                .addConstruct(DIRECTION_REF_VAR, QueryHandler.RDF_TYPE, "bim:DirectionVector")
                .addConstruct(PLACEMENT_VAR, "bim:hasAxisDirection", DIRECTION_AXIS_VAR)
                .addConstruct(DIRECTION_AXIS_VAR, QueryHandler.RDF_TYPE, "bim:DirectionVector");

        //IFC query structure
        builder.addWhere(PLACEMENT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcLocalPlacement")
                .addWhere(PLACEMENT_VAR, "ifc:relativePlacement_IfcLocalPlacement", AXISPLACEMENT_VAR)
                .addWhere(AXISPLACEMENT_VAR, "ifc:location_IfcPlacement", COORDS_VAR)
                .addWhere(COORDS_VAR, QueryHandler.RDF_TYPE, "ifc:IfcCartesianPoint")
                // All Ifc elements have a relative position to a parent element's placement except for IfcSite
                .addOptional(PLACEMENT_VAR, "ifc:placementRelTo_IfcLocalPlacement", PARENT_PLACEMENT_VAR);

        // Adds a subgroup with Union pattern as the placement transformer may be either 2D or 3D with different properties
        unionBuilder.addWhere(AXISPLACEMENT_VAR, "ifc:refDirection_IfcAxis2Placement2D", DIRECTION_REF_VAR)
                .addWhere(DIRECTION_REF_VAR, QueryHandler.RDF_TYPE, "ifc:IfcDirection");
        subgroupBuilder.addWhere(AXISPLACEMENT_VAR, "ifc:axis_IfcAxis2Placement3D", DIRECTION_AXIS_VAR)
                .addWhere(DIRECTION_AXIS_VAR, QueryHandler.RDF_TYPE, "ifc:IfcDirection")
                .addWhere(AXISPLACEMENT_VAR, "ifc:refDirection_IfcAxis2Placement3D", DIRECTION_REF_VAR)
                .addWhere(DIRECTION_REF_VAR, QueryHandler.RDF_TYPE, "ifc:IfcDirection");
        subgroupBuilder.addUnion(unionBuilder);
        builder.addWhere(subgroupBuilder);

        // Add VALUES statement for all IRIs
        Map<String, List<RDFNode>> varMap = new HashMap<>();
        varMap.put(PLACEMENT_VAR, iriList);
        builder.addWhereValueVars(varMap);
    }

    /**
     * Add the query statements for all Cartesian transformation operators and their properties.
     *
     * @param iriList List of IRI to Cartesian transformation operators.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructCartesianTransformationOperatorTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        builder.addConstruct(CART_TRANSFORMER_OPERATOR_VAR, QueryHandler.RDF_TYPE, "bim:CartesianTransformationOperator")
                .addConstruct(CART_TRANSFORMER_OPERATOR_VAR, "bim:hasLocalOrigin", CARTPOINT_VAR)
                .addConstruct(CARTPOINT_VAR, QueryHandler.RDF_TYPE, "bim:CartesianPoint")
                .addConstruct(CART_TRANSFORMER_OPERATOR_VAR, "bim:hasScale", SCALE_FACTOR_VAR)
                .addConstruct(CART_TRANSFORMER_OPERATOR_VAR, "bim:hasDerivedXAxis", XDIRECTION_VAR)
                .addConstruct(CART_TRANSFORMER_OPERATOR_VAR, "bim:hasDerivedYAxis", YDIRECTION_VAR);

        // IFC query structure
        builder.addWhere(CART_TRANSFORMER_OPERATOR_VAR, QueryHandler.RDF_TYPE, "ifc:IfcCartesianTransformationOperator3D")
                .addWhere(CART_TRANSFORMER_OPERATOR_VAR, "ifc:localOrigin_IfcCartesianTransformationOperator", CARTPOINT_VAR)
                .addWhere(CARTPOINT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcCartesianPoint");
        // Optional statements for optional IFC properties
        builder.addOptional(CART_TRANSFORMER_OPERATOR_VAR, "ifc:scale_IfcCartesianTransformationOperator/express:hasDouble", SCALE_FACTOR_VAR)
                .addOptional(CART_TRANSFORMER_OPERATOR_VAR, "ifc:axis1_IfcCartesianTransformationOperator", XDIRECTION_VAR)
                .addOptional(CART_TRANSFORMER_OPERATOR_VAR, "ifc:axis2_IfcCartesianTransformationOperator", YDIRECTION_VAR);
        // Add VALUES statement for all IRIs
        Map<String, List<RDFNode>> varMap = new HashMap<>();
        varMap.put(CART_TRANSFORMER_OPERATOR_VAR, iriList);
        builder.addWhereValueVars(varMap);
    }

    /**
     * Add the query statements for all Direction vectors and their properties.
     *
     * @param iriList List of IRI to Cartesian points.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructDirectionVectorTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        builder.addConstruct(DIRECTION_VAR, QueryHandler.RDF_TYPE, "bim:DirectionVector")
                .addConstruct(DIRECTION_VAR, "bim:hasXDirectionRatio", XVALUE_VAR)
                .addConstruct(DIRECTION_VAR, "bim:hasYDirectionRatio", YVALUE_VAR)
                .addConstruct(DIRECTION_VAR, "bim:hasZDirectionRatio", ZVALUE_VAR);

        // IFC query structure
        builder.addWhere(DIRECTION_VAR, QueryHandler.RDF_TYPE, "ifc:IfcDirection")
                .addWhere(DIRECTION_VAR, "ifc:directionRatios_IfcDirection", XLIST_VAR)
                .addWhere(XLIST_VAR, "list:hasContents/express:hasDouble", XVALUE_VAR)
                .addWhere(XLIST_VAR, "list:hasNext", YLIST_VAR)
                .addWhere(YLIST_VAR, "list:hasContents/express:hasDouble", YVALUE_VAR)
                .addOptional(YLIST_VAR, "list:hasNext/list:hasContents/express:hasDouble", ZVALUE_VAR); // Z-coordinates are optional in Ifc
        // Add VALUES statement for all IRIs
        Map<String, List<RDFNode>> varMap = new HashMap<>();
        varMap.put(DIRECTION_VAR, iriList);
        builder.addWhereValueVars(varMap);
    }

    /**
     * Add the query statements for all Cartesian points and their properties.
     *
     * @param iriList List of IRI to Cartesian points.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructCartesianPointTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        builder.addConstruct(CARTPOINT_VAR, QueryHandler.RDF_TYPE, "bim:CartesianPoint")
                .addConstruct(CARTPOINT_VAR, "bim:hasXCoordinate", XVALUE_VAR)
                .addConstruct(CARTPOINT_VAR, "bim:hasYCoordinate", YVALUE_VAR)
                .addConstruct(CARTPOINT_VAR, "bim:hasZCoordinate", ZVALUE_VAR);

        // IFC query structure
        builder.addWhere(CARTPOINT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcCartesianPoint")
                .addWhere(CARTPOINT_VAR, "ifc:coordinates_IfcCartesianPoint", XLIST_VAR)
                .addWhere(XLIST_VAR, "list:hasContents/express:hasDouble", XVALUE_VAR)
                .addWhere(XLIST_VAR, "list:hasNext", YLIST_VAR)
                .addWhere(YLIST_VAR, "list:hasContents/express:hasDouble", YVALUE_VAR)
                .addOptional(YLIST_VAR, "list:hasNext/list:hasContents/express:hasDouble", ZVALUE_VAR); // Z-coordinates are optional in Ifc
        // Add VALUES statement for all IRIs
        Map<String, List<RDFNode>> varMap = new HashMap<>();
        varMap.put(CARTPOINT_VAR, iriList);
        builder.addWhereValueVars(varMap);
    }
}
