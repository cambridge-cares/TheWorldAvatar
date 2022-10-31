package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcgeometry;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.rdf.model.RDFNode;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Provides query statements relevant to all geospatial operators like GeometricRepresentationSubContext, DirectionVector, CartesianPoint, and more.
 *
 * @author qhouyee
 */
class IfcGeospatialOperatorConstructBuilder {
    /**
     * Add the query statements for all geometric representation sub-context and their properties.
     *
     * @param iriList List of IRI to geometric representation sub-context.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructGeometricRepresentationSubContextTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        builder.addConstruct("?subcontext", "rdf:type", "bim:GeometricRepresentationSubContext")
                .addConstruct("?subcontext", "bim:hasParentContext", "?subContextParent")
                .addConstruct("?subcontext", "bim:hasContextIdentifier", "?subContextIdentifier")
                .addConstruct("?subcontext", "bim:hasContextType", "?subContextViewType")
                .addConstruct("?subcontext", "bim:hasTargetView", "?subContextTargetView");

        //IFC query structure
        builder.addWhere("?subcontext", "rdf:type", "ifc:IfcGeometricRepresentationSubContext")
                .addWhere("?subcontext", "ifc:parentContext_IfcGeometricRepresentationSubContext", "?subContextParent")
                .addWhere("?subcontext", "ifc:contextIdentifier_IfcRepresentationContext/express:hasString", "?subContextIdentifier")
                .addWhere("?subcontext", "ifc:contextType_IfcRepresentationContext/express:hasString", "?subContextViewType")
                .addWhere("?subcontext", "ifc:targetView_IfcGeometricRepresentationSubContext", "?subContextTargetView");
        // Add VALUES statement for all IRIs
        Map<String, List<RDFNode>> varMap = new HashMap<>();
        varMap.put("?subcontext", iriList);
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
        builder.addConstruct("?localplacement", "rdf:type", "bim:LocalPlacement")
                .addConstruct("?localplacement", "bim:hasRelativePosition", "?parentplacement")
                .addConstruct("?localplacement", "bim:hasPosition", "?coordinates")
                .addConstruct("?coordinates", "rdf:type", "bim:CartesianPoint")
                .addConstruct("?localplacement", "bim:hasRefDirection", "?refdirection")
                .addConstruct("?refdirection", "rdf:type", "bim:DirectionVector")
                .addConstruct("?localplacement", "bim:hasAxisDirection", "?axisdirection")
                .addConstruct("?axisdirection", "rdf:type", "bim:DirectionVector");

        //IFC query structure
        builder.addWhere("?localplacement", "rdf:type", "ifc:IfcLocalPlacement")
                .addWhere("?localplacement", "ifc:relativePlacement_IfcLocalPlacement", "?axisplacement")
                .addWhere("?axisplacement", "ifc:location_IfcPlacement", "?coordinates")
                .addWhere("?coordinates", "rdf:type", "ifc:IfcCartesianPoint")
                // All Ifc elements have a relative position to a parent element's placement except for IfcSite
                .addOptional("?localplacement", "ifc:placementRelTo_IfcLocalPlacement", "?parentplacement");

        // Adds a subgroup with Union pattern as the placement transformer may be either 2D or 3D with different properties
        unionBuilder.addWhere("?axisplacement", "ifc:refDirection_IfcAxis2Placement2D", "?refdirection")
                .addWhere("?refdirection", "rdf:type", "ifc:IfcDirection");
        subgroupBuilder.addWhere("?axisplacement", "ifc:axis_IfcAxis2Placement3D", "?axisdirection")
                .addWhere("?axisdirection", "rdf:type", "ifc:IfcDirection")
                .addWhere("?axisplacement", "ifc:refDirection_IfcAxis2Placement3D", "?refdirection")
                .addWhere("?refdirection", "rdf:type", "ifc:IfcDirection");
        subgroupBuilder.addUnion(unionBuilder);
        builder.addWhere(subgroupBuilder);

        // Add VALUES statement for all IRIs
        Map<String, List<RDFNode>> varMap = new HashMap<>();
        varMap.put("?localplacement", iriList);
        builder.addWhereValueVars(varMap);
    }

    /**
     * Add the query statements for all Cartesian transformation operators and their properties.
     *
     * @param iriList List of IRI to Cartesian transformation operators.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructCartesianTransformationOperatorTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        builder.addConstruct("?operator", "rdf:type", "bim:CartesianTransformationOperator")
                .addConstruct("?operator", "bim:hasLocalOrigin", "?cartesianpoint")
                .addConstruct("?cartesianpoint", "rdf:type", "bim:CartesianPoint")
                .addConstruct("?operator", "bim:hasScale", "?scalefactor")
                .addConstruct("?operator", "bim:hasDerivedXAxis", "?xdirection")
                .addConstruct("?operator", "bim:hasDerivedYAxis", "?ydirection");

        // IFC query structure
        builder.addWhere("?operator", "rdf:type", "ifc:IfcCartesianTransformationOperator3D")
                .addWhere("?operator", "ifc:localOrigin_IfcCartesianTransformationOperator", "?cartesianpoint")
                .addWhere("?cartesianpoint", "rdf:type", "ifc:IfcCartesianPoint");
        // Optional statements for optional IFC properties
        builder.addOptional("?operator", "ifc:scale_IfcCartesianTransformationOperator/express:hasDouble", "?scalefactor")
                .addOptional("?operator", "ifc:axis1_IfcCartesianTransformationOperator", "?xdirection")
                .addOptional("?operator", "ifc:axis2_IfcCartesianTransformationOperator", "?ydirection");
        // Add VALUES statement for all IRIs
        Map<String, List<RDFNode>> varMap = new HashMap<>();
        varMap.put("?operator", iriList);
        builder.addWhereValueVars(varMap);
    }

    /**
     * Add the query statements for all Direction vectors and their properties.
     *
     * @param iriList List of IRI to Cartesian points.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructDirectionVectorTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        builder.addConstruct("?direction", "rdf:type", "bim:DirectionVector")
                .addConstruct("?direction", "bim:hasXDirectionRatio", "?xvalue")
                .addConstruct("?direction", "bim:hasYDirectionRatio", "?yvalue")
                .addConstruct("?direction", "bim:hasZDirectionRatio", "?zvalue");

        // IFC query structure
        builder.addWhere("?direction", "rdf:type", "ifc:IfcDirection")
                .addWhere("?direction", "ifc:directionRatios_IfcDirection", "?xlist")
                .addWhere("?xlist", "list:hasContents/express:hasDouble", "?xvalue")
                .addWhere("?xlist", "list:hasNext", "?ylist")
                .addWhere("?ylist", "list:hasContents/express:hasDouble", "?yvalue")
                .addOptional("?ylist", "list:hasNext/list:hasContents/express:hasDouble", "?zvalue"); // Z-coordinates are optional in Ifc
        // Add VALUES statement for all IRIs
        Map<String, List<RDFNode>> varMap = new HashMap<>();
        varMap.put("?direction", iriList);
        builder.addWhereValueVars(varMap);
    }

    /**
     * Add the query statements for all Cartesian points and their properties.
     *
     * @param iriList List of IRI to Cartesian points.
     * @param builder A Construct Builder object to hold the statements.
     */
    protected static void constructCartesianPointTriples(List<RDFNode> iriList, ConstructBuilder builder) {
        builder.addConstruct("?point", "rdf:type", "bim:CartesianPoint")
                .addConstruct("?point", "bim:hasXCoordinate", "?xvalue")
                .addConstruct("?point", "bim:hasYCoordinate", "?yvalue")
                .addConstruct("?point", "bim:hasZCoordinate", "?zvalue");

        // IFC query structure
        builder.addWhere("?point", "rdf:type", "ifc:IfcCartesianPoint")
                .addWhere("?point", "ifc:coordinates_IfcCartesianPoint", "?xlist")
                .addWhere("?xlist", "list:hasContents/express:hasDouble", "?xvalue")
                .addWhere("?xlist", "list:hasNext", "?ylist")
                .addWhere("?ylist", "list:hasContents/express:hasDouble", "?yvalue")
                .addOptional("?ylist", "list:hasNext/list:hasContents/express:hasDouble", "?zvalue"); // Z-coordinates are optional in Ifc
        // Add VALUES statement for all IRIs
        Map<String, List<RDFNode>> varMap = new HashMap<>();
        varMap.put("?point", iriList);
        builder.addWhereValueVars(varMap);
    }
}
