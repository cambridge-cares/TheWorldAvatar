package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;

/**
 * Provides supplementary query statements relevant to walls.
 *
 * @author qhouyee
 */
class IfcWallQuery {
    // Note that I have excluded the IfcRelConnectsPathElements that indicates if the wall are connected to another wall,
    // and at which end. This class seems only useful for visualising the IFC schema in IFC viewers. But for conversion,
    // it does not matter as the dimensions and positions given should suffice to generate the required geometry.
    /**
     * Add the statements for querying a second shape representation. Usually, the wall and WallStandardCase classes
     * have a polyline for the first shape representation instance, but the second instance may refer to anything else.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    protected static void addSecondShapeRepresentationQueryComponents(ConstructBuilder builder) {
        builder.addConstruct("?element", "bim:hasGeometricRepresentation", "?secondshaperep")
                .addConstruct("?secondshaperep", "rdf:type", "bim:ModelRepresentation3D")
                .addConstruct("?secondshaperep", "bim:hasRepresentationType", "?shapereptype")
                .addConstruct("?secondshaperep", "bim:hasSubContext", "?secondsubcontext") // Sub-context
                .addConstruct("?secondsubcontext", "rdf:type", "bim:GeometricRepresentationSubContext")
                .addConstruct("?secondshaperep", "bim:hasRepresentationItem", "?secondgeometry") // Geometry
                .addConstruct("?secondgeometry", "rdf:type", "?secondgeomtype");

        SelectBuilder optionalBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(optionalBuilder);
        optionalBuilder.addWhere("?productDefinitionShape", "ifc:representations_IfcProductRepresentation/list:hasNext/list:hasContents", "?secondshaperep")
                .addWhere("?secondshaperep", "rdf:type", "ifc:IfcShapeRepresentation")
                .addWhere("?secondshaperep", "ifc:representationType_IfcRepresentation/express:hasString", "?secondshapereptype")
                .addWhere("?secondshaperep", "ifc:contextOfItems_IfcRepresentation", "?secondsubcontext")
                .addWhere("?secondsubcontext", "rdf:type", "ifc:IfcGeometricRepresentationSubContext")
                .addWhere("?secondshaperep", "ifc:items_IfcRepresentation", "?secondgeometry")
                .addWhere("?secondgeometry", "rdf:type", "?secondgeomtype");
        builder.addOptional(optionalBuilder);
    }
}
