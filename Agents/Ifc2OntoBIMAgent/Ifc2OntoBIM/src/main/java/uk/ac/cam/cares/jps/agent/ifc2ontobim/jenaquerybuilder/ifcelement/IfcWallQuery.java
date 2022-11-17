package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

/**
 * Provides supplementary query statements relevant to walls.
 *
 * @author qhouyee
 */
class IfcWallQuery {
    private static final String SEC_SHAPEREP_VAR = "?secondshaperep";
    private static final String SEC_SHAPEREP_TYPE_VAR = "?secondshapereptype";
    private static final String SEC_SUBCONTEXT_VAR = "?secondsubcontext";
    private static final String SEC_GEOM_VAR = "?secondgeometry";
    private static final String SEC_GEOM_TYPE_VAR = "?secondgeomtype";


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
        builder.addConstruct(IfcElementConstructBuilder.ELEMENT_VAR, "bim:hasGeometricRepresentation", SEC_SHAPEREP_VAR)
                .addConstruct(SEC_SHAPEREP_VAR, QueryHandler.RDF_TYPE, "bim:ModelRepresentation3D")
                .addConstruct(SEC_SHAPEREP_VAR, "bim:hasRepresentationType", SEC_SHAPEREP_TYPE_VAR)
                .addConstruct(SEC_SHAPEREP_VAR, "bim:hasSubContext", SEC_SUBCONTEXT_VAR) // Sub-context
                .addConstruct(SEC_SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, "bim:GeometricRepresentationSubContext")
                .addConstruct(SEC_SHAPEREP_VAR, "bim:hasRepresentationItem", SEC_GEOM_VAR) // Geometry
                .addConstruct(SEC_GEOM_VAR, QueryHandler.RDF_TYPE, SEC_GEOM_TYPE_VAR);

        SelectBuilder optionalBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(optionalBuilder);
        optionalBuilder.addWhere("?productDefinitionShape", "ifc:representations_IfcProductRepresentation/list:hasNext/list:hasContents", SEC_SHAPEREP_VAR)
                .addWhere(SEC_SHAPEREP_VAR, QueryHandler.RDF_TYPE, "ifc:IfcShapeRepresentation")
                .addWhere(SEC_SHAPEREP_VAR, "ifc:representationType_IfcRepresentation/express:hasString", SEC_SHAPEREP_TYPE_VAR)
                .addWhere(SEC_SHAPEREP_VAR, "ifc:contextOfItems_IfcRepresentation", SEC_SUBCONTEXT_VAR)
                .addWhere(SEC_SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcGeometricRepresentationSubContext")
                .addWhere(SEC_SHAPEREP_VAR, "ifc:items_IfcRepresentation", SEC_GEOM_VAR)
                .addWhere(SEC_GEOM_VAR, QueryHandler.RDF_TYPE, SEC_GEOM_TYPE_VAR);
        builder.addOptional(optionalBuilder);
    }
}
