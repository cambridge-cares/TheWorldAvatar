package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

/**
 * Provides supplementary query statements relevant to voids or openings.
 *
 * @author qhouyee
 */
class IfcOpeningQuery {
    private static final String OPENING_ELEMENT_VAR = "?openingelement";
    private static final String REL_VOID_ELEMENT_VAR = "?relvoidelement";
    private static final String VOID_TYPE_VAR = "?voidtype";
    private static final String VOID_PLACEMENT_VAR = "?voidplacement";
    private static final String VOID_SHAPEREP_VAR = "?voidshaperep";
    private static final String VOID_PRODUCT_DEF_VAR = "?voidproductdefinition";
    private static final String VOID_REP_TYPE_VAR = "?voidreptype";
    private static final String VOID_SUBCONTEXT_VAR = "?voidsubcontext";
    private static final String VOID_GEOM_VAR = "?voidgeometry";
    private static final String VOID_GEOM_TYPE_VAR = "?voidgeomtype";


    /**
     * Add the statements for querying the voids/openings of an element and their inforamtion.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    protected static void addVoidRepresentationQueryComponents(ConstructBuilder builder) {
        SelectBuilder optionalBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(optionalBuilder);

        // Base Void attributes
        builder.addConstruct(IfcElementConstructBuilder.ELEMENT_VAR, "bim:hasVoid", OPENING_ELEMENT_VAR)
                .addConstruct(OPENING_ELEMENT_VAR, QueryHandler.RDF_TYPE, "bim:GeometricVoid")
                .addConstruct(OPENING_ELEMENT_VAR, "bim:hasVoidType", VOID_TYPE_VAR)
                .addConstruct(OPENING_ELEMENT_VAR, "bim:hasLocalPosition", VOID_PLACEMENT_VAR)
                .addConstruct(VOID_PLACEMENT_VAR, QueryHandler.RDF_TYPE, "bim:LocalPlacement")
                .addConstruct(OPENING_ELEMENT_VAR, "bim:hasGeometricRepresentation", VOID_SHAPEREP_VAR);

        optionalBuilder.addWhere(REL_VOID_ELEMENT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcRelVoidsElement")
                .addWhere(REL_VOID_ELEMENT_VAR, "ifc:relatedOpeningElement_IfcRelVoidsElement", OPENING_ELEMENT_VAR)
                .addWhere(REL_VOID_ELEMENT_VAR, "ifc:relatingBuildingElement_IfcRelVoidsElement  ", IfcElementConstructBuilder.ELEMENT_VAR)
                .addWhere(OPENING_ELEMENT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcOpeningElement")
                .addWhere(OPENING_ELEMENT_VAR, "ifc:objectType_IfcObject/express:hasString", VOID_TYPE_VAR)
                .addWhere(OPENING_ELEMENT_VAR, "ifc:objectPlacement_IfcProduct", VOID_PLACEMENT_VAR)
                .addWhere(OPENING_ELEMENT_VAR, "ifc:representation_IfcProduct", VOID_PRODUCT_DEF_VAR)
                .addWhere(VOID_PRODUCT_DEF_VAR, "ifc:representations_IfcProductRepresentation/list:hasContents", VOID_SHAPEREP_VAR);
        // Void shape representation details
        builder.addConstruct(VOID_SHAPEREP_VAR, QueryHandler.RDF_TYPE, "bim:ModelRepresentation3D")
                .addConstruct(VOID_SHAPEREP_VAR, "bim:hasRepresentationType", VOID_REP_TYPE_VAR)
                .addConstruct(VOID_SHAPEREP_VAR, "bim:hasSubContext", VOID_SUBCONTEXT_VAR)
                .addConstruct(VOID_SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, "bim:GeometricRepresentationSubContext")
                .addConstruct(VOID_SHAPEREP_VAR, "bim:hasRepresentationItem", VOID_GEOM_VAR)
                .addConstruct(VOID_GEOM_VAR, QueryHandler.RDF_TYPE, VOID_GEOM_TYPE_VAR);
        optionalBuilder.addWhere(VOID_SHAPEREP_VAR, QueryHandler.RDF_TYPE, "ifc:IfcShapeRepresentation")
                .addWhere(VOID_SHAPEREP_VAR, "ifc:representationType_IfcRepresentation/express:hasString", VOID_REP_TYPE_VAR)
                .addWhere(VOID_SHAPEREP_VAR, "ifc:contextOfItems_IfcRepresentation", VOID_SUBCONTEXT_VAR)
                .addWhere(VOID_SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcGeometricRepresentationSubContext")
                .addWhere(VOID_SHAPEREP_VAR, "ifc:items_IfcRepresentation", VOID_GEOM_VAR)
                .addWhere(VOID_GEOM_VAR, QueryHandler.RDF_TYPE, VOID_GEOM_TYPE_VAR);
        builder.addOptional(optionalBuilder);
    }
}
