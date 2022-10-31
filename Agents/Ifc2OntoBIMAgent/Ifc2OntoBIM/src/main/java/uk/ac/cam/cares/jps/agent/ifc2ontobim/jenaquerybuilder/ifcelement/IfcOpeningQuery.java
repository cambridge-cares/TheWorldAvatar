package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;

/**
 * Provides supplementary query statements relevant to voids or openings.
 *
 * @author qhouyee
 */
class IfcOpeningQuery {
    /**
     * Add the statements for querying the host element of an element that fills a void/opening.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    protected static void addHostElementQueryComponents(ConstructBuilder builder) {
        builder.addConstruct("?hostelement", "bot:hasSubElement", "?element");
        builder.addWhere("?relfillselement", "rdf:type", "ifc:IfcRelFillsElement")
                .addWhere("?relfillselement", "ifc:relatedBuildingElement_IfcRelFillsElement", "?element")
                .addWhere("?relfillselement", "ifc:relatingOpeningElement_IfcRelFillsElement", "?openingelement")
                .addWhere("?openingelement", "rdf:type", "ifc:IfcOpeningElement")
                .addWhere("?relvoidelement", "rdf:type", "ifc:IfcRelVoidsElement")
                .addWhere("?relvoidelement", "ifc:relatedOpeningElement_IfcRelVoidsElement", "?openingelement")
                .addWhere("?relvoidelement", "ifc:relatingBuildingElement_IfcRelVoidsElement", "?hostelement");
    }

    /**
     * Add the statements for querying the voids/openings of an element and their inforamtion.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    protected static void addVoidRepresentationQueryComponents(ConstructBuilder builder) {
        SelectBuilder optionalBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(optionalBuilder);

        // Base Void attributes
        builder.addConstruct("?element", "bim:hasVoid", "?openingelement")
                .addConstruct("?openingelement", "rdf:type", "bim:GeometricVoid")
                .addConstruct("?openingelement", "bim:hasVoidType", "?voidtype")
                .addConstruct("?openingelement", "bim:hasLocalPosition", "?voidplacement")
                .addConstruct("?voidplacement", "rdf:type", "bim:LocalPlacement")
                .addConstruct("?openingelement", "bim:hasGeometricRepresentation", "?voidshaperep");

        optionalBuilder.addWhere("?relvoidelement", "rdf:type", "ifc:IfcRelVoidsElement")
                .addWhere("?relvoidelement", "ifc:relatedOpeningElement_IfcRelVoidsElement", "?openingelement")
                .addWhere("?relvoidelement", "ifc:relatingBuildingElement_IfcRelVoidsElement  ", "?element")
                .addWhere("?openingelement", "rdf:type", "ifc:IfcOpeningElement")
                .addWhere("?openingelement", "ifc:objectType_IfcObject/express:hasString", "?voidtype")
                .addWhere("?openingelement", "ifc:objectPlacement_IfcProduct", "?voidplacement")
                .addWhere("?openingelement", "ifc:representation_IfcProduct", "?voidproductdefinition")
                .addWhere("?voidproductdefinition", "ifc:representations_IfcProductRepresentation/list:hasContents", "?voidshaperep");
        // Void shape representation details
        builder.addConstruct("?voidshaperep", "rdf:type", "bim:ModelRepresentation3D")
                .addConstruct("?voidshaperep", "bim:hasRepresentationType", "?voidreptype")
                .addConstruct("?voidshaperep", "bim:hasSubContext", "?voidsubcontext")
                .addConstruct("?voidsubcontext", "rdf:type", "bim:GeometricRepresentationSubContext")
                .addConstruct("?voidshaperep", "bim:hasRepresentationItem", "?voidgeometry")
                .addConstruct("?voidgeometry", "rdf:type", "?voidgeomtype");
        optionalBuilder.addWhere("?voidshaperep", "rdf:type", "ifc:IfcShapeRepresentation")
                .addWhere("?voidshaperep", "ifc:representationType_IfcRepresentation/express:hasString", "?voidreptype")
                .addWhere("?voidshaperep", "ifc:contextOfItems_IfcRepresentation", "?voidsubcontext")
                .addWhere("?subcontext", "rdf:type", "ifc:IfcGeometricRepresentationSubContext")
                .addWhere("?voidshaperep", "ifc:items_IfcRepresentation", "?voidgeometry")
                .addWhere("?voidgeometry", "rdf:type", "?voidgeomtype");
        builder.addOptional(optionalBuilder);
    }
}
