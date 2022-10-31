package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base;

import org.apache.jena.arq.querybuilder.ConstructBuilder;

/**
 * Provides a template query in the form of a Construct Builder that is common for all Spatial Zones and Elements classes.
 *
 * @author qhouyee
 */
abstract public class IfcConstructBuilderTemplate {
    protected abstract void switchFunctionDependingOnInput(ConstructBuilder builder, String ifcClass, String botClass);
    /**
     * A final template method that calls the required common methods.
     * All subclasses will call this method to generate a common SPARQL Construct query template.
     * Additional query syntax can be added depending on subclass requirements.
     *
     * @param builder  Construct Builder object to add Construct query statements.
     * @param ifcClass The object's class name in the IfcOwl ontology.
     * @param bimClass The object's class name in the ontoBIM ontology.
     */
    protected final void createTemplateSparqlQuery(ConstructBuilder builder, String ifcClass, String bimClass) {
        this.validateClassArguments(ifcClass, bimClass);
        this.addBaseQueryComponents(builder, ifcClass, bimClass);
        this.addModelPositionQueryComponents(builder);
    }

    /**
     * A final template method that cannot be overwritten. Checks the validity of the class argument inputs.
     * Throws an exception when invalid.
     *
     * @param ifcClass The object's class name in the IfcOwl ontology.
     * @param bimClass The object's class name in the ontoBIM ontology.
     */
    private void validateClassArguments(String ifcClass, String bimClass) {
        if (!ifcClass.contains("ifc")) {
            throw new IllegalArgumentException("ifcClass string is missing the ifc namespace!");
        }
        if (!bimClass.contains("bim") && !bimClass.contains("bot") && !bimClass.contains("saref")) {
            throw new IllegalArgumentException("The namespace of bimClass string does not belong to either bim, bot, or saref!");
        }
    }

    /**
     * A final template method that cannot be overwritten.
     * Add the statements for querying common metadata such as class name, their unique ifc ID, and name into the builder.
     */
    private void addBaseQueryComponents(ConstructBuilder builder, String ifcClass, String bimClass) {
        builder.addConstruct("?element", "rdf:type", bimClass)
                .addConstruct("?element", "bim:hasIfcId", "?uid")
                .addConstruct("?element", "rdfs:label", "?name");

        builder.addWhere("?element", "rdf:type", ifcClass)
                .addWhere("?element", "ifc:globalId_IfcRoot/express:hasString", "?uid")
                .addWhere("?element", "ifc:name_IfcRoot/express:hasString", "?name");
    }

    /**
     * A final template method that cannot be overwritten.
     * Add the statements for querying their model positions into the builder.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addModelPositionQueryComponents(ConstructBuilder builder) {
        builder.addConstruct("?element", "bim:hasLocalPosition", "?localplacement")
                .addConstruct("?localplacement", "rdf:type", "bim:LocalPlacement");

        // Ifc Placement query structure
        builder.addWhere("?element", "ifc:objectPlacement_IfcProduct", "?localplacement")
                .addWhere("?localplacement", "rdf:type", "ifc:IfcLocalPlacement");
    }
}
