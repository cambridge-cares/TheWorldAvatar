package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

/**
 * Provides a template query in the form of a Construct Builder that is common for all Spatial Zones and Elements classes.
 *
 * @author qhouyee
 */
abstract public class IfcConstructBuilderTemplate {
    public static final String ZONE_VAR = "?zone";
    public static final String ELEMENT_VAR = "?element";
    public static final String NAME_VAR = "?name";
    public static final String UID_VAR = "?uid";
    public static final String RELAGGR_VAR = "?relaggregates";
    public static final String PLACEMENT_VAR = "?localplacement";
    public static final String BIM_PREFIX = "bim:";
    public static final String BASE_PREFIX = "inst:";

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
        if (!bimClass.contains("bim") && !bimClass.contains("bot")  && !bimClass.contains("ifc")) {
            throw new IllegalArgumentException("The namespace of bimClass string does not belong to either bim, bot, or ifc!");
        }
    }

    /**
     * A final template method that cannot be overwritten.
     * Add the statements for querying common metadata such as class name, their unique ifc ID, and name into the builder.
     */
    private void addBaseQueryComponents(ConstructBuilder builder, String ifcClass, String bimClass) {
        builder.addConstruct(ELEMENT_VAR, QueryHandler.RDF_TYPE, bimClass)
                .addConstruct(ELEMENT_VAR, BIM_PREFIX + "hasIfcId", UID_VAR)
                .addConstruct(ELEMENT_VAR, QueryHandler.RDFS_LABEL, NAME_VAR);

        builder.addWhere(ELEMENT_VAR, QueryHandler.RDF_TYPE, ifcClass)
                .addWhere(ELEMENT_VAR, "ifc:globalId_IfcRoot/express:hasString", UID_VAR)
                .addWhere(ELEMENT_VAR, "ifc:name_IfcRoot/express:hasString", NAME_VAR);
    }

    /**
     * A final template method that cannot be overwritten.
     * Add the statements for querying their model positions into the builder.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addModelPositionQueryComponents(ConstructBuilder builder) {
        builder.addConstruct(ELEMENT_VAR, BIM_PREFIX + "hasLocalPosition", PLACEMENT_VAR)
                .addConstruct(PLACEMENT_VAR, QueryHandler.RDF_TYPE, BIM_PREFIX + "LocalPlacement");

        // Ifc Placement query structure
        builder.addWhere(ELEMENT_VAR, "ifc:objectPlacement_IfcProduct", PLACEMENT_VAR)
                .addWhere(PLACEMENT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcLocalPlacement");
    }
}
