package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

/**
 * Provides query statements specific to the Ifc Project container class.
 *
 * @author qhouyee
 */
public class IfcProjectConstructBuilder {
    private final String PROJECT_VAR = "?project";
    private final String ROOT_VAR = "?root";

    /**
     * Create the SPARQL query syntax for Construct queries.
     *
     * @param builder Construct Builder object to add Construct query statements.
     * @return The SPARQL query string for the project container.
     */
    public String createSparqlQuery(ConstructBuilder builder) {
        this.createProjectSparqlQuery(builder);
        return builder.buildString();
    }

    /**
     * Add the statements for querying the project's container into the builder. This is equivalent to
     * the IfcProject class in Ifc schema.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void createProjectSparqlQuery(ConstructBuilder builder) {
        builder.addConstruct(PROJECT_VAR, "bim:hasRootZone", ROOT_VAR);

        builder.addWhere(PROJECT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcProject")
                .addWhere(IfcConstructBuilderTemplate.RELAGGR_VAR, QueryHandler.RDF_TYPE, "ifc:IfcRelAggregates")
                .addWhere(IfcConstructBuilderTemplate.RELAGGR_VAR, "ifc:relatingObject_IfcRelDecomposes", PROJECT_VAR)
                .addWhere(IfcConstructBuilderTemplate.RELAGGR_VAR, "ifc:relatedObjects_IfcRelDecomposes", ROOT_VAR);
    }
}
