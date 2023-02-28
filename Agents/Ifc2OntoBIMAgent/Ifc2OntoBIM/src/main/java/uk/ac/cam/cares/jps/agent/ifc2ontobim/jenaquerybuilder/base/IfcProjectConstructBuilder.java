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
    private final String REPCONTEXT_VAR = "?repcontext";
    private final String ROOT_VAR = "?root";
    private final String PHASE_VAR = "?phase";

    /**
     * Create the SPARQL query syntax for Construct queries.
     *
     * @param builder Construct Builder object to add Construct query statements.
     * @return The SPARQL query string for the project container.
     */
    public String createSparqlQuery(ConstructBuilder builder) {
        this.createProjectSparqlQuery(builder);
        this.addGeometricRepresentationContextQueryComponent(builder);
        return builder.buildString();
    }

    /**
     * Add the statements for querying the project's container into the builder. This is equivalent to
     * the IfcProject class in Ifc schema.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void createProjectSparqlQuery(ConstructBuilder builder) {
        builder.addConstruct(PROJECT_VAR, "bim:hasContext", REPCONTEXT_VAR)
                .addConstruct(PROJECT_VAR, "bim:hasRootZone", ROOT_VAR);

        builder.addWhere(PROJECT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcProject")
                .addWhere(PROJECT_VAR, "ifc:representationContexts_IfcProject", REPCONTEXT_VAR)
                .addWhere(IfcConstructBuilderTemplate.RELAGGR_VAR, QueryHandler.RDF_TYPE, "ifc:IfcRelAggregates")
                .addWhere(IfcConstructBuilderTemplate.RELAGGR_VAR, "ifc:relatingObject_IfcRelDecomposes", PROJECT_VAR)
                .addWhere(IfcConstructBuilderTemplate.RELAGGR_VAR, "ifc:relatedObjects_IfcRelDecomposes", ROOT_VAR);
    }

    /**
     * Add the statements for querying the project's geometric representation context into the builder.
     * This is equivalent to the IfcGeometricRepresentationContext class in Ifc schema.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addGeometricRepresentationContextQueryComponent(ConstructBuilder builder) {
        builder.addConstruct(REPCONTEXT_VAR, QueryHandler.RDF_TYPE, "bim:GeometricRepresentationContext")
                .addConstruct(REPCONTEXT_VAR, "bim:hasSpaceDimensions", "?spacedimensions")
                .addConstruct(REPCONTEXT_VAR, "bim:hasPrecision", "?modelprecision")
                .addConstruct(REPCONTEXT_VAR, "bim:hasTrueNorth", "?northdirection")
                .addConstruct(REPCONTEXT_VAR, "bim:hasWorldCoordinateSystem", "?modelplacement")
                .addConstruct("?northdirection", QueryHandler.RDF_TYPE, "bim:DirectionVector")
                .addConstruct("?modelplacement", QueryHandler.RDF_TYPE, "bim:LocalPlacement");
        builder.addWhere(REPCONTEXT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcGeometricRepresentationContext")
                .addWhere(REPCONTEXT_VAR, "ifc:worldCoordinateSystem_IfcGeometricRepresentationContext", "?modelplacement")
                .addWhere(REPCONTEXT_VAR, "ifc:coordinateSpaceDimension_IfcGeometricRepresentationContext/express:hasInteger", "?spacedimensions")
                .addWhere(REPCONTEXT_VAR, "ifc:precision_IfcGeometricRepresentationContext/express:hasDouble", "?modelprecision")
                .addWhere(REPCONTEXT_VAR, "ifc:trueNorth_IfcGeometricRepresentationContext", "?northdirection");
    }
}
