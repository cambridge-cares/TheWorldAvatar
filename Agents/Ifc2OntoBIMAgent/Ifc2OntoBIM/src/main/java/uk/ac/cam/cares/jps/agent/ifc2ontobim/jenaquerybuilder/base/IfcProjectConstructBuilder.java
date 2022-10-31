package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base;

import org.apache.jena.arq.querybuilder.ConstructBuilder;

/**
 * Provides query statements specific to the Ifc Project container class.
 *
 * @author qhouyee
 */
public class IfcProjectConstructBuilder {
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
        builder.addConstruct("?project", "rdfs:label", "?name")
                .addConstruct("?project", "bim:hasPhase", "?phase")
                .addConstruct("?project", "bim:hasContext", "?repcontext")
                .addConstruct("?project", "bim:hasRootZone", "?root");

        builder.addWhere("?project", "rdf:type", "ifc:IfcProject")
                .addWhere("?project", "ifc:longName_IfcProject/express:hasString", "?name")
                .addWhere("?project", "ifc:phase_IfcProject/express:hasString", "?phase")
                .addWhere("?project", "ifc:representationContexts_IfcProject", "?repcontext")
                .addWhere("?relaggregate", "rdf:type", "ifc:IfcRelAggregates")
                .addWhere("?relaggregate", "ifc:relatingObject_IfcRelDecomposes", "?project")
                .addWhere("?relaggregate", "ifc:relatedObjects_IfcRelDecomposes", "?root");
    }

    /**
     * Add the statements for querying the project's geometric representation context into the builder.
     * This is equivalent to the IfcGeometricRepresentationContext class in Ifc schema.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addGeometricRepresentationContextQueryComponent(ConstructBuilder builder) {
        builder.addConstruct("?repcontext", "rdf:type", "bim:GeometricRepresentationContext")
                .addConstruct("?repcontext", "bim:hasSpaceDimensions", "?spacedimensions")
                .addConstruct("?repcontext", "bim:hasPrecision", "?modelprecision")
                .addConstruct("?repcontext", "bim:hasTrueNorth", "?northdirection")
                .addConstruct("?repcontext", "bim:hasWorldCoordinateSystem", "?modelplacement")
                .addConstruct("?northdirection", "rdf:type", "bim:DirectionVector")
                .addConstruct("?modelplacement", "rdf:type", "bim:LocalPlacement");
        builder.addWhere("?repcontext", "rdf:type", "ifc:IfcGeometricRepresentationContext")
                .addWhere("?repcontext", "ifc:worldCoordinateSystem_IfcGeometricRepresentationContext", "?modelplacement")
                .addWhere("?repcontext", "ifc:coordinateSpaceDimension_IfcGeometricRepresentationContext/express:hasInteger", "?spacedimensions")
                .addWhere("?repcontext", "ifc:precision_IfcGeometricRepresentationContext/express:hasDouble", "?modelprecision")
                .addWhere("?repcontext", "ifc:trueNorth_IfcGeometricRepresentationContext", "?northdirection");
    }
}
