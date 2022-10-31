package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base;

import org.apache.jena.arq.querybuilder.ConstructBuilder;

/**
 * Provides query statements specific to the spatial zones such as Site, Building, Building Storey, and Spaces.
 *
 * @author qhouyee
 */
public class IfcSpatialZonesConstructBuilder extends IfcConstructBuilderTemplate {
    /**
     * Create the SPARQL query syntax for Construct queries.
     *
     * @param builder  Construct Builder object to add Construct query statements.
     * @param ifcClass The IfcOwl ontology's class name for the spatial structure element.
     * @param botClass The bot ontology's class name for the spatial structure element.
     * @return The SPARQL query string for spatial zones
     */
    public String createSparqlQuery(ConstructBuilder builder, String ifcClass, String botClass) {
        // Calls the template method to generate the base SPARQL query statements
        this.createTemplateSparqlQuery(builder, ifcClass, botClass);

        // Calls methods specific to each input to generate additional SPARQL query statements
        this.switchFunctionDependingOnInput(builder, ifcClass, botClass);
        return builder.buildString();
    }

    /**
     * A utility method to call other functions that generate additional SPARQL query statements based on the inputs.
     */
    @Override
    protected void switchFunctionDependingOnInput(ConstructBuilder builder, String ifcClass, String botClass) {
        switch (ifcClass) {
            case "ifc:IfcSite":
                addReferenceSystemParamQueryComponents(builder);
                break;
            case "ifc:IfcBuilding":
            case "ifc:IfcSpace":
                addSpatialLocationQueryComponents(builder, ifcClass);
                break;
            case "ifc:IfcBuildingStorey":
                addSpatialLocationQueryComponents(builder, ifcClass);
                addBuildingStoreyQueryComponents(builder);
                break;
        }
    }

    /**
     * Add the statements for relating each spatial structure element to their parent element.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addSpatialLocationQueryComponents(ConstructBuilder builder, String ifcClass) {
        switch (ifcClass) {
            case "ifc:IfcBuilding":
                builder.addConstruct("?zone", "bot:hasBuilding", "?element");
                break;
            case "ifc:IfcBuildingStorey":
                builder.addConstruct("?zone", "bot:hasStorey", "?element");
                break;
            case "ifc:IfcSpace":
                builder.addConstruct("?zone", "bot:hasSpace", "?element");
                break;
        }

        builder.addWhere("?relaggregates", "rdf:type", "ifc:IfcRelAggregates")
                .addWhere("?relaggregates", "ifc:relatingObject_IfcRelDecomposes", "?zone")
                .addWhere("?relaggregates", "ifc:relatedObjects_IfcRelDecomposes", "?element");
    }

    /**
     * Add the statements specifically for building storeys.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addBuildingStoreyQueryComponents(ConstructBuilder builder) {
        builder.addConstruct("?element", "bim:hasRefElevation", "?refelevation");
        builder.addWhere("?element", "ifc:elevation_IfcBuildingStorey/express:hasDouble", "?refelevation");
    }

    /**
     * Add the statements for querying the reference system parameters like Latitude, longitude, elevation into the builder.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addReferenceSystemParamQueryComponents(ConstructBuilder builder) {
        builder.addConstruct("?element", "bim:hasRefElevation", "?elevation")
                .addConstruct("?element", "bim:hasRefLatitude", "?latcompoundangle")
                .addConstruct("?latcompoundangle", "rdf:type", "bim:Latitude")
                .addConstruct("?latcompoundangle", "bim:hasDegree", "?latdegree")
                .addConstruct("?latcompoundangle", "bim:hasMinute", "?latminute")
                .addConstruct("?latcompoundangle", "bim:hasSecond", "?latsecond")
                .addConstruct("?latcompoundangle", "bim:hasMillionthSecond", "?latmilsecond")
                .addConstruct("?element", "bim:hasRefLongitude", "?longcompoundangle")
                .addConstruct("?longcompoundangle", "rdf:type", "bim:Longitude")
                .addConstruct("?longcompoundangle", "bim:hasDegree", "?longdegree")
                .addConstruct("?longcompoundangle", "bim:hasMinute", "?longminute")
                .addConstruct("?longcompoundangle", "bim:hasSecond", "?longsecond")
                .addConstruct("?longcompoundangle", "bim:hasMillionthSecond", "?longmilsecond");

        builder.addWhere("?element", "ifc:refLatitude_IfcSite", "?latcompoundangle")
                .addWhere("?latcompoundangle", "rdf:type", "ifc:IfcCompoundPlaneAngleMeasure")
                .addWhere("?latcompoundangle", "list:hasContents/express:hasInteger", "?latdegree")
                .addWhere("?latcompoundangle", "list:hasNext/list:hasContents/express:hasInteger", "?latminute")
                .addWhere("?latcompoundangle", "list:hasNext/list:hasNext/list:hasContents/express:hasInteger", "?latsecond")
                .addOptional("?latcompoundangle", "list:hasNext/list:hasNext/list:hasNext/list:hasContents/express:hasInteger", "?latmilsecond")
                .addWhere("?element", "ifc:refLongitude_IfcSite", "?longcompoundangle")
                .addWhere("?longcompoundangle", "rdf:type", "ifc:IfcCompoundPlaneAngleMeasure")
                .addWhere("?longcompoundangle", "list:hasContents/express:hasInteger", "?longdegree")
                .addWhere("?longcompoundangle", "list:hasNext/list:hasContents/express:hasInteger", "?longminute")
                .addWhere("?longcompoundangle", "list:hasNext/list:hasNext/list:hasContents/express:hasInteger", "?longsecond")
                .addOptional("?longcompoundangle", "list:hasNext/list:hasNext/list:hasNext/list:hasContents/express:hasInteger", "?longmilsecond")
                .addWhere("?element", "ifc:refElevation_IfcSite/express:hasDouble", "?elevation");
    }
}
