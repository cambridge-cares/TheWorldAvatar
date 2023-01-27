package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

/**
 * Provides query statements specific to the spatial zones such as Site, Building, Building Storey, and Spaces.
 *
 * @author qhouyee
 */
public class IfcSpatialZonesConstructBuilder extends IfcConstructBuilderTemplate {
    public static final String LAT_ANGLE_VAR = "?latcompoundangle";
    public static final String LAT_DEGREE_VAR = "?latdegree";
    public static final String LAT_MIN_VAR = "?latminute";
    public static final String LAT_SEC_VAR = "?latsecond";
    public static final String LAT_MILSEC_VAR = "?latmilsecond";
    public static final String LONG_ANGLE_VAR = "?longcompoundangle";
    public static final String LONG_DEGREE_VAR = "?longdegree";
    public static final String LONG_MIN_VAR = "?longminute";
    public static final String LONG_SEC_VAR = "?longsecond";
    public static final String LONG_MILSEC_VAR = "?longmilsecond";


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
                builder.addConstruct(ZONE_VAR, "bot:hasBuilding", ELEMENT_VAR);
                break;
            case "ifc:IfcBuildingStorey":
                builder.addConstruct(ZONE_VAR, "bot:hasStorey", ELEMENT_VAR);
                break;
            case "ifc:IfcSpace":
                builder.addConstruct(ZONE_VAR, "bot:hasSpace", ELEMENT_VAR);
                break;
        }

        builder.addWhere(RELAGGR_VAR, QueryHandler.RDF_TYPE, "ifc:IfcRelAggregates")
                .addWhere(RELAGGR_VAR, "ifc:relatingObject_IfcRelDecomposes", ZONE_VAR)
                .addWhere(RELAGGR_VAR, "ifc:relatedObjects_IfcRelDecomposes", ELEMENT_VAR);
    }

    /**
     * Add the statements specifically for building storeys.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addBuildingStoreyQueryComponents(ConstructBuilder builder) {
        builder.addConstruct(ELEMENT_VAR, "bim:hasRefElevation", "?refelevation");
        builder.addWhere(ELEMENT_VAR, "ifc:elevation_IfcBuildingStorey/express:hasDouble", "?refelevation");
    }

    /**
     * Add the statements for querying the reference system parameters like Latitude, longitude, elevation into the builder.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addReferenceSystemParamQueryComponents(ConstructBuilder builder) {
        builder.addConstruct(ELEMENT_VAR, "bim:hasRefElevation", ELEVATION_VAR)
                .addConstruct(ELEMENT_VAR, "bim:hasRefLatitude", LAT_ANGLE_VAR)
                .addConstruct(LAT_ANGLE_VAR, QueryHandler.RDF_TYPE, "bim:CompoundPlaneAngle")
                .addConstruct(LAT_ANGLE_VAR, "bim:hasDegree", LAT_DEGREE_VAR)
                .addConstruct(LAT_ANGLE_VAR, "bim:hasMinute", LAT_MIN_VAR)
                .addConstruct(LAT_ANGLE_VAR, "bim:hasSecond", LAT_SEC_VAR)
                .addConstruct(LAT_ANGLE_VAR, "bim:hasMillionthSecond", LAT_MILSEC_VAR)
                .addConstruct(ELEMENT_VAR, "bim:hasRefLongitude", LONG_ANGLE_VAR)
                .addConstruct(LONG_ANGLE_VAR, QueryHandler.RDF_TYPE, "bim:CompoundPlaneAngle")
                .addConstruct(LONG_ANGLE_VAR, "bim:hasDegree", LONG_DEGREE_VAR)
                .addConstruct(LONG_ANGLE_VAR, "bim:hasMinute", LONG_MIN_VAR)
                .addConstruct(LONG_ANGLE_VAR, "bim:hasSecond", LONG_SEC_VAR)
                .addConstruct(LONG_ANGLE_VAR, "bim:hasMillionthSecond", LONG_MILSEC_VAR);

        builder.addWhere(ELEMENT_VAR, "ifc:refLatitude_IfcSite", LAT_ANGLE_VAR)
                .addWhere(LAT_ANGLE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcCompoundPlaneAngleMeasure")
                .addWhere(LAT_ANGLE_VAR, "list:hasContents/express:hasInteger", LAT_DEGREE_VAR)
                .addWhere(LAT_ANGLE_VAR, "list:hasNext/list:hasContents/express:hasInteger", LAT_MIN_VAR)
                .addWhere(LAT_ANGLE_VAR, "list:hasNext/list:hasNext/list:hasContents/express:hasInteger", LAT_SEC_VAR)
                .addOptional(LAT_ANGLE_VAR, "list:hasNext/list:hasNext/list:hasNext/list:hasContents/express:hasInteger", LAT_MILSEC_VAR)
                .addWhere(ELEMENT_VAR, "ifc:refLongitude_IfcSite", LONG_ANGLE_VAR)
                .addWhere(LONG_ANGLE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcCompoundPlaneAngleMeasure")
                .addWhere(LONG_ANGLE_VAR, "list:hasContents/express:hasInteger", LONG_DEGREE_VAR)
                .addWhere(LONG_ANGLE_VAR, "list:hasNext/list:hasContents/express:hasInteger", LONG_MIN_VAR)
                .addWhere(LONG_ANGLE_VAR, "list:hasNext/list:hasNext/list:hasContents/express:hasInteger", LONG_SEC_VAR)
                .addOptional(LONG_ANGLE_VAR, "list:hasNext/list:hasNext/list:hasNext/list:hasContents/express:hasInteger", LONG_MILSEC_VAR)
                .addWhere(ELEMENT_VAR, "ifc:refElevation_IfcSite/express:hasDouble", ELEVATION_VAR);
    }
}
