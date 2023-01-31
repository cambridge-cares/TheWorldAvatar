package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.Converters;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base.IfcConstructBuilderTemplate;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * Provides query statements specific to all elements such as doors, walls, floors, furnitures, and devices.
 *
 * @author qhouyee
 */
public class IfcElementConstructBuilder extends IfcConstructBuilderTemplate {
    public static final String ELEMENT_CLASS = "bot:Element";
    public static final String ELEMENT_TYPE_VAR = "?elementtype";
    public static final String SHAPEREP_VAR = "?shaperep";
    public static final String INST_SHAPEREP_VAR = "?instshaperep";
    public static final String SHAPEREP_TYPE_VAR = "?shapereptype";
    public static final String SPATIAL_REL_VAR = "?spatialStructureRelationship";
    public static final String MAPPED_ITEM_VAR = "?mappeditem";
    public static final String REP_MAP_VAR = "?representationmap";
    public static final String GEOM_VAR = "?geometry";
    public static final String GEOM_TYPE_VAR = "?geomtype";
    public static final String SUBCONTEXT_VAR = "?subcontext";
    public static final String CART_TRANSFORMER_VAR = "?cartesiantransformer";
    public static final String GEOM_AXISPLACEMENT_VAR = "?geomaxisplacement";
    public static final String PRODUCT_DEFINITION_VAR = "?productDefinitionShape";
    public static final String REL_TYPE_DEF_VAR = "?reltypedefine";


    /**
     * Create the SPARQL query syntax for Construct queries of all Ifc elements.
     *
     * @param builder  Construct Builder object to add Construct query statements.
     * @param ifcClass The element's class name in the IfcOwl ontology.
     * @param bimClass The object's class name in the OntoBIM ontology.
     * @return The SPARQL query string for IFC elements
     */
    public String createSparqlQuery(ConstructBuilder builder, String ifcClass, String bimClass) {
        // Replace ifcClass with their actual classes if identifiers are used
        bimClass = bimClass.equals("ifc:IfcSlabF") ? "ifc:IfcSlab" :
                bimClass.equals("ifc:IfcSlabR") ? "ifc:IfcRoof" : bimClass;
        this.createTemplateSparqlQuery(builder, ifcClass, bimClass);
        // Add the main Element class
        builder.addConstruct(ELEMENT_VAR, QueryHandler.RDF_TYPE, "bot:Element");
        this.switchFunctionDependingOnInput(builder, ifcClass, bimClass);
        return builder.buildString();
    }

    /**
     * A utility method to call other functions that generate additional SPARQL query statements based on the inputs.
     */
    @Override
    protected void switchFunctionDependingOnInput(ConstructBuilder builder, String ifcClass, String bimClass) {
        switch (bimClass) {
            case "ifc:IfcSlab":
            case "ifc:IfcRoof":
                IfcOpeningQuery.addVoidRepresentationQueryComponents(builder);
                IfcSlabQuery.addSlabQueryComponents(builder, ifcClass);
                break;
            case "ifc:IfcDoor":
            case "ifc:IfcWindow":
                IfcOpeningQuery.addHostElementQueryComponents(builder);
                break;
            case "ifc:IfcWall":
            case "ifc:IfcWallStandardCase":
                IfcWallQuery.addSecondShapeRepresentationQueryComponents(builder);
                break;
            case "ifc:IfcStair":
                IfcStairQuery.addSubElementsQueryComponents(builder);
                break;
            case "ifc:IfcCovering":
                IfcCoveringQuery.addCeilingQueryComponents(builder);
                break;
        }

        // Spatial location are usually the same for most elements except for the following classes
        // -- IfcSlab that are classified as Roofs are usually not attached to any zone
        if (!(ifcClass.equals("ifc:IfcSlabR") && bimClass.equals("ifc:IfcRoof"))) {
            this.addSpatialLocationQueryComponents(builder);
        }
        // Geometric representation are usually the same for most elements except for the following classes
        if (!bimClass.equals("ifc:IfcStair")) {
            this.addGeometricRepresentationQueryComponents(builder);
        }
    }

    /**
     * Add the statements for relating each element to the zone they belong in.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addSpatialLocationQueryComponents(ConstructBuilder builder) {
        builder.addConstruct(ZONE_VAR, "bot:containsElement", ELEMENT_VAR);
        builder.addWhere(SPATIAL_REL_VAR, QueryHandler.RDF_TYPE, "ifc:IfcRelContainedInSpatialStructure")
                .addWhere(SPATIAL_REL_VAR, "ifc:relatedElements_IfcRelContainedInSpatialStructure", ELEMENT_VAR)
                .addWhere(SPATIAL_REL_VAR, "ifc:relatingStructure_IfcRelContainedInSpatialStructure", ZONE_VAR);
    }

    /**
     * Add the statements for querying the internal reference system into the builder. This is equivalent to
     * the IfcGeometricRepresentationContext class in Ifc schema.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addGeometricRepresentationQueryComponents(ConstructBuilder builder) {
        builder.addConstruct(ELEMENT_VAR, "bim:hasGeometricRepresentation", INST_SHAPEREP_VAR)
                .addConstruct(INST_SHAPEREP_VAR, QueryHandler.RDF_TYPE, "bim:ModelRepresentation3D")
                .addConstruct(INST_SHAPEREP_VAR, "bim:hasRepresentationType", SHAPEREP_TYPE_VAR)
                .addConstruct(INST_SHAPEREP_VAR, "bim:hasSubContext", SUBCONTEXT_VAR) // Sub-context
                .addConstruct(SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, "bim:GeometricRepresentationSubContext")
                .addConstruct(INST_SHAPEREP_VAR, "bim:hasRepresentationItem", GEOM_VAR) // Geometry
                .addConstruct(GEOM_VAR, QueryHandler.RDF_TYPE, GEOM_TYPE_VAR)
                .addConstruct(INST_SHAPEREP_VAR, "bim:hasTargetPlacement", CART_TRANSFORMER_VAR) // Optional transformer operator
                .addConstruct(CART_TRANSFORMER_VAR, QueryHandler.RDF_TYPE, "bim:CartesianTransformationOperator")
                .addConstruct(INST_SHAPEREP_VAR, "bim:hasSourcePlacement", GEOM_AXISPLACEMENT_VAR) // Optional source placement
                .addConstruct(GEOM_AXISPLACEMENT_VAR, QueryHandler.RDF_TYPE, "bim:LocalPlacement");
        // Common query syntax related to geometric representation triples
        builder.addWhere(ELEMENT_VAR, "ifc:representation_IfcProduct", PRODUCT_DEFINITION_VAR)
                .addWhere(PRODUCT_DEFINITION_VAR, QueryHandler.RDF_TYPE, "ifc:IfcProductDefinitionShape");
        // Sub-query syntax
        this.addGeometricRepresentationUnionSubQuery(builder);
    }


    /**
     * Add a sub-group query comprising two query variations with a UNION pattern. Both queries are enclosed in a group {}
     * Geometric Representation can have one of two query structure in the IFC schema. But there doesn't seem to be
     * a pattern when one occurs or not. As one of the UNION query will return empty values, it would be better to
     * structure the query as such for this case.
     *
     * @param builder Construct Builder object to add the sub-query statements.
     */
    private void addGeometricRepresentationUnionSubQuery(ConstructBuilder builder) {
        // Set up the empty builders and add the necessary prefixes
        SelectBuilder subgroupBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(subgroupBuilder);
        SelectBuilder unionBuilder = subgroupBuilder.clone();
        // Query for the individual shape representation instances that are directly linked to specific geometries
        subgroupBuilder.addWhere(PRODUCT_DEFINITION_VAR, "ifc:representations_IfcProductRepresentation/list:hasContents", INST_SHAPEREP_VAR)
                .addWhere(INST_SHAPEREP_VAR, QueryHandler.RDF_TYPE, "ifc:IfcShapeRepresentation")
                .addWhere(INST_SHAPEREP_VAR, "ifc:representationType_IfcRepresentation/express:hasString", SHAPEREP_TYPE_VAR)
                .addWhere(INST_SHAPEREP_VAR, "ifc:contextOfItems_IfcRepresentation", SUBCONTEXT_VAR)
                .addWhere(SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcGeometricRepresentationSubContext")
                .addWhere(INST_SHAPEREP_VAR, "ifc:items_IfcRepresentation", GEOM_VAR)
                .addWhere(GEOM_VAR, QueryHandler.RDF_TYPE, GEOM_TYPE_VAR);

        // ADD Optional for infinite list has next
        try {
            subgroupBuilder.addFilter("!regex(str(" + GEOM_TYPE_VAR + ") ,'IfcMappedItem')");
        } catch (ParseException e) {
            throw new JPSRuntimeException(e);
        }

        // Query for the family representation instance when there is no geometry available for individual instances
        // In these cases, individual instances are linked as a mapped item and are nested from the individual instances
        unionBuilder.addWhere(PRODUCT_DEFINITION_VAR, "ifc:representations_IfcProductRepresentation/list:hasContents", SHAPEREP_VAR)
                .addWhere(SHAPEREP_VAR, QueryHandler.RDF_TYPE, "ifc:IfcShapeRepresentation")
                .addWhere(SHAPEREP_VAR, "ifc:representationType_IfcRepresentation/express:hasString", Converters.makeLiteral("MappedRepresentation"))
                .addWhere(SHAPEREP_VAR, "ifc:items_IfcRepresentation", MAPPED_ITEM_VAR) // individual instances with no geometries
                .addWhere(MAPPED_ITEM_VAR, QueryHandler.RDF_TYPE, "ifc:IfcMappedItem")
                .addWhere(MAPPED_ITEM_VAR, "ifc:mappingSource_IfcMappedItem", REP_MAP_VAR)
                .addWhere(MAPPED_ITEM_VAR, "ifc:mappingTarget_IfcMappedItem", CART_TRANSFORMER_VAR) // Transforms origin location to new location
                .addWhere(CART_TRANSFORMER_VAR, QueryHandler.RDF_TYPE, "ifc:IfcCartesianTransformationOperator3D")
                .addWhere(REP_MAP_VAR, QueryHandler.RDF_TYPE, "ifc:IfcRepresentationMap")
                // Source placement structure is directly queried here as there is no IfcLocalPlacement instance in this specific case
                .addWhere(REP_MAP_VAR, "ifc:mappingOrigin_IfcRepresentationMap", GEOM_AXISPLACEMENT_VAR)
                // Start of specific instance shape rep
                .addWhere(REP_MAP_VAR, "ifc:mappedRepresentation_IfcRepresentationMap", INST_SHAPEREP_VAR)
                .addWhere(INST_SHAPEREP_VAR, QueryHandler.RDF_TYPE, "ifc:IfcShapeRepresentation")
                .addWhere(INST_SHAPEREP_VAR, "ifc:representationType_IfcRepresentation/express:hasString", SHAPEREP_TYPE_VAR)
                .addWhere(INST_SHAPEREP_VAR, "ifc:contextOfItems_IfcRepresentation", SUBCONTEXT_VAR) // Sub-context
                .addWhere(SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcGeometricRepresentationSubContext")
                .addWhere(INST_SHAPEREP_VAR, "ifc:items_IfcRepresentation", GEOM_VAR) // Geometries
                .addWhere(GEOM_VAR, QueryHandler.RDF_TYPE, GEOM_TYPE_VAR);
        subgroupBuilder.addUnion(unionBuilder);
        builder.addWhere(subgroupBuilder);
    }
}
