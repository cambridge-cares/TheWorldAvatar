package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

/**
 * Provides supplementary query statements relevant to stairs.
 *
 * @author qhouyee
 */
class IfcStairQuery {
    private static final String STAIRFLIGHT_VAR = "?stairflight";
    private static final String LANDING_VAR = "?landing";
    private static final String RAILING_VAR = "?railing";
    private static final String STRUCTURAL_COMPONENT_VAR = "?structurecomponent";
    private static final String RISER_NUM_VAR = "?riserno";
    private static final String RISER_HEIGHT_VAR = "?riserheight";
    private static final String TREAD_NUM_VAR = "?treadno";
    private static final String TREAD_LENGTH_VAR = "?treadlength";

    /**
     * Add the statements for querying the physical sub-elements that constitutes the abstract parent Stair container.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    protected static void addSubElementsQueryComponents(ConstructBuilder builder) {
        builder.addConstruct(IfcElementConstructBuilder.ELEMENT_VAR, "bim:hasStairSubElement", STAIRFLIGHT_VAR)
                .addConstruct(STAIRFLIGHT_VAR, QueryHandler.RDF_TYPE, "bim:StairFlight")
                .addConstruct(IfcElementConstructBuilder.ELEMENT_VAR, "bim:hasStairSubElement", LANDING_VAR)
                .addConstruct(LANDING_VAR, QueryHandler.RDF_TYPE, "bim:Landing")
                .addConstruct(IfcElementConstructBuilder.ELEMENT_VAR, "bim:hasStairSubElement", RAILING_VAR)
                .addConstruct(RAILING_VAR, QueryHandler.RDF_TYPE, "bim:Railing")
                .addConstruct(IfcElementConstructBuilder.ELEMENT_VAR, "bim:hasStairSubElement", STRUCTURAL_COMPONENT_VAR)
                .addConstruct(STRUCTURAL_COMPONENT_VAR, QueryHandler.RDF_TYPE, "bim:StructuralComponent");
        builder.addWhere(IfcElementConstructBuilder.RELAGGR_VAR, QueryHandler.RDF_TYPE, "ifc:IfcRelAggregates")
                .addWhere(IfcElementConstructBuilder.RELAGGR_VAR, "ifc:relatingObject_IfcRelDecomposes", IfcElementConstructBuilder.ELEMENT_VAR)
                .addWhere(IfcElementConstructBuilder.RELAGGR_VAR, "ifc:relatedObjects_IfcRelDecomposes", STAIRFLIGHT_VAR)
                .addWhere(STAIRFLIGHT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcStairFlight")
                .addWhere(IfcElementConstructBuilder.RELAGGR_VAR, "ifc:relatedObjects_IfcRelDecomposes", LANDING_VAR)
                .addWhere(LANDING_VAR, QueryHandler.RDF_TYPE, "ifc:IfcSlab")
                .addWhere(IfcElementConstructBuilder.RELAGGR_VAR, "ifc:relatedObjects_IfcRelDecomposes", RAILING_VAR)
                .addWhere(RAILING_VAR, QueryHandler.RDF_TYPE, "ifc:IfcRailing")
                .addWhere(IfcElementConstructBuilder.RELAGGR_VAR, "ifc:relatedObjects_IfcRelDecomposes", STRUCTURAL_COMPONENT_VAR)
                .addWhere(STRUCTURAL_COMPONENT_VAR, QueryHandler.RDF_TYPE, "ifc:IfcMember");
        addStairFlightQueryComponents(builder);
        addSubElementModelRepresentationQueryComponents(builder, "stairflight");
        addSubElementModelRepresentationQueryComponents(builder, "landing");
        addSubElementModelRepresentationQueryComponents(builder, "railing");
        addSubElementModelRepresentationQueryComponents(builder, "structurecomponent");
    }

    /**
     * Add the statements for querying the information specific to the flight of stairs.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private static void addStairFlightQueryComponents(ConstructBuilder builder) {
        builder.addConstruct(STAIRFLIGHT_VAR, "bim:hasNumOfRiser", RISER_NUM_VAR)
                .addConstruct(STAIRFLIGHT_VAR, "bim:hasNumOfTread", TREAD_NUM_VAR)
                .addConstruct(STAIRFLIGHT_VAR, "bim:hasRiserHeight", RISER_HEIGHT_VAR)
                .addConstruct(STAIRFLIGHT_VAR, "bim:hasTreadLength", TREAD_LENGTH_VAR);
        builder.addWhere(STAIRFLIGHT_VAR, "ifc:numberOfRiser_IfcStairFlight/express:hasInteger", RISER_NUM_VAR)
                .addWhere(STAIRFLIGHT_VAR, "ifc:numberOfTreads_IfcStairFlight/express:hasInteger", TREAD_NUM_VAR)
                .addWhere(STAIRFLIGHT_VAR, "ifc:riserHeight_IfcStairFlight/express:hasDouble", RISER_HEIGHT_VAR)
                .addWhere(STAIRFLIGHT_VAR, "ifc:treadLength_IfcStairFlight/express:hasDouble", TREAD_LENGTH_VAR);
    }

    /**
     * Add the statements for querying the sub-elements' model representation.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private static void addSubElementModelRepresentationQueryComponents(ConstructBuilder builder, String subStairComponent) {
        String base = "?" + subStairComponent;
        String placement = base + "placement";
        String productShapeDefinition = base + "definition";
        String shapeRep = base + "shaperep";
        String shapeRepType = base + "shapereptype";
        String subContext = base + "context";
        String geom = base + "geom";
        String geomType = base + "geomtype";

        builder.addConstruct(base, "bim:hasLocalPosition", placement)
                .addConstruct(placement, QueryHandler.RDF_TYPE, "bim:LocalPlacement")
                .addConstruct(base, "bim:hasGeometricRepresentation", shapeRep)
                .addConstruct(shapeRep, QueryHandler.RDF_TYPE, "bim:ModelRepresentation3D")
                .addConstruct(shapeRep, "bim:hasRepresentationType", shapeRepType)
                .addConstruct(shapeRep, "bim:hasSubContext", subContext)
                .addConstruct(subContext, QueryHandler.RDF_TYPE, "bim:GeometricRepresentationSubContext")
                .addConstruct(shapeRep, "bim:hasRepresentationItem", geom)
                .addConstruct(geom, QueryHandler.RDF_TYPE, geomType);
        builder.addWhere(base, "ifc:objectPlacement_IfcProduct", placement)
                .addWhere(base, "ifc:representation_IfcProduct", productShapeDefinition)
                .addWhere(productShapeDefinition, "ifc:representations_IfcProductRepresentation/list:hasContents", shapeRep)
                .addWhere(shapeRep, QueryHandler.RDF_TYPE, "ifc:IfcShapeRepresentation")
                .addWhere(shapeRep, "ifc:representationType_IfcRepresentation/express:hasString", shapeRepType)
                .addWhere(shapeRep, "ifc:contextOfItems_IfcRepresentation", subContext)
                .addWhere(subContext, QueryHandler.RDF_TYPE, "ifc:IfcGeometricRepresentationSubContext")
                .addWhere(shapeRep, "ifc:items_IfcRepresentation", geom)
                .addWhere(geom, QueryHandler.RDF_TYPE, geomType);
    }
}
