package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;

/**
 * Provides supplementary query statements relevant to stairs.
 *
 * @author qhouyee
 */
class IfcStairQuery {
    /**
     * Add the statements for querying the physical sub-elements that constitutes the abstract parent Stair container.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    protected static void addSubElementsQueryComponents(ConstructBuilder builder) {
        builder.addConstruct("?element","bot:hasSubElement","?stairflight")
                .addConstruct("?stairflight","rdf:type", "bim:StairFlight")
                .addConstruct("?element","bot:hasSubElement","?landing")
                .addConstruct("?landing","rdf:type", "bim:Landing")
                .addConstruct("?element","bot:hasSubElement","?railing")
                .addConstruct("?railing","rdf:type", "bim:Railing")
                .addConstruct("?element","bot:hasSubElement","?structurecomponent")
                .addConstruct("?structurecomponent","rdf:type", "bim:StructuralComponent");
        builder.addWhere("?relagg", "rdf:type", "ifc:IfcRelAggregates")
                .addWhere("?relagg", "ifc:relatingObject_IfcRelDecomposes", "?element")
                .addWhere("?relagg", "ifc:relatedObjects_IfcRelDecomposes", "?stairflight")
                .addWhere("?stairflight", "rdf:type", "ifc:IfcStairFlight")
                .addWhere("?relagg", "ifc:relatedObjects_IfcRelDecomposes", "?landing")
                .addWhere("?landing", "rdf:type", "ifc:IfcSlab")
                .addWhere("?relagg", "ifc:relatedObjects_IfcRelDecomposes", "?railing")
                .addWhere("?railing", "rdf:type", "ifc:IfcRailing")
                .addWhere("?relagg", "ifc:relatedObjects_IfcRelDecomposes", "?structurecomponent")
                .addWhere("?structurecomponent", "rdf:type", "ifc:IfcMember");
        addStairFlightQueryComponents(builder);
        addSubElementModelRepresentationQueryComponents(builder,"stairflight");
        addSubElementModelRepresentationQueryComponents(builder,"landing");
        addSubElementModelRepresentationQueryComponents(builder,"railing");
        addSubElementModelRepresentationQueryComponents(builder,"structurecomponent");
    }

    /**
     * Add the statements for querying the information specific to the flight of stairs.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private static void addStairFlightQueryComponents(ConstructBuilder builder) {
        builder.addConstruct("?stairflight", "bim:hasNumOfRiser", "?riserno")
                .addConstruct("?stairflight", "bim:hasNumOfTread", "?treadno")
                .addConstruct("?stairflight", "bim:hasRiserHeight", "?riserheight")
                .addConstruct("?stairflight", "bim:hasTreadLength", "?treadlength");
        builder.addWhere("?stairflight", "ifc:numberOfRiser_IfcStairFlight/express:hasInteger", "?riserno")
                .addWhere("?stairflight", "ifc:numberOfTreads_IfcStairFlight/express:hasInteger", "?treadno")
                .addWhere("?stairflight", "ifc:riserHeight_IfcStairFlight/express:hasDouble", "?riserheight")
                .addWhere("?stairflight", "ifc:treadLength_IfcStairFlight/express:hasDouble", "?treadlength");
    }

    /**
     * Add the statements for querying the sub-elements' model representation.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private static void addSubElementModelRepresentationQueryComponents(ConstructBuilder builder, String subStairComponent) {
        String base =  "?" + subStairComponent;
        String placement = base +"placement";
        String productShapeDefinition = base +"definition";
        String shapeRep = base +"shaperep";
        String shapeRepType = base +"shapereptype";
        String subContext = base +"context";
        String geom = base +"geom";
        String geomType = base +"geomtype";

        builder.addConstruct(base, "bim:hasLocalPosition", placement)
                .addConstruct(placement, "rdf:type", "bim:LocalPlacement")
                .addConstruct(base, "bim:hasGeometricRepresentation", shapeRep)
                .addConstruct(shapeRep, "rdf:type", "bim:ModelRepresentation3D")
                .addConstruct(shapeRep, "bim:hasRepresentationType", shapeRepType)
                .addConstruct(shapeRep, "bim:hasSubContext", subContext)
                .addConstruct(subContext, "rdf:type", "bim:GeometricRepresentationSubContext")
                .addConstruct(shapeRep, "bim:hasRepresentationItem", geom)
                .addConstruct(geom, "rdf:type", geomType);
        builder.addWhere(base, "ifc:objectPlacement_IfcProduct", placement)
                .addWhere(base, "ifc:representation_IfcProduct", productShapeDefinition)
                .addWhere(productShapeDefinition, "ifc:representations_IfcProductRepresentation/list:hasContents", shapeRep)
                .addWhere(shapeRep, "rdf:type", "ifc:IfcShapeRepresentation")
                .addWhere(shapeRep, "ifc:representationType_IfcRepresentation/express:hasString", shapeRepType)
                .addWhere(shapeRep, "ifc:contextOfItems_IfcRepresentation", subContext)
                .addWhere(subContext, "rdf:type", "ifc:IfcGeometricRepresentationSubContext")
                .addWhere(shapeRep, "ifc:items_IfcRepresentation", geom)
                .addWhere(geom, "rdf:type", geomType);
    }
}
