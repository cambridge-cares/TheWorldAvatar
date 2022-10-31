package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.shared.PrefixMapping;
import org.apache.jena.shared.impl.PrefixMappingImpl;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Provides supplementary query statements relevant to slabs for floor and roof.
 *
 * @author qhouyee
 */
class IfcSlabQuery {
    /**
     * Switch function that adds the relevant statements according to input.
     *
     * @param builder Construct Builder object to add Construct query statements.
     * @param bimClass   The object's class name in the OntoBIM ontology.
     */
    protected static void addSlabQueryComponents(ConstructBuilder builder, String bimClass) {
        builder.addWhere("?reltypedefine","rdf:type"," ifc:IfcRelDefinesByType")
                .addWhere("?reltypedefine","ifc:relatedObjects_IfcRelDefines"," ?element")
                .addWhere("?reltypedefine","ifc:relatingType_IfcRelDefinesByType"," ?elementtype")
                .addWhere("?elementtype","ifc:predefinedType_IfcSlabType","?slabEnum");

        switch (bimClass) {
            case "bim:Floor":
                addFloorFilterQueryComponents(builder);
                break;
            case "bim:Roof":
                addRoofFilterQueryComponents(builder);
                addRoofSpatialLocationQueryComponents(builder);
                break;
        }
    }

    /**
     * Add the statements for querying the floor rather than other slab types. A floor may either be of enum Floor or BaseSlab.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private static void addFloorFilterQueryComponents(ConstructBuilder builder) {
        builder.addWhereValueVar("?slabEnum", "ifc:FLOOR", "ifc:BASESLAB");
    }

    /**
     * Add the statements for querying the roof rather than other slab types.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private static void addRoofFilterQueryComponents(ConstructBuilder builder) {
        builder.addWhere("?elementtype","ifc:predefinedType_IfcSlabType","ifc:ROOF");
    }

    /**
     * Add the statements for querying the spatial zone holding the roof.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private static void addRoofSpatialLocationQueryComponents(ConstructBuilder builder) {
        builder.addConstruct("?roofzone", "bot:containsElement", "?element");
        builder.addWhere("?roofrelaggregate","rdf:type"," ifc:IfcRelAggregates")
                .addWhere("?roofrelaggregate","ifc:relatingObject_IfcRelDecomposes"," ?ifcroof")
                .addWhere("?roofrelaggregate","ifc:relatedObjects_IfcRelDecomposes"," ?element")
                .addWhere("?relspatialcontainer","rdf:type"," ifc:IfcRelContainedInSpatialStructure")
                .addWhere("?relspatialcontainer","ifc:relatedElements_IfcRelContainedInSpatialStructure"," ?ifcroof")
                .addWhere("?relspatialcontainer","ifc:relatingStructure_IfcRelContainedInSpatialStructure"," ?roofzone");
    }
}
