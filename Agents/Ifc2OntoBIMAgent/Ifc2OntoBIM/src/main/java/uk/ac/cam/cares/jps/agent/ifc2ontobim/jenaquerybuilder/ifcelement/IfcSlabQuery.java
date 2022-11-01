package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.shared.PrefixMapping;
import org.apache.jena.shared.impl.PrefixMappingImpl;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

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
    private static final String SLAB_ENUM_VAR = "?slabEnum";
    private static final String ROOFZONE_VAR = "?roofzone";
    private static final String IFCROOF_VAR = "?ifcroof";
    private static final String ROOF_RELAGG_VAR = "?roofrelaggregate";
    private static final String REL_SPATIAL_STRUCTURE_VAR = "?relspatialcontainer";

    /**
     * Switch function that adds the relevant statements according to input.
     *
     * @param builder  Construct Builder object to add Construct query statements.
     * @param bimClass The object's class name in the OntoBIM ontology.
     */
    protected static void addSlabQueryComponents(ConstructBuilder builder, String bimClass) {
        builder.addWhere(IfcElementConstructBuilder.REL_TYPE_DEF_VAR, QueryHandler.RDF_TYPE, "ifc:IfcRelDefinesByType")
                .addWhere(IfcElementConstructBuilder.REL_TYPE_DEF_VAR, "ifc:relatedObjects_IfcRelDefines", IfcElementConstructBuilder.ELEMENT_VAR)
                .addWhere(IfcElementConstructBuilder.REL_TYPE_DEF_VAR, "ifc:relatingType_IfcRelDefinesByType", IfcElementConstructBuilder.ELEMENT_TYPE_VAR)
                .addWhere(IfcElementConstructBuilder.ELEMENT_TYPE_VAR, "ifc:predefinedType_IfcSlabType", SLAB_ENUM_VAR);

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
        builder.addWhereValueVar(SLAB_ENUM_VAR, "ifc:FLOOR", "ifc:BASESLAB");
    }

    /**
     * Add the statements for querying the roof rather than other slab types.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private static void addRoofFilterQueryComponents(ConstructBuilder builder) {
        builder.addWhere(IfcElementConstructBuilder.ELEMENT_TYPE_VAR, "ifc:predefinedType_IfcSlabType", "ifc:ROOF");
    }

    /**
     * Add the statements for querying the spatial zone holding the roof.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private static void addRoofSpatialLocationQueryComponents(ConstructBuilder builder) {
        builder.addConstruct(ROOFZONE_VAR, "bot:containsElement", IfcElementConstructBuilder.ELEMENT_VAR);
        builder.addWhere(ROOF_RELAGG_VAR, QueryHandler.RDF_TYPE, "ifc:IfcRelAggregates")
                .addWhere(ROOF_RELAGG_VAR, "ifc:relatingObject_IfcRelDecomposes", IFCROOF_VAR)
                .addWhere(ROOF_RELAGG_VAR, "ifc:relatedObjects_IfcRelDecomposes", IfcElementConstructBuilder.ELEMENT_TYPE_VAR)
                .addWhere(REL_SPATIAL_STRUCTURE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcRelContainedInSpatialStructure")
                .addWhere(REL_SPATIAL_STRUCTURE_VAR, "ifc:relatedElements_IfcRelContainedInSpatialStructure", IFCROOF_VAR)
                .addWhere(REL_SPATIAL_STRUCTURE_VAR, "ifc:relatingStructure_IfcRelContainedInSpatialStructure", ROOFZONE_VAR);
    }
}
