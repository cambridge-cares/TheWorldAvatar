package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

/**
 * Provides supplementary query statements relevant to coverings, including ceiling, flooring, and roofing.
 *
 * @author qhouyee
 */
class IfcCoveringQuery {
    /**
     * Add the statements for querying the ceiling rather than other covering types.
     *
     * @param builder  Construct Builder object to add Construct query statements.
     */
    protected static void addCeilingQueryComponents(ConstructBuilder builder) {
        builder.addWhere(IfcElementConstructBuilder.REL_TYPE_DEF_VAR, QueryHandler.RDF_TYPE, " ifc:IfcRelDefinesByType")
                .addWhere(IfcElementConstructBuilder.REL_TYPE_DEF_VAR, "ifc:relatedObjects_IfcRelDefines", IfcElementConstructBuilder.ELEMENT_VAR)
                .addWhere(IfcElementConstructBuilder.REL_TYPE_DEF_VAR, "ifc:relatingType_IfcRelDefinesByType", IfcElementConstructBuilder.ELEMENT_TYPE_VAR)
                .addWhere(IfcElementConstructBuilder.ELEMENT_TYPE_VAR, QueryHandler.RDF_TYPE, "ifc:IfcCoveringType")
                .addWhere(IfcElementConstructBuilder.ELEMENT_TYPE_VAR, "ifc:predefinedType_IfcCoveringType", "ifc:CEILING");
    }
}
