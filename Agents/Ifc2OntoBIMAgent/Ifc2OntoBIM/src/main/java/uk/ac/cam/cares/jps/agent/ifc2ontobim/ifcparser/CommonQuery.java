package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;

/**
 * Provides reusable query statements for spatial zones.
 *
 * @author qhouyee
 */
public class CommonQuery {
    protected static final String UID_VAR = "?uid";
    protected static final String NAME_VAR = "?name";
    protected static final String ZONE_VAR = "?zone";
    protected static final String PARENT_ZONE_VAR = "?subzone";
    protected static final String RELAGGR_VAR = "?relaggregates";
    protected static final String LAT_VAR = "?latitude";
    protected static final String LAT_DEGREE_VAR = "?latdegree";
    protected static final String LAT_MIN_VAR = "?latminute";
    protected static final String LAT_SEC_VAR = "?latsecond";
    protected static final String LAT_MIL_SEC_VAR = "?latmilsec";
    protected static final String LONG_VAR = "?longitude";
    protected static final String LONG_DEGREE_VAR = "?longdegree";
    protected static final String LONG_MIN_VAR = "?longtminute";
    protected static final String LONG_SEC_VAR = "?longsecond";
    protected static final String LONG_MIL_SEC_VAR = "?longmilsec";
    protected static final String ELEVATION_VAR = "?elev";
    protected static final String TER_ELEVATION_VAR = "?terElev";
    // IfcOwl Properties
    protected static final String EXPRESS_HASDOUBLE = "/express:hasDouble";
    protected static final String EXPRESS_HASINTEGER = "/express:hasInteger";
    protected static final String EXPRESS_HASSTRING = "/express:hasString";
    protected static final String LIST_HAS_CONTENT = "list:hasContents";
    protected static final String LIST_HAS_NEXT = "list:hasNext";
    protected static final String IFC_ID = NamespaceMapper.IFC_PREFIX + ":globalId_IfcRoot";
    protected static final String IFC_NAME= NamespaceMapper.IFC_PREFIX + ":name_IfcRoot";
    protected static final String IFC_SITE_ELEV = NamespaceMapper.IFC_PREFIX + ":refElevation_IfcSite";
    protected static final String IFC_BUILDING_ELEV = NamespaceMapper.IFC_PREFIX + ":elevationOfRefHeight_IfcBuilding";
    protected static final String IFC_BUILDING_TERELEV = NamespaceMapper.IFC_PREFIX + ":elevationOfTerrain_IfcBuilding";
    protected static final String IFC_STOREY_ELEV = NamespaceMapper.IFC_PREFIX + ":elevation_IfcBuildingStorey";
    protected static final String IFC_PARENT_ZONE_REL = NamespaceMapper.IFC_PREFIX + ":relatingObject_IfcRelDecomposes";
    protected static final String IFC_CHILD_ZONE_REL = NamespaceMapper.IFC_PREFIX + ":relatedObjects_IfcRelDecomposes";
    protected static final String IFC_REF_LAT = NamespaceMapper.IFC_PREFIX + ":refLatitude_IfcSite";
    protected static final String IFC_REF_LONG = NamespaceMapper.IFC_PREFIX + ":refLongitude_IfcSite";
    // IfcOwl Classes
    protected static final String IFCSITE = NamespaceMapper.IFC_PREFIX + ":IfcSite";
    protected static final String IFCBUILDING = NamespaceMapper.IFC_PREFIX + ":IfcBuilding";
    protected static final String IFCSTOREY = NamespaceMapper.IFC_PREFIX + ":IfcBuildingStorey";
    protected static final String IFCSPACE = NamespaceMapper.IFC_PREFIX + ":IfcSpace";
    protected static final String RELAGG = NamespaceMapper.IFC_PREFIX + ":IfcRelAggregates";
    protected static final String IFCCOMPOUND_PLANE_ANGLE = NamespaceMapper.IFC_PREFIX + ":IfcCompoundPlaneAngleMeasure";

    /**
     * Add the statements for querying common metadata such as class name, their unique ifc ID, and name into the builder.
     *
     * @param builder A select builder object to append the statements to.
     */
    public static void addBaseQueryComponents(SelectBuilder builder) {
        builder.addVar(CommonQuery.UID_VAR)
                .addVar(CommonQuery.NAME_VAR);
        builder.addWhere(ZONE_VAR, IFC_ID + EXPRESS_HASSTRING, UID_VAR)
                .addWhere(ZONE_VAR, IFC_NAME + EXPRESS_HASSTRING, NAME_VAR);
    }
}
