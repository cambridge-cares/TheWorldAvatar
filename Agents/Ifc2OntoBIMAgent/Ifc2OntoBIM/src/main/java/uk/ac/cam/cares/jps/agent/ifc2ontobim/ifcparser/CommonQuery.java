package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.Converters;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * Provides reusable query statements for spatial zones.
 *
 * @author qhouyee
 */
public class CommonQuery {
    public static final String UID_VAR = "?uid";
    public static final String NAME_VAR = "?name";
    public static final String ZONE_VAR = "?zone";
    public static final String PARENT_ZONE_VAR = "?subzone";
    public static final String PHASE_VAR = "?phase";
    public static final String RELAGGR_VAR = "?relaggregates";
    public static final String LAT_VAR = "?latitude";
    public static final String LAT_DEGREE_VAR = "?latdegree";
    public static final String LAT_MIN_VAR = "?latminute";
    public static final String LAT_SEC_VAR = "?latsecond";
    public static final String LAT_MIL_SEC_VAR = "?latmilsec";
    public static final String LONG_VAR = "?longitude";
    public static final String LONG_DEGREE_VAR = "?longdegree";
    public static final String LONG_MIN_VAR = "?longtminute";
    public static final String LONG_SEC_VAR = "?longsecond";
    public static final String LONG_MIL_SEC_VAR = "?longmilsec";
    public static final String ELEVATION_VAR = "?elev";
    public static final String TER_ELEVATION_VAR = "?terElev";
    public static final String REP_CONTEXT_VAR = "?repcontext";
    public static final String REP_SUBCONTEXT_VAR = "?subcontext";
    public static final String CONTEXT_REL_VAR = "?contextrelation";
    public static final String PROJECT_VAR = "?project";
    public static final String SPACE_DIMENSION_VAR = "?spacedimension";
    public static final String MODEL_PRECISION_VAR = "?modelprecision";
    public static final String NORTH_DIR_VAR = "?northdirection";
    public static final String MODEL_PLACEMENT_VAR = "?modelplacement";
    public static final String PLACEMENT_VAR = "?placement";
    // Geometry variables
    public static final String PRODUCT_DEFINITION_VAR = "?productDefinitionShape";
    public static final String SHAPE_REP_VAR = "?shaperep";
    public static final String INST_SHAPE_REP_VAR = "?instshaperep";
    public static final String INST_SHAPE_REP_TYPE_VAR = "?shapereptype";
    public static final String GEOM_VAR = "?geometry";
    public static final String GEOM_TYPE_VAR = "?geomtype";
    public static final String MAPPED_ITEM_VAR = "?mappeditem";
    public static final String REP_MAP_VAR = "?representationmap";
    public static final String GEOM_AXIS_PLACEMENT_VAR = "?geomaxisplacement";
    public static final String CART_TRANSFORMER_VAR = "?cartesiantransformer";
    // IfcOwl Properties
    public static final String EXPRESS_HASDOUBLE = "/express:hasDouble";
    public static final String EXPRESS_HASINTEGER = "/express:hasInteger";
    public static final String EXPRESS_HASSTRING = "/express:hasString";
    public static final String LIST_HAS_CONTENT = "list:hasContents";
    public static final String LIST_HAS_NEXT = "list:hasNext";
    public static final String IFC_ID = NamespaceMapper.IFC_PREFIX + ":globalId_IfcRoot";
    public static final String IFC_NAME = NamespaceMapper.IFC_PREFIX + ":name_IfcRoot";
    public static final String IFC_SITE_ELEV = NamespaceMapper.IFC_PREFIX + ":refElevation_IfcSite";
    public static final String IFC_BUILDING_ELEV = NamespaceMapper.IFC_PREFIX + ":elevationOfRefHeight_IfcBuilding";
    public static final String IFC_BUILDING_TERELEV = NamespaceMapper.IFC_PREFIX + ":elevationOfTerrain_IfcBuilding";
    public static final String IFC_STOREY_ELEV = NamespaceMapper.IFC_PREFIX + ":elevation_IfcBuildingStorey";
    public static final String IFC_PARENT_ZONE_REL = NamespaceMapper.IFC_PREFIX + ":relatingObject_IfcRelDecomposes";
    public static final String IFC_CHILD_ZONE_REL = NamespaceMapper.IFC_PREFIX + ":relatedObjects_IfcRelDecomposes";
    public static final String IFC_REL_ZONE = NamespaceMapper.IFC_PREFIX + ":relatingStructure_IfcRelContainedInSpatialStructure";
    public static final String IFC_REL_ELEMENT = NamespaceMapper.IFC_PREFIX + ":relatedElements_IfcRelContainedInSpatialStructure";
    public static final String IFC_REF_LAT = NamespaceMapper.IFC_PREFIX + ":refLatitude_IfcSite";
    public static final String IFC_REF_LONG = NamespaceMapper.IFC_PREFIX + ":refLongitude_IfcSite";
    public static final String IFC_PROJECT_NAME = NamespaceMapper.IFC_PREFIX + ":longName_IfcProject";
    public static final String IFC_PROJECT_PHASE = NamespaceMapper.IFC_PREFIX + ":phase_IfcProject";
    public static final String IFC_PROJECT_REP_CONTEXT = NamespaceMapper.IFC_PREFIX + ":representationContexts_IfcProject";
    public static final String IFC_PROJECT_COORD_DIM = NamespaceMapper.IFC_PREFIX + ":coordinateSpaceDimension_IfcGeometricRepresentationContext";
    public static final String IFC_PROJECT_CONTEXT_PRECISION = NamespaceMapper.IFC_PREFIX + ":precision_IfcGeometricRepresentationContext";
    public static final String IFC_PROJECT_WCS_CONTEXT = NamespaceMapper.IFC_PREFIX + ":worldCoordinateSystem_IfcGeometricRepresentationContext";
    public static final String IFC_PROJECT_TRUE_NORTH = NamespaceMapper.IFC_PREFIX + ":trueNorth_IfcGeometricRepresentationContext";
    // IfcOwl geometry properties
    public static final String IFC_PRODUCT_REPRESENTATION = NamespaceMapper.IFC_PREFIX + ":representation_IfcProduct";
    public static final String IFC_PRODUCT_REPRESENTATIONS = NamespaceMapper.IFC_PREFIX + ":representations_IfcProductRepresentation";
    public static final String IFC_PRODUCT_REPRESENTATION_TYPE = NamespaceMapper.IFC_PREFIX + ":representationType_IfcRepresentation";
    public static final String IFC_REP_CONTEXT = NamespaceMapper.IFC_PREFIX + ":contextOfItems_IfcRepresentation";
    public static final String IFC_REP_ITEMS = NamespaceMapper.IFC_PREFIX + ":items_IfcRepresentation";
    public static final String IFC_MAPPING_SOURCE = NamespaceMapper.IFC_PREFIX + ":mappingSource_IfcMappedItem";
    public static final String IFC_MAPPING_TARGET = NamespaceMapper.IFC_PREFIX + ":mappingTarget_IfcMappedItem";
    public static final String IFC_MAPPING_ORIGIN = NamespaceMapper.IFC_PREFIX + ":mappingOrigin_IfcRepresentationMap";
    public static final String IFC_MAPPED_REP = NamespaceMapper.IFC_PREFIX + ":mappedRepresentation_IfcRepresentationMap";
    public static final String IFC_OBJ_PLACEMENT = NamespaceMapper.IFC_PREFIX + ":objectPlacement_IfcProduct";
    // IfcOwl Classes
    public static final String IFCPROJECT = NamespaceMapper.IFC_PREFIX + ":IfcProject";
    public static final String IFCSITE = NamespaceMapper.IFC_PREFIX + ":IfcSite";
    public static final String IFCBUILDING = NamespaceMapper.IFC_PREFIX + ":IfcBuilding";
    public static final String IFCSTOREY_CLASS = "IfcBuildingStorey";
    public static final String IFCSTOREY = NamespaceMapper.IFC_PREFIX + ":" + IFCSTOREY_CLASS;
    public static final String IFCSPACE_CLASS = "IfcSpace";
    public static final String IFCSPACE = NamespaceMapper.IFC_PREFIX + ":" + IFCSPACE_CLASS;
    public static final String RELAGG = NamespaceMapper.IFC_PREFIX + ":IfcRelAggregates";
    public static final String REL_SPATIAL_ZONE_ELEMENT = NamespaceMapper.IFC_PREFIX + ":IfcRelContainedInSpatialStructure";
    public static final String IFCCOMPOUND_PLANE_ANGLE = NamespaceMapper.IFC_PREFIX + ":IfcCompoundPlaneAngleMeasure";
    public static final String IFCGEOM_REP_CONTEXT = NamespaceMapper.IFC_PREFIX + ":IfcGeometricRepresentationContext";
    public static final String IFCGEOM_REP_SUBCONTEXT = NamespaceMapper.IFC_PREFIX + ":IfcGeometricRepresentationSubContext";
    // IfcOwl geometry classes
    public static final String IFC_PRODUCT_DEF_SHAPE = NamespaceMapper.IFC_PREFIX + ":IfcProductDefinitionShape";
    public static final String IFC_SHAPE_REP = NamespaceMapper.IFC_PREFIX + ":IfcShapeRepresentation";
    public static final String IFC_MAPPED_ITEM = NamespaceMapper.IFC_PREFIX + ":IfcMappedItem";
    public static final String IFC_REP_MAP = NamespaceMapper.IFC_PREFIX + ":IfcRepresentationMap";
    public static final String IFCLOCALPLACEMENT = NamespaceMapper.IFC_PREFIX + ":IfcLocalPlacement";
    public static final String IFC_CART_TRANSFORMATION_OPERATOR = NamespaceMapper.IFC_PREFIX + ":IfcCartesianTransformationOperator3D";
    // IfcOwl Element Classes
    public static final String IFCDOOR = NamespaceMapper.IFC_PREFIX + ":IfcDoor";

    /**
     * Add the statements for querying common metadata such as class name, their unique ifc ID, and name into the builder.
     *
     * @param builder    A select builder object to append the statements to.
     * @param subjectVar The subject var, zone or element, for this builder.
     */
    public static void addBaseQueryComponents(SelectBuilder builder, String subjectVar) {
        builder.addVar(subjectVar)
                .addVar(UID_VAR)
                .addVar(NAME_VAR)
                .addVar(PLACEMENT_VAR);
        builder.addWhere(subjectVar, IFC_ID + EXPRESS_HASSTRING, UID_VAR)
                .addWhere(subjectVar, IFC_NAME + EXPRESS_HASSTRING, NAME_VAR)
                .addWhere(subjectVar, IFC_OBJ_PLACEMENT, PLACEMENT_VAR)
                .addWhere(PLACEMENT_VAR, QueryHandler.RDF_TYPE, IFCLOCALPLACEMENT);
    }

    /**
     * Add the statements for querying common metadata such as class name, their unique ifc ID, and name into the builder.
     *
     * @param builder A select builder object to append the statements to.
     */
    public static void addElementModelRepresentationQueryComponents(SelectBuilder builder) {
        builder.addVar(INST_SHAPE_REP_VAR)
                .addVar(REP_SUBCONTEXT_VAR)
                .addVar(GEOM_VAR)
                .addVar(GEOM_TYPE_VAR)
                .addVar(INST_SHAPE_REP_TYPE_VAR)
                .addVar(GEOM_AXIS_PLACEMENT_VAR)
                .addVar(CART_TRANSFORMER_VAR);
        builder.addWhere(ZONE_VAR, IFC_PRODUCT_REPRESENTATION, PRODUCT_DEFINITION_VAR)
                .addWhere(PRODUCT_DEFINITION_VAR, QueryHandler.RDF_TYPE, IFC_PRODUCT_DEF_SHAPE);
        // Set up empty builders for subgroups and unions with necessary prefixes
        SelectBuilder subgroupBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(subgroupBuilder);
        SelectBuilder unionBuilder = subgroupBuilder.clone();
        // Query for the individual shape representation instances that are directly linked to specific geometries
        subgroupBuilder.addWhere(PRODUCT_DEFINITION_VAR, IFC_PRODUCT_REPRESENTATIONS + "/" + LIST_HAS_CONTENT, INST_SHAPE_REP_VAR)
                .addWhere(INST_SHAPE_REP_VAR, QueryHandler.RDF_TYPE, IFC_SHAPE_REP)
                .addWhere(INST_SHAPE_REP_VAR, IFC_PRODUCT_REPRESENTATION_TYPE + EXPRESS_HASSTRING, INST_SHAPE_REP_TYPE_VAR)
                .addWhere(INST_SHAPE_REP_VAR, IFC_REP_CONTEXT, REP_SUBCONTEXT_VAR)
                .addWhere(REP_SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, IFCGEOM_REP_SUBCONTEXT)
                .addWhere(INST_SHAPE_REP_VAR, IFC_REP_ITEMS, GEOM_VAR)
                .addWhere(GEOM_VAR, QueryHandler.RDF_TYPE, GEOM_TYPE_VAR);
        // For the first sub-query, this should apply to all geom type except for IfcMappedItem
        try {
            subgroupBuilder.addFilter("!regex(str(" + GEOM_TYPE_VAR + ") ,'IfcMappedItem')");
        } catch (ParseException e) {
            throw new JPSRuntimeException(e);
        }

        // Query for the family representation instance when there is no geometry available for individual instances
        // In these cases, individual instances are linked as a mapped item and are nested from the individual instances
        unionBuilder.addWhere(PRODUCT_DEFINITION_VAR, IFC_PRODUCT_REPRESENTATIONS + "/" + LIST_HAS_CONTENT, SHAPE_REP_VAR)
                .addWhere(SHAPE_REP_VAR, QueryHandler.RDF_TYPE, IFC_SHAPE_REP)
                .addWhere(SHAPE_REP_VAR, IFC_PRODUCT_REPRESENTATION_TYPE + EXPRESS_HASSTRING, Converters.makeLiteral("MappedRepresentation"))
                .addWhere(SHAPE_REP_VAR, IFC_REP_ITEMS, MAPPED_ITEM_VAR) // individual instances with no geometries
                .addWhere(MAPPED_ITEM_VAR, QueryHandler.RDF_TYPE, IFC_MAPPED_ITEM)
                .addWhere(MAPPED_ITEM_VAR, IFC_MAPPING_SOURCE, REP_MAP_VAR)
                .addWhere(MAPPED_ITEM_VAR, IFC_MAPPING_TARGET, CART_TRANSFORMER_VAR) // Transforms origin location to new location
                .addWhere(CART_TRANSFORMER_VAR, QueryHandler.RDF_TYPE, IFC_CART_TRANSFORMATION_OPERATOR)
                .addWhere(REP_MAP_VAR, QueryHandler.RDF_TYPE, IFC_REP_MAP)
                // Source placement structure is directly queried here as there is no IfcLocalPlacement instance in this specific case
                .addWhere(REP_MAP_VAR, IFC_MAPPING_ORIGIN, GEOM_AXIS_PLACEMENT_VAR)
                // Start of specific instance shape rep
                .addWhere(REP_MAP_VAR, IFC_MAPPED_REP, INST_SHAPE_REP_VAR)
                .addWhere(INST_SHAPE_REP_VAR, QueryHandler.RDF_TYPE, IFC_SHAPE_REP)
                .addWhere(INST_SHAPE_REP_VAR, IFC_PRODUCT_REPRESENTATION_TYPE + EXPRESS_HASSTRING, INST_SHAPE_REP_TYPE_VAR)
                .addWhere(INST_SHAPE_REP_VAR, IFC_REP_CONTEXT, REP_SUBCONTEXT_VAR) // Sub-context
                .addWhere(REP_SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, IFCGEOM_REP_SUBCONTEXT)
                .addWhere(INST_SHAPE_REP_VAR, IFC_REP_ITEMS, GEOM_VAR) // Geometries
                .addWhere(GEOM_VAR, QueryHandler.RDF_TYPE, GEOM_TYPE_VAR);
        subgroupBuilder.addUnion(unionBuilder);
        builder.addWhere(subgroupBuilder);
    }
}
