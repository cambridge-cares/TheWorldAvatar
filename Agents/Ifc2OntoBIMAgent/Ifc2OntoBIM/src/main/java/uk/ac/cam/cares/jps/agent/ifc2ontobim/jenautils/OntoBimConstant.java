package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils;

public class OntoBimConstant {
    public static final String BACKSLASH = "/";
    public static final String HASH = "#";

    // restrict instantiation
    private OntoBimConstant() {
    }

    // General symbols
    public static final String UNDERSCORE = "_";

    // Generic properties
    public static final String RDF_TYPE = NamespaceMapper.RDF_NAMESPACE + "type";
    public static final String RDFS_LABEL = NamespaceMapper.RDFS_NAMESPACE + "label";

    // Quantity properties
    public static final String OM_HAS_VALUE = NamespaceMapper.OM_NAMESPACE + "hasValue";
    public static final String OM_HAS_NUMERICAL_VALUE = NamespaceMapper.OM_NAMESPACE + "hasNumericalValue";
    public static final String OM_HAS_UNIT = NamespaceMapper.OM_NAMESPACE + "hasUnit";
    public static final String SKOS_NOTATION = NamespaceMapper.SKOS_NAMESPACE + "notation";

    // OntoBim properties
    public static final String BIM_HAS_REF_ELEVATION = NamespaceMapper.BIM_NAMESPACE + "hasRefElevation";
    public static final String BIM_HAS_TER_ELEVATION = NamespaceMapper.BIM_NAMESPACE + "hasTerrainElevation";
    public static final String BIM_HAS_IFC_REPRESENTATION = NamespaceMapper.BIM_NAMESPACE + "hasIfcRepresentation";
    public static final String BOT_HAS_BUILDING = NamespaceMapper.BOT_NAMESPACE + "hasBuilding";
    public static final String BOT_HAS_STOREY = NamespaceMapper.BOT_NAMESPACE + "hasStorey";
    public static final String BIM_HAS_ROOM = NamespaceMapper.BIM_NAMESPACE + "hasRoom";
    public static final String BIM_HAS_ID = NamespaceMapper.BIM_NAMESPACE + "hasIfcId";
    public static final String BIM_HAS_LAT = NamespaceMapper.BIM_NAMESPACE + "hasRefLatitude";
    public static final String BIM_HAS_LONG = NamespaceMapper.BIM_NAMESPACE + "hasRefLongitude";
    public static final String BIM_HAS_DEGREE = NamespaceMapper.BIM_NAMESPACE + "hasDegree";
    public static final String BIM_HAS_MINUTE = NamespaceMapper.BIM_NAMESPACE + "hasMinute";
    public static final String BIM_HAS_SEC = NamespaceMapper.BIM_NAMESPACE + "hasSecond";
    public static final String BIM_HAS_MILSEC = NamespaceMapper.BIM_NAMESPACE + "hasMillionthSecond";
    // OntoBim project/ model properties
    public static final String BIM_HAS_PHASE = NamespaceMapper.BIM_NAMESPACE + "hasPhase";
    public static final String BIM_HAS_ROOT_ZONE = NamespaceMapper.BIM_NAMESPACE + "hasRootZone";
    public static final String BIM_HAS_CONTEXT = NamespaceMapper.BIM_NAMESPACE + "hasContext";
    public static final String BIM_HAS_SPACE_DIMENSION = NamespaceMapper.BIM_NAMESPACE + "hasSpaceDimensions";
    public static final String BIM_HAS_PRECISION = NamespaceMapper.BIM_NAMESPACE + "hasPrecision";
    public static final String BIM_HAS_TRUE_NORTH= NamespaceMapper.BIM_NAMESPACE + "hasTrueNorth";
    public static final String BIM_HAS_WCS = NamespaceMapper.BIM_NAMESPACE + "hasWorldCoordinateSystem";
    // OntoBIM modelling position properties
    public static final String BIM_HAS_LOCAL_POSITION = NamespaceMapper.BIM_NAMESPACE + "hasLocalPosition";
    // OntoBim classes
    public static final String PROJECT_CLASS = "IfcProjectRepresentation";
    public static final String BIM_PROJECT_CLASS = NamespaceMapper.BIM_NAMESPACE + PROJECT_CLASS;
    public static final String GEOM_CONTEXT_CLASS = "GeometricRepresentationContext";
    public static final String BIM_GEOM_CONTEXT_CLASS = NamespaceMapper.BIM_NAMESPACE + GEOM_CONTEXT_CLASS;
    public static final String SITE_CLASS = "Site";
    public static final String BUILDING_CLASS = "Building";
    public static final String STOREY_CLASS = "Storey";
    public static final String ROOM_CLASS = "Room";
    public static final String BOT_SITE_CLASS = NamespaceMapper.BOT_NAMESPACE + SITE_CLASS;
    public static final String BOT_BUILDING_CLASS = NamespaceMapper.BOT_NAMESPACE + BUILDING_CLASS;
    public static final String BOT_STOREY_CLASS = NamespaceMapper.BOT_NAMESPACE + STOREY_CLASS;
    public static final String BIM_ROOM_CLASS = NamespaceMapper.BIM_NAMESPACE + ROOM_CLASS;
    public static final String SITE_REP_CLASS = "IfcSiteRepresentation";
    public static final String BUILDING_REP_CLASS = "IfcBuildingRepresentation";
    public static final String STOREY_REP_CLASS = "IfcStoreyRepresentation";
    public static final String ROOM_REP_CLASS = "IfcRoomRepresentation";
    public static final String BIM_SITE_REP_CLASS = NamespaceMapper.BIM_NAMESPACE + SITE_REP_CLASS;
    public static final String BIM_BUILDING_REP_CLASS = NamespaceMapper.BIM_NAMESPACE + BUILDING_REP_CLASS;
    public static final String BIM_STOREY_REP_CLASS = NamespaceMapper.BIM_NAMESPACE + STOREY_REP_CLASS;
    public static final String BIM_ROOM_REP_CLASS = NamespaceMapper.BIM_NAMESPACE + ROOM_REP_CLASS;
    public static final String BIM_COMPOUND_PLANE_ANGLE = NamespaceMapper.BIM_NAMESPACE + "CompoundPlaneAngle";
    // OntoBim geometry classes
    public static final String DIR_VEC_CLASS = "DirectionVector";
    public static final String BIM_DIR_VEC_CLASS = NamespaceMapper.BIM_NAMESPACE + DIR_VEC_CLASS;
    public static final String LOCAL_PLACEMENT_CLASS = "LocalPlacement";
    public static final String BIM_LOCAL_PLACEMENT_CLASS = NamespaceMapper.BIM_NAMESPACE + LOCAL_PLACEMENT_CLASS;
    // OM classes
    public static final String HEIGHT_CLASS = NamespaceMapper.OM_NAMESPACE + "Height";
    public static final String MEASURE_CLASS = NamespaceMapper.OM_NAMESPACE + "Measure";
    public static final String LENGTH_CLASS = NamespaceMapper.OM_NAMESPACE + "Length";
}
