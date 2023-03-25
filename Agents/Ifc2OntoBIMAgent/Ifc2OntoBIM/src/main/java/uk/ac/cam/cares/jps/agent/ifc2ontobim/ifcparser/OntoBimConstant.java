package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;

public class OntoBimConstant {
    public static final String BACKSLASH = "/";
    public static final String HASH = "#";
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
    public static final String BOT_CONTAINS_ELEMENT = NamespaceMapper.BOT_NAMESPACE + "containsElement";
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
    // OntoBIM element geometry properties
    public static final String BIM_HAS_GEOM_REP = NamespaceMapper.BIM_NAMESPACE + "hasGeometricRepresentation";
    public static final String BIM_HAS_REP_TYPE = NamespaceMapper.BIM_NAMESPACE + "hasRepresentationType";
    public static final String BIM_HAS_SUBCONTEXT = NamespaceMapper.BIM_NAMESPACE + "hasSubContext";
    public static final String BIM_HAS_REP_ITEM = NamespaceMapper.BIM_NAMESPACE + "hasRepresentationItem";
    public static final String BIM_HAS_SOURCE_PLACEMENT = NamespaceMapper.BIM_NAMESPACE + "hasSourcePlacement";
    public static final String BIM_HAS_TARGET_PLACEMENT = NamespaceMapper.BIM_NAMESPACE + "hasTargetPlacement";
    public static final String BIM_HAS_VOID = NamespaceMapper.BIM_NAMESPACE + "hasVoid";
    public static final String BIM_HAS_VOID_TYPE = NamespaceMapper.BIM_NAMESPACE + "hasVoidType";
    public static final String BUILDING_STRUCTURE_CONSISTS_OF = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + "consistsOf";
    // OntoBuildingStructure stair properties
    public static final String BUILDING_STRUCTURE_HAS_RISER_NUM = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + "hasNumOfRiser";
    public static final String BUILDING_STRUCTURE_HAS_TREAD_NUM = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + "hasNumOfTread";
    public static final String BUILDING_STRUCTURE_HAS_RISER_HEIGHT = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + "hasRiserHeight";
    public static final String BUILDING_STRUCTURE_HAS_TREAD_LENGTH = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + "hasTreadLength";
    // OntoBIM modelling position properties
    public static final String BIM_HAS_LOCAL_POSITION = NamespaceMapper.BIM_NAMESPACE + "hasLocalPosition";
    public static final String BIM_HAS_REL_POSITION = NamespaceMapper.BIM_NAMESPACE + "hasRelativePositionTo";
    public static final String BIM_HAS_REF_POINT = NamespaceMapper.BIM_NAMESPACE + "hasRefPoint";
    public static final String BIM_HAS_X_COORDINATE = NamespaceMapper.BIM_NAMESPACE + "hasXCoordinate";
    public static final String BIM_HAS_Y_COORDINATE = NamespaceMapper.BIM_NAMESPACE + "hasYCoordinate";
    public static final String BIM_HAS_Z_COORDINATE = NamespaceMapper.BIM_NAMESPACE + "hasZCoordinate";
    public static final String BIM_HAS_REF_DIRECTION = NamespaceMapper.BIM_NAMESPACE + "hasRefDirection";
    public static final String BIM_HAS_AXIS_DIRECTION = NamespaceMapper.BIM_NAMESPACE + "hasAxisDirection";
    public static final String BIM_HAS_X_DIR_RATIO = NamespaceMapper.BIM_NAMESPACE + "hasXDirectionRatio";
    public static final String BIM_HAS_Y_DIR_RATIO = NamespaceMapper.BIM_NAMESPACE + "hasYDirectionRatio";
    public static final String BIM_HAS_Z_DIR_RATIO = NamespaceMapper.BIM_NAMESPACE + "hasZDirectionRatio";
    public static final String BIM_HAS_LOCAL_ORIGIN = NamespaceMapper.BIM_NAMESPACE + "hasLocalOrigin";
    public static final String BIM_HAS_SCALE = NamespaceMapper.BIM_NAMESPACE + "hasScale";
    public static final String BIM_HAS_DERIVED_X_AXIS = NamespaceMapper.BIM_NAMESPACE + "hasDerivedXAxis";
    public static final String BIM_HAS_DERIVED_Y_AXIS = NamespaceMapper.BIM_NAMESPACE + "hasDerivedYAxis";

    // OntoBim classes
    public static final String PROJECT_CLASS = "IfcProjectRepresentation";
    public static final String BIM_PROJECT_CLASS = NamespaceMapper.BIM_NAMESPACE + PROJECT_CLASS;
    public static final String GEOM_CONTEXT_CLASS = "GeometricRepresentationContext";
    public static final String BIM_GEOM_CONTEXT_CLASS = NamespaceMapper.BIM_NAMESPACE + GEOM_CONTEXT_CLASS;
    public static final String GEOM_SUB_CONTEXT_CLASS = "GeometricRepresentationSubContext";
    public static final String BIM_GEOM_SUB_CONTEXT_CLASS = NamespaceMapper.BIM_NAMESPACE + GEOM_SUB_CONTEXT_CLASS;
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
    // OntoBim element classes
    public static final String ASSET_MODEL_REP_CLASS = "IfcModelRepresentation";
    public static final String BIM_ASSET_MODEL_REP_CLASS = NamespaceMapper.BIM_NAMESPACE + ASSET_MODEL_REP_CLASS;
    public static final String CEILING_CLASS = "Ceiling";
    public static final String BIM_CEILING_CLASS = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + CEILING_CLASS;
    public static final String COLUMN_CLASS = "Column";
    public static final String BIM_COLUMN_CLASS = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + COLUMN_CLASS;
    public static final String DOOR_CLASS = "Door";
    public static final String BIM_DOOR_CLASS = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + DOOR_CLASS;
    public static final String FLOOR_CLASS = "Floor";
    public static final String BIM_FLOOR_CLASS = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + FLOOR_CLASS;
    public static final String ROOF_CLASS = "Roof";
    public static final String BIM_ROOF_CLASS = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + ROOF_CLASS;
    public static final String STAIR_CLASS = "Stair";
    public static final String BIM_STAIR_CLASS = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + STAIR_CLASS;
    public static final String STAIR_FLIGHT_CLASS = "StairFlight";
    public static final String BIM_STAIR_FLIGHT_CLASS = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + STAIR_FLIGHT_CLASS;
    public static final String STAIR_LANDING_CLASS = "Landing";
    public static final String BIM_STAIR_LANDING_CLASS = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + STAIR_LANDING_CLASS;
    public static final String STAIR_RAILING_CLASS = "Railing";
    public static final String BIM_STAIR_RAILING_CLASS = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + STAIR_RAILING_CLASS;
    public static final String STAIR_STRUCT_COMP_CLASS = "StairStructuralComponent";
    public static final String BIM_STAIR_STRUCT_COMP_CLASS = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + STAIR_STRUCT_COMP_CLASS;
    public static final String WALL_CLASS = "Wall";
    public static final String BIM_WALL_CLASS = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + WALL_CLASS;
    public static final String WINDOW_CLASS = "Window";
    public static final String BIM_WINDOW_CLASS = NamespaceMapper.BUILDING_STRUCTURE_NAMESPACE + WINDOW_CLASS;
    // OntoBim geometry classes
    public static final String GEOM_MODEL_REP_CLASS = "ModelRepresentation3D";
    public static final String BIM_GEOM_MODEL_REP_CLASS = NamespaceMapper.BIM_NAMESPACE + GEOM_MODEL_REP_CLASS;
    public static final String GEOM_VOID_CLASS = "GeometricVoid";
    public static final String BIM_GEOM_VOID_CLASS = NamespaceMapper.BIM_NAMESPACE + GEOM_VOID_CLASS;
    public static final String DIR_VEC_CLASS = "DirectionVector";
    public static final String BIM_DIR_VEC_CLASS = NamespaceMapper.BIM_NAMESPACE + DIR_VEC_CLASS;
    public static final String CART_TRANS_OPERATOR_CLASS = "CartesianTransformationOperator";
    public static final String BIM_CART_TRANS_OPERATOR_CLASS = NamespaceMapper.BIM_NAMESPACE + CART_TRANS_OPERATOR_CLASS;
    public static final String LOCAL_PLACEMENT_CLASS = "LocalPlacement";
    public static final String BIM_LOCAL_PLACEMENT_CLASS = NamespaceMapper.BIM_NAMESPACE + LOCAL_PLACEMENT_CLASS;
    public static final String CARTESIAN_POINT_CLASS = "CartesianPoint";
    public static final String BIM_CARTESIAN_POINT_CLASS = NamespaceMapper.BIM_NAMESPACE + CARTESIAN_POINT_CLASS;
    // OM classes
    public static final String HEIGHT_CLASS = NamespaceMapper.OM_NAMESPACE + "Height";
    public static final String MEASURE_CLASS = NamespaceMapper.OM_NAMESPACE + "Measure";
    public static final String LENGTH_CLASS = NamespaceMapper.OM_NAMESPACE + "Length";

    // restrict instantiation
    private OntoBimConstant() {
    }
}
