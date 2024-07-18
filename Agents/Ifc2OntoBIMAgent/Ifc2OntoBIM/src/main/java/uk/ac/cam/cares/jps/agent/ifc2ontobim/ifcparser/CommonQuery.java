package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.Converters;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.QueryHandler;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * Provides reusable query statements and common fields for IfcOwl queries.
 *
 * @author qhouyee
 */
public class CommonQuery {
    public static final String UID_VAR = "?uid";
    public static final String NAME_VAR = "?name";
    public static final String ZONE_VAR = "?zone";
    public static final String ELEMENT_VAR = "?element";
    public static final String PARENT_ZONE_VAR = "?subzone";
    public static final String PHASE_VAR = "?phase";
    public static final String POSTAL_ADDRESS_VAR = "?postalAddress";
    public static final String POSTAL_ADDRESS_LINES_VAR = "?postalAddressLines";
    public static final String POSTAL_TOWN_VAR = "?postalTown";
    public static final String POSTAL_REGION_VAR = "?postalRegion";
    public static final String POSTAL_COUNTRY_VAR = "?postalCountry";
    public static final String POSTAL_CODE_VAR = "?postalCode";
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
    public static final String REP_SEC_SUBCONTEXT_VAR = "?secondsubcontext";
    public static final String VOID_SUB_CONTEXT_VAR = "?voidsubcontext";
    public static final String CONTEXT_REL_VAR = "?contextrelation";
    public static final String PROJECT_VAR = "?project";
    public static final String SPACE_DIMENSION_VAR = "?spacedimension";
    public static final String MODEL_PRECISION_VAR = "?modelprecision";
    public static final String NORTH_DIR_VAR = "?northdirection";
    public static final String MODEL_PLACEMENT_VAR = "?modelplacement";
    public static final String PLACEMENT_VAR = "?placement";
    // Element supplementary variables
    public static final String REL_TYPE_DEFINITION_VAR = "?reltypedefine";
    public static final String ELEMENT_TYPE_VAR = "?elementtype";
    public static final String ASSEMBLY_VAR = "?assemblyelement";
    public static final String OPENING_ELEMENT_VAR = "?openingelement";
    public static final String REL_FILLS_ELEMENT_VAR = "?relfillselement";
    public static final String REL_VOID_ELEMENT_VAR = "?relvoidelement";
    public static final String REL_SPATIAL_STRUCTURE_VAR = "?relspatialstructure";
    public static final String REL_AGG_STAIR__VAR = "?relaggstair";
    public static final String ROOF_SLAB_VAR = "?roofslab";
    public static final String SLAB_ENUM_VAR = "?slabenum";
    public static final String STAIRFLIGHT_VAR = "?stairflight";
    public static final String LANDING_VAR = "?landing";
    public static final String RAILING_VAR = "?railing";
    public static final String STAIR_STRUCTURAL_COMPONENT_VAR = "?stairstructurecomponent";
    public static final String STAIR_RISER_NUM_VAR = "?riserno";
    public static final String STAIR_TREAD_NUM_VAR = "?treadno";
    public static final String STAIR_RISER_HEIGHT_VAR = "?riserheight";
    public static final String STAIR_TREAD_LENGTH_VAR = "?treadlength";
    // Stair shape representation
    public static final String STAIR_FLIGHT_UID_VAR = "?stairflightuid";
    public static final String STAIR_FLIGHT_NAME_VAR = "?stairflightname";
    public static final String STAIR_FLIGHT_PLACEMENT_VAR = "?stairflightplacement";
    public static final String STAIR_FLIGHT_PRODUCT_DEFINITION_VAR = "?stairflightproductDefinition";
    public static final String STAIR_FLIGHT_SHAPE_REP_VAR = "?stairflightshaperep";
    public static final String STAIR_FLIGHT_REP_TYPE_VAR = "?stairflightshapereptype";
    public static final String STAIR_FLIGHT_SUB_CONTEXT_VAR = "?stairflightsubcontext";
    public static final String STAIR_FLIGHT_GEOM_VAR = "?stairflightgeometry";
    public static final String STAIR_FLIGHT_GEOM_TYPE_VAR = "?stairflightgeomtype";
    public static final String STAIR_LANDING_UID_VAR = "?stairlandinguid";
    public static final String STAIR_LANDING_NAME_VAR = "?stairlandingname";
    public static final String STAIR_LANDING_PLACEMENT_VAR = "?stairlandingplacement";
    public static final String STAIR_LANDING_PRODUCT_DEFINITION_VAR = "?stairlandingproductDefinition";
    public static final String STAIR_LANDING_SHAPE_REP_VAR = "?stairlandingshaperep";
    public static final String STAIR_LANDING_REP_TYPE_VAR = "?stairlandingshapereptype";
    public static final String STAIR_LANDING_SUB_CONTEXT_VAR = "?stairlandingsubcontext";
    public static final String STAIR_LANDING_GEOM_VAR = "?stairlandinggeometry";
    public static final String STAIR_LANDING_GEOM_TYPE_VAR = "?stairlandinggeomtype";
    public static final String STAIR_RAILING_UID_VAR = "?stairrailinguid";
    public static final String STAIR_RAILING_NAME_VAR = "?stairrailingname";
    public static final String STAIR_RAILING_PLACEMENT_VAR = "?stairrailingplacement";
    public static final String STAIR_RAILING_PRODUCT_DEFINITION_VAR = "?stairrailingproductDefinition";
    public static final String STAIR_RAILING_SHAPE_REP_VAR = "?stairrailingshaperep";
    public static final String STAIR_RAILING_REP_TYPE_VAR = "?stairrailingshapereptype";
    public static final String STAIR_RAILING_SUB_CONTEXT_VAR = "?stairrailingsubcontext";
    public static final String STAIR_RAILING_GEOM_VAR = "?stairrailinggeometry";
    public static final String STAIR_RAILING_GEOM_TYPE_VAR = "?stairrailinggeomtype";
    public static final String STAIR_STRUCT_COMP_UID_VAR = "?stairmemberuid";
    public static final String STAIR_STRUCT_COMP_NAME_VAR = "?stairmembername";
    public static final String STAIR_STRUCT_COMP_PLACEMENT_VAR = "?stairmemberplacement";
    public static final String STAIR_STRUCT_COMP_PRODUCT_DEFINITION_VAR = "?stairmemberproductDefinition";
    public static final String STAIR_STRUCT_COMP_SHAPE_REP_VAR = "?stairmembershaperep";
    public static final String STAIR_STRUCT_COMP_REP_TYPE_VAR = "?stairmembershapereptype";
    public static final String STAIR_STRUCT_COMP_SUB_CONTEXT_VAR = "?stairmembersubcontext";
    public static final String STAIR_STRUCT_COMP_GEOM_VAR = "?stairmembergeometry";
    public static final String STAIR_STRUCT_COMP_GEOM_TYPE_VAR = "?stairmembergeomtype";
    // Geometry variables
    public static final String PRODUCT_DEFINITION_VAR = "?productDefinitionShape";
    public static final String SHAPE_REP_VAR = "?shaperep";
    public static final String INST_SHAPE_REP_VAR = "?instshaperep";
    public static final String INST_SHAPE_REP_SEC_VAR = "?secondinstshaperep";
    public static final String INST_SHAPE_REP_TYPE_VAR = "?shapereptype";
    public static final String INST_SHAPE_REP_TYPE_SEC_VAR = "?secshapereptype";
    public static final String VOID_REP_TYPE_VAR = "?voidshapereptype";
    public static final String VOID_PRODUCT_DEFINITION_VAR = "?voidproductDefinition";
    public static final String VOID_SHAPE_REP_VAR = "?voidshaperep";
    public static final String VOID_PLACEMENT_VAR = "?voidplacement";
    public static final String VOID_TYPE_VAR = "?voidtype";
    public static final String GEOM_VAR = "?geometry";
    public static final String GEOM_SEC_VAR = "?secgeometry";
    public static final String VOID_GEOM_VAR = "?voidgeometry";
    public static final String GEOM_TYPE_VAR = "?geomtype";
    public static final String GEOM_TYPE_SEC_VAR = "?secgeomtype";
    public static final String VOID_GEOM_TYPE_VAR = "?voidgeomtype";
    public static final String MAPPED_ITEM_VAR = "?mappeditem";
    public static final String REP_MAP_VAR = "?representationmap";
    public static final String GEOM_AXIS_PLACEMENT_VAR = "?geomaxisplacement";
    public static final String CART_TRANSFORMER_VAR = "?cartesiantransformer";
    public static final String BREP_CLOSED_SHELL_VAR = "?closedshell";
    public static final String BREP_FACE_VAR = "?face";
    public static final String BREP_FACE_OUTER_BOUND_VAR = "?faceouterbound";
    public static final String DEPTH_VAR = "?depth";
    public static final String EXTRUDED_DIRECTION_VAR = "?extrudeddirection";
    public static final String PROFILE_DEF_VAR = "?profiledefinition";
    public static final String PROFILE_DEF_CLASS_VAR = "?profiledefinitionclass";
    public static final String PROFILE_DEF_TYPE_VAR = "?profiledefinitiontype";
    public static final String PROFILE_DEF_CART_POINT_VAR = "?profilecartesianpoint";
    public static final String SURFACE_VAR = "?surface";
    public static final String BOOLEAN_VAR = "?boolean";
    public static final String SEC_CART_POINT_VAR = "?secpoint";
    public static final String SEC_REF_DIRECTION_VAR = "?secrefdirection";
    public static final String SEC_AXIS_DIRECTION_VAR = "?secaxisdirection";
    public static final String POLYLINE_VAR = "?polyline";
    public static final String FIRST_LINE_VERTEX_VAR = "?firstlinevertex";
    public static final String LINE_VERTEX_VAR = "?linevertex";
    public static final String NEXT_LINE_VERTEX_VAR = "?nextlinevertex";
    public static final String OPERATOR_VAR = "?operator";
    public static final String FIRST_OPERAND_VAR = "?firstoperand";
    public static final String SEC_OPERAND_VAR = "?secoperand";
    // Modelling operator variables
    public static final String REL_PLACEMENT_VAR = "?relativeplacement";
    public static final String REF_DIR_VECTOR_VAR = "?refdirection";
    public static final String AXIS_DIR_VECTOR_VAR = "?axisdirection";
    public static final String CART_POINT_VAR = "?cartesianpoint";
    public static final String DIR_VECTOR_VAR = "?directionvector";
    public static final String X_VALUE_VAR = "?xvalue";
    public static final String Y_VALUE_VAR = "?yvalue";
    public static final String Z_VALUE_VAR = "?zvalue";
    public static final String X_LIST_VAR = "?xlist";
    public static final String Y_LIST_VAR = "?ylist";
    public static final String X_AXIS_DIR_VECTOR_VAR = "?xaxisdirection";
    public static final String Y_AXIS_DIR_VECTOR_VAR = "?yaxisdirection";
    public static final String SCALE_VAR = "?scale";
    public static final String CONTEXT_TYPE_VAR = "?contexttype";
    public static final String CONTEXT_IDENTIFIER_VAR = "?contextidentifier";
    public static final String CONTEXT_VIEW_VAR = "?contextview";
    // IfcOwl Properties
    public static final String EXPRESS_HASDOUBLE = "/express:hasDouble";
    public static final String EXPRESS_HASINTEGER = "/express:hasInteger";
    public static final String EXPRESS_HASSTRING = "/express:hasString";
    public static final String EXPRESS_HASBOOLEAN = "/express:hasBoolean";
    public static final String LIST_HAS_CONTENT = "list:hasContents";
    public static final String LIST_HAS_NEXT = "list:hasNext";
    public static final String IFC_ID = NamespaceMapper.IFC_PREFIX + ":globalId_IfcRoot";
    public static final String IFC_NAME = NamespaceMapper.IFC_PREFIX + ":name_IfcRoot";
    public static final String IFC_SITE_ELEV = NamespaceMapper.IFC_PREFIX + ":refElevation_IfcSite";
    public static final String IFC_BUILDING_ADDRESS = NamespaceMapper.IFC_PREFIX + ":buildingAddress_IfcBuilding";
    public static final String IFC_BUILDING_ADDRESS_LINES = NamespaceMapper.IFC_PREFIX + ":addressLines_IfcPostalAddress";
    public static final String IFC_BUILDING_TOWN = NamespaceMapper.IFC_PREFIX + ":town_IfcPostalAddress";
    public static final String IFC_BUILDING_REGION = NamespaceMapper.IFC_PREFIX + ":region_IfcPostalAddress";
    public static final String IFC_BUILDING_POSTAL_CODE = NamespaceMapper.IFC_PREFIX + ":postalCode_IfcPostalAddress";
    public static final String IFC_BUILDING_COUNTRY = NamespaceMapper.IFC_PREFIX + ":country_IfcPostalAddress";
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
    // IfcOwl element supplementary properties
    public static final String IFC_RELATED_OBJECT = NamespaceMapper.IFC_PREFIX + ":relatedObjects_IfcRelDefines";
    public static final String IFC_RELATING_TYPE = NamespaceMapper.IFC_PREFIX + ":relatingType_IfcRelDefinesByType";
    public static final String IFC_PREDEFINED_TYPE_COVERING = NamespaceMapper.IFC_PREFIX + ":predefinedType_IfcCoveringType";
    public static final String IFC_PREDEFINED_TYPE_SLAB = NamespaceMapper.IFC_PREFIX + ":predefinedType_IfcSlabType";
    public static final String IFC_REL_FILLS_SUB_ELEMENT = NamespaceMapper.IFC_PREFIX + ":relatedBuildingElement_IfcRelFillsElement";
    public static final String IFC_REL_FILLS_OPENING = NamespaceMapper.IFC_PREFIX + ":relatingOpeningElement_IfcRelFillsElement";
    public static final String IFC_REL_VOIDS_OPENING = NamespaceMapper.IFC_PREFIX + ":relatedOpeningElement_IfcRelVoidsElement";
    public static final String IFC_REL_VOIDS_ASSEMBLY = NamespaceMapper.IFC_PREFIX + ":relatingBuildingElement_IfcRelVoidsElement";
    public static final String IFC_STAIR_RISER_NUM = NamespaceMapper.IFC_PREFIX + ":numberOfRiser_IfcStairFlight";
    public static final String IFC_STAIR_TREAD_NUM = NamespaceMapper.IFC_PREFIX + ":numberOfTreads_IfcStairFlight";
    public static final String IFC_STAIR_RISER_HEIGHT = NamespaceMapper.IFC_PREFIX + ":riserHeight_IfcStairFlight";
    public static final String IFC_STAIR_TREAD_LENGTH = NamespaceMapper.IFC_PREFIX + ":treadLength_IfcStairFlight";
    // IfcOwl geometry properties
    public static final String IFC_OBJ_TYPE = NamespaceMapper.IFC_PREFIX + ":objectType_IfcObject";
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
    public static final String IFC_BREP_OUTER = NamespaceMapper.IFC_PREFIX + ":outer_IfcManifoldSolidBrep";
    public static final String IFC_BREP_FACES = NamespaceMapper.IFC_PREFIX + ":cfsFaces_IfcConnectedFaceSet";
    public static final String IFC_BREP_FACE_BOUNDS = NamespaceMapper.IFC_PREFIX + ":bounds_IfcFace";
    public static final String IFC_BREP_FACE_BOUNDARY = NamespaceMapper.IFC_PREFIX + ":bound_IfcFaceBound";
    public static final String IFC_BREP_FACE_ORIENTATION = NamespaceMapper.IFC_PREFIX + ":orientation_IfcFaceBound";
    public static final String IFC_SWEPT_AREA_POSITION = NamespaceMapper.IFC_PREFIX + ":position_IfcSweptAreaSolid";
    public static final String IFC_EXTRUDED_DIRECTION = NamespaceMapper.IFC_PREFIX + ":extrudedDirection_IfcExtrudedAreaSolid";
    public static final String IFC_EXTRUDED_DEPTH = NamespaceMapper.IFC_PREFIX + ":depth_IfcExtrudedAreaSolid";
    public static final String IFC_EXTRUDED_PROFILE_AREA = NamespaceMapper.IFC_PREFIX + ":sweptArea_IfcSweptAreaSolid";
    public static final String IFC_PROFILE_TYPE = NamespaceMapper.IFC_PREFIX + ":profileType_IfcProfileDef";
    public static final String IFC_PROFILE_POSITION = NamespaceMapper.IFC_PREFIX + ":position_IfcParameterizedProfileDef";
    public static final String IFC_PROFILE_RECTANGLE_X_DIM = NamespaceMapper.IFC_PREFIX + ":xDim_IfcRectangleProfileDef";
    public static final String IFC_PROFILE_RECTANGLE_Y_DIM = NamespaceMapper.IFC_PREFIX + ":yDim_IfcRectangleProfileDef";
    public static final String IFC_HALF_SPACE_POSITION = NamespaceMapper.IFC_PREFIX + ":position_IfcPolygonalBoundedHalfSpace";
    public static final String IFC_HALF_SPACE_BOUNDARY = NamespaceMapper.IFC_PREFIX + ":polygonalBoundary_IfcPolygonalBoundedHalfSpace";
    public static final String IFC_HALF_SPACE_FLAG = NamespaceMapper.IFC_PREFIX + ":agreementFlag_IfcHalfSpaceSolid";
    public static final String IFC_HALF_SPACE_SURFACE = NamespaceMapper.IFC_PREFIX + ":baseSurface_IfcHalfSpaceSolid";
    public static final String IFC_HALF_SPACE_SURFACE_POSITION = NamespaceMapper.IFC_PREFIX + ":position_IfcElementarySurface";
    public static final String IFC_POLYLINE_POINTS = NamespaceMapper.IFC_PREFIX + ":points_IfcPolyline";
    public static final String IFC_POLYLOOP_POLYGON = NamespaceMapper.IFC_PREFIX + ":polygon_IfcPolyLoop";
    public static final String IFC_CLIPPING_RESULT_OPERATOR = NamespaceMapper.IFC_PREFIX + ":operator_IfcBooleanResult";
    public static final String IFC_CLIPPING_RESULT_FIRST_OPERAND = NamespaceMapper.IFC_PREFIX + ":firstOperand_IfcBooleanResult";
    public static final String IFC_CLIPPING_RESULT_SEC_OPERAND = NamespaceMapper.IFC_PREFIX + ":secondOperand_IfcBooleanResult";
    // IfcOwl modelling operator properties
    public static final String IFC_PLACEMENT_POSITION = NamespaceMapper.IFC_PREFIX + ":relativePlacement_IfcLocalPlacement";
    public static final String IFC_PLACEMENT_LOCATION = NamespaceMapper.IFC_PREFIX + ":location_IfcPlacement";
    public static final String IFC_REL_PLACEMENT = NamespaceMapper.IFC_PREFIX + ":placementRelTo_IfcLocalPlacement";
    public static final String IFC_REF_DIRECTION_2D = NamespaceMapper.IFC_PREFIX + ":refDirection_IfcAxis2Placement2D";
    public static final String IFC_REF_DIRECTION_3D = NamespaceMapper.IFC_PREFIX + ":refDirection_IfcAxis2Placement3D";
    public static final String IFC_AXIS_DIRECTION_3D = NamespaceMapper.IFC_PREFIX + ":axis_IfcAxis2Placement3D";
    public static final String IFC_COORDINATES = NamespaceMapper.IFC_PREFIX + ":coordinates_IfcCartesianPoint";
    public static final String IFC_DIR_RATIOS = NamespaceMapper.IFC_PREFIX + ":directionRatios_IfcDirection";
    public static final String IFC_ORIGIN_TRANSFORMATION = NamespaceMapper.IFC_PREFIX + ":localOrigin_IfcCartesianTransformationOperator";
    public static final String IFC_SCALE_TRANSFORMATION = NamespaceMapper.IFC_PREFIX + ":scale_IfcCartesianTransformationOperator";
    public static final String IFC_X_AXIS_TRANSFORMATION = NamespaceMapper.IFC_PREFIX + ":axis1_IfcCartesianTransformationOperator";
    public static final String IFC_Y_AXIS_TRANSFORMATION = NamespaceMapper.IFC_PREFIX + ":axis2_IfcCartesianTransformationOperator";
    public static final String IFC_PARENT_CONTEXT = NamespaceMapper.IFC_PREFIX + ":parentContext_IfcGeometricRepresentationSubContext";
    public static final String IFC_CONTEXT_TYPE = NamespaceMapper.IFC_PREFIX + ":contextType_IfcRepresentationContext";
    public static final String IFC_CONTEXT_IDENTIFIER = NamespaceMapper.IFC_PREFIX + ":contextIdentifier_IfcRepresentationContext";
    public static final String IFC_TARGET_VIEW = NamespaceMapper.IFC_PREFIX + ":targetView_IfcGeometricRepresentationSubContext";
    // IfcOwl Classes
    public static final String IFCPROJECT = NamespaceMapper.IFC_PREFIX + ":IfcProject";
    public static final String IFCSITE = NamespaceMapper.IFC_PREFIX + ":IfcSite";
    public static final String IFCBUILDING = NamespaceMapper.IFC_PREFIX + ":IfcBuilding";
    public static final String IFCADDRESS = NamespaceMapper.IFC_PREFIX + ":IfcPostalAddress";
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
    public static final String IFC_FACETED_BREP = NamespaceMapper.IFC_PREFIX + ":IfcFacetedBrep";
    public static final String IFC_CLOSED_SHELL = NamespaceMapper.IFC_PREFIX + ":IfcClosedShell";
    public static final String IFC_FACE = NamespaceMapper.IFC_PREFIX + ":IfcFace";
    public static final String IFC_FACE_OUTER_BOUND = NamespaceMapper.IFC_PREFIX + ":IfcFaceOuterBound";
    public static final String IFC_EXTRUDED_AREA_SOLID = NamespaceMapper.IFC_PREFIX + ":IfcExtrudedAreaSolid";
    public static final String IFC_HALF_SPACE_SOLID = NamespaceMapper.IFC_PREFIX + ":IfcHalfSpaceSolid";
    public static final String IFC_POLYGONAL_HALF_SPACE = NamespaceMapper.IFC_PREFIX + ":IfcPolygonalBoundedHalfSpace";
    public static final String IFC_SURFACE_PLANE = NamespaceMapper.IFC_PREFIX + ":IfcPlane";
    public static final String IFC_POLYLINE = NamespaceMapper.IFC_PREFIX + ":IfcPolyline";
    public static final String IFC_POLYLOOP = NamespaceMapper.IFC_PREFIX + ":IfcPolyLoop";
    public static final String IFC_CLIPPING_RESULT = NamespaceMapper.IFC_PREFIX + ":IfcBooleanClippingResult";
    // IfcOwl Element Classes
    public static final String IFC_CEILING = NamespaceMapper.IFC_PREFIX + ":IfcCovering";
    public static final String IFC_COLUMN = NamespaceMapper.IFC_PREFIX + ":IfcColumn";
    public static final String IFC_DOOR = NamespaceMapper.IFC_PREFIX + ":IfcDoor";
    public static final String IFC_SLAB = NamespaceMapper.IFC_PREFIX + ":IfcSlab";
    public static final String IFC_ROOF = NamespaceMapper.IFC_PREFIX + ":IfcRoof";
    public static final String IFC_WALL = NamespaceMapper.IFC_PREFIX + ":IfcWall";
    public static final String IFC_WALL_STANDARD_CASE = NamespaceMapper.IFC_PREFIX + ":IfcWallStandardCase";
    public static final String IFC_WINDOW = NamespaceMapper.IFC_PREFIX + ":IfcWindow";
    public static final String IFC_OPENING_ELEMENT = NamespaceMapper.IFC_PREFIX + ":IfcOpeningElement";
    public static final String IFC_STAIR_FLIGHT = NamespaceMapper.IFC_PREFIX + ":IfcStairFlight";
    public static final String IFC_STAIR_RAILING = NamespaceMapper.IFC_PREFIX + ":IfcRailing";
    public static final String IFC_STAIR_MEMBER = NamespaceMapper.IFC_PREFIX + ":IfcMember";
    public static final String IFC_STAIR = NamespaceMapper.IFC_PREFIX + ":IfcStair";
    public static final String IFC_BUILDING_ELEMENT_PROXY = NamespaceMapper.IFC_PREFIX + ":IfcBuildingElementProxy";
    public static final String IFC_FLOW_SEGMENT = NamespaceMapper.IFC_PREFIX + ":IfcFlowSegment";
    public static final String IFC_FLOW_TERMINAL = NamespaceMapper.IFC_PREFIX + ":IfcFlowTerminal";
    public static final String IFC_FURNISHING_ELEMENT = NamespaceMapper.IFC_PREFIX + ":IfcFurnishingElement";
    // IfcOwl element supplementary Classes
    public static final String IFC_REL_TYPE_DEFINITION = NamespaceMapper.IFC_PREFIX + ":IfcRelDefinesByType";
    public static final String IFC_REL_FILLS_ELEMENT = NamespaceMapper.IFC_PREFIX + ":IfcRelFillsElement";
    public static final String IFC_REL_VOIDS_ELEMENT = NamespaceMapper.IFC_PREFIX + ":IfcRelVoidsElement";
    public static final String IFC_COVERING_TYPE = NamespaceMapper.IFC_PREFIX + ":IfcCoveringType";
    // IfcOwl modelling operator classes
    public static final String IFC_CART_POINT = NamespaceMapper.IFC_PREFIX + ":IfcCartesianPoint";
    public static final String IFC_DIRECTION = NamespaceMapper.IFC_PREFIX + ":IfcDirection";
    // IfcOwl enum
    public static final String IFC_CEILING_ENUM = NamespaceMapper.IFC_PREFIX + ":CEILING";
    public static final String IFC_SLAB_FLOOR_ENUM = NamespaceMapper.IFC_PREFIX + ":FLOOR";
    public static final String IFC_SLAB_BASE_SLAB_ENUM = NamespaceMapper.IFC_PREFIX + ":BASESLAB";
    public static final String IFC_SLAB_ROOF_ENUM = NamespaceMapper.IFC_PREFIX + ":ROOF";

    /**
     * Add the statements for querying common metadata such as class name, their unique ifc ID, and name into the builder.
     *
     * @param builder      A select builder object to append the statements to.
     * @param subjectVar   The subject var, zone or element, for this builder.
     * @param uidVar       The uid var for this builder.
     * @param nameVar      The name var for this builder.
     * @param placementVar The placement var, zone or element, for this builder.
     */
    public static void addBaseQueryComponents(SelectBuilder builder, String subjectVar, String uidVar, String nameVar, String placementVar) {
        builder.addVar(subjectVar)
                .addVar(uidVar)
                .addVar(nameVar)
                .addVar(placementVar);
        builder.addWhere(subjectVar, IFC_ID + EXPRESS_HASSTRING, uidVar)
                .addWhere(subjectVar, IFC_NAME + EXPRESS_HASSTRING, nameVar)
                .addWhere(subjectVar, IFC_OBJ_PLACEMENT, placementVar)
                .addWhere(placementVar, QueryHandler.RDF_TYPE, IFCLOCALPLACEMENT);
    }

    /**
     * Add the statements for relating each element to the zone they belong in.
     *
     * @param builder A select builder object to append the statements to.
     */
    public static void addElementHostZoneQueryComponents(SelectBuilder builder) {
        builder.addVar(PARENT_ZONE_VAR);
        builder.addWhere(RELAGGR_VAR, QueryHandler.RDF_TYPE, REL_SPATIAL_ZONE_ELEMENT)
                .addWhere(RELAGGR_VAR, IFC_REL_ZONE, PARENT_ZONE_VAR)
                .addWhere(RELAGGR_VAR, IFC_REL_ELEMENT, ELEMENT_VAR);
    }

    /**
     * Add the statements for querying the geometric model representation of elements into the builder.
     *
     * @param builder A select builder object to append the statements to.
     */
    public static void addElementModelRepresentationQueryComponents(SelectBuilder builder) {
        builder.addVar(INST_SHAPE_REP_VAR)
                .addVar(REP_SUBCONTEXT_VAR)
                .addVar(GEOM_VAR)
                .addVar(INST_SHAPE_REP_TYPE_VAR)
                .addVar(GEOM_AXIS_PLACEMENT_VAR)
                .addVar(CART_TRANSFORMER_VAR);
        builder.addWhere(ELEMENT_VAR, IFC_PRODUCT_REPRESENTATION, PRODUCT_DEFINITION_VAR)
                .addWhere(PRODUCT_DEFINITION_VAR, QueryHandler.RDF_TYPE, IFC_PRODUCT_DEF_SHAPE);
        // Set up empty builders for subgroups and unions with necessary prefixes
        SelectBuilder subgroupBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(subgroupBuilder);
        SelectBuilder unionBuilder = subgroupBuilder.clone();
        // Query for the individual shape representation instances that are directly linked to specific geometries
        addModelRepQueryComponents(subgroupBuilder, PRODUCT_DEFINITION_VAR, INST_SHAPE_REP_VAR,
                INST_SHAPE_REP_TYPE_VAR, REP_SUBCONTEXT_VAR, GEOM_VAR, GEOM_TYPE_VAR);
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

    /**
     * Add the statements for querying the geometric void representation of elements into the builder.
     *
     * @param builder A select builder object to append the statements to.
     */
    public static void addVoidRepresentationQueryComponents(SelectBuilder builder) {
        SelectBuilder optionalBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(optionalBuilder);
        builder.addVar(OPENING_ELEMENT_VAR)
                .addVar(VOID_TYPE_VAR)
                .addVar(VOID_PLACEMENT_VAR)
                .addVar(VOID_SHAPE_REP_VAR)
                .addVar(VOID_REP_TYPE_VAR)
                .addVar(VOID_GEOM_VAR)
                .addVar(VOID_SUB_CONTEXT_VAR);
        // Base Void attributes
        optionalBuilder.addWhere(REL_VOID_ELEMENT_VAR, QueryHandler.RDF_TYPE, IFC_REL_VOIDS_ELEMENT)
                .addWhere(REL_VOID_ELEMENT_VAR, IFC_REL_VOIDS_OPENING, OPENING_ELEMENT_VAR)
                .addWhere(REL_VOID_ELEMENT_VAR, IFC_REL_VOIDS_ASSEMBLY, ELEMENT_VAR)
                .addWhere(OPENING_ELEMENT_VAR, QueryHandler.RDF_TYPE, IFC_OPENING_ELEMENT)
                .addWhere(OPENING_ELEMENT_VAR, IFC_OBJ_TYPE + EXPRESS_HASSTRING, VOID_TYPE_VAR)
                .addWhere(OPENING_ELEMENT_VAR, IFC_OBJ_PLACEMENT, VOID_PLACEMENT_VAR)
                .addWhere(VOID_PLACEMENT_VAR, QueryHandler.RDF_TYPE, IFCLOCALPLACEMENT);
        // Void shape representation details
        addModelRepQueryComponents(optionalBuilder, OPENING_ELEMENT_VAR, VOID_PRODUCT_DEFINITION_VAR, VOID_SHAPE_REP_VAR,
                VOID_REP_TYPE_VAR, VOID_SUB_CONTEXT_VAR, VOID_GEOM_VAR, VOID_GEOM_TYPE_VAR);
        builder.addOptional(optionalBuilder);
    }

    /**
     * Overloaded method including the element variable to link an element to their product definition if necessary
     *
     * @param builder              A select builder object to append the statements to.
     * @param elementVar           The element variable.
     * @param productDefinitionVar The product definition variable.
     * @param shapeRepVar          The shape representation variable.
     * @param shapeRepType         The shape representation type variable.
     * @param subContextVar        The sub context variable.
     * @param geomVar              The geometry variable.
     * @param geomType             The geometry type variable.
     */
    public static void addModelRepQueryComponents(SelectBuilder builder, String elementVar, String productDefinitionVar, String shapeRepVar, String shapeRepType,
                                                  String subContextVar, String geomVar, String geomType) {
        builder.addWhere(elementVar, IFC_PRODUCT_REPRESENTATION, productDefinitionVar)
                .addWhere(productDefinitionVar, QueryHandler.RDF_TYPE, IFC_PRODUCT_DEF_SHAPE);
        addModelRepQueryComponents(builder, productDefinitionVar, shapeRepVar, shapeRepType, subContextVar, geomVar, geomType);
    }

    /**
     * Add the statements for querying the model/shape representation into the builder.
     *
     * @param builder              A select builder object to append the statements to.
     * @param productDefinitionVar The product definition variable.
     * @param shapeRepVar          The shape representation variable.
     * @param shapeRepType         The shape representation type variable.
     * @param subContextVar        The sub context variable.
     * @param geomVar              The geometry variable.
     * @param geomType             The geometry type variable.
     */
    public static void addModelRepQueryComponents(SelectBuilder builder, String productDefinitionVar, String shapeRepVar, String shapeRepType,
                                                  String subContextVar, String geomVar, String geomType) {
        builder.addWhere(productDefinitionVar, IFC_PRODUCT_REPRESENTATIONS + "/" + LIST_HAS_CONTENT, shapeRepVar)
                .addWhere(shapeRepVar, QueryHandler.RDF_TYPE, IFC_SHAPE_REP)
                .addWhere(shapeRepVar, IFC_PRODUCT_REPRESENTATION_TYPE + EXPRESS_HASSTRING, shapeRepType)
                .addWhere(shapeRepVar, IFC_REP_CONTEXT, subContextVar)
                .addWhere(subContextVar, QueryHandler.RDF_TYPE, IFCGEOM_REP_SUBCONTEXT)
                .addWhere(shapeRepVar, IFC_REP_ITEMS, geomVar)
                .addWhere(geomVar, QueryHandler.RDF_TYPE, geomType);
    }

    /**
     * Add the statements for querying the stair landing and its assembly stair into the builder.
     *
     * @param builder A select builder object to append the statements to.
     */
    public static void addStairLandingQueryComponents(SelectBuilder builder) {
        builder.addVar(LANDING_VAR)
                .addVar(STAIR_LANDING_SHAPE_REP_VAR)
                .addVar(STAIR_LANDING_SUB_CONTEXT_VAR)
                .addVar(STAIR_LANDING_GEOM_VAR)
                .addVar(STAIR_LANDING_REP_TYPE_VAR);
        addBaseQueryComponents(builder, LANDING_VAR, STAIR_LANDING_UID_VAR, STAIR_LANDING_NAME_VAR, STAIR_LANDING_PLACEMENT_VAR);
        builder.addWhere(REL_AGG_STAIR__VAR, QueryHandler.RDF_TYPE, RELAGG)
                .addWhere(REL_AGG_STAIR__VAR, IFC_PARENT_ZONE_REL, ELEMENT_VAR)
                // Although landing isn't properly restricted to slab enum types,
                // the additional landing query statements will restrict which slab is returned
                .addWhere(REL_AGG_STAIR__VAR, IFC_CHILD_ZONE_REL, LANDING_VAR)
                .addWhere(LANDING_VAR, QueryHandler.RDF_TYPE, IFC_SLAB);
        addModelRepQueryComponents(builder, LANDING_VAR, STAIR_LANDING_PRODUCT_DEFINITION_VAR, STAIR_LANDING_SHAPE_REP_VAR,
                STAIR_LANDING_REP_TYPE_VAR, STAIR_LANDING_SUB_CONTEXT_VAR, STAIR_LANDING_GEOM_VAR, STAIR_LANDING_GEOM_TYPE_VAR);
    }

    /**
     * Add the statements for querying the stair railing and its assembly stair into the builder.
     *
     * @param builder A select builder object to append the statements to.
     */
    public static void addStairRailingQueryComponents(SelectBuilder builder) {
        builder.addVar(RAILING_VAR)
                .addVar(STAIR_RAILING_SHAPE_REP_VAR)
                .addVar(STAIR_RAILING_SUB_CONTEXT_VAR)
                .addVar(STAIR_RAILING_GEOM_VAR)
                .addVar(STAIR_RAILING_REP_TYPE_VAR);
        addBaseQueryComponents(builder, RAILING_VAR, STAIR_RAILING_UID_VAR, STAIR_RAILING_NAME_VAR, STAIR_RAILING_PLACEMENT_VAR);
        builder.addWhere(REL_AGG_STAIR__VAR, QueryHandler.RDF_TYPE, RELAGG)
                .addWhere(REL_AGG_STAIR__VAR, IFC_PARENT_ZONE_REL, ELEMENT_VAR)
                .addWhere(REL_AGG_STAIR__VAR, IFC_CHILD_ZONE_REL, RAILING_VAR)
                .addWhere(RAILING_VAR, QueryHandler.RDF_TYPE, IFC_STAIR_RAILING);
        addModelRepQueryComponents(builder, RAILING_VAR, STAIR_RAILING_PRODUCT_DEFINITION_VAR, STAIR_RAILING_SHAPE_REP_VAR,
                STAIR_RAILING_REP_TYPE_VAR, STAIR_RAILING_SUB_CONTEXT_VAR, STAIR_RAILING_GEOM_VAR, STAIR_RAILING_GEOM_TYPE_VAR);
    }

    /**
     * Add the statements for querying the stair structural component and its assembly stair into the builder.
     *
     * @param builder A select builder object to append the statements to.
     */
    public static void addStairStructuralComponentQueryComponents(SelectBuilder builder) {
        builder.addVar(STAIR_STRUCTURAL_COMPONENT_VAR)
                .addVar(STAIR_STRUCT_COMP_SHAPE_REP_VAR)
                .addVar(STAIR_STRUCT_COMP_SUB_CONTEXT_VAR)
                .addVar(STAIR_STRUCT_COMP_GEOM_VAR)
                .addVar(STAIR_STRUCT_COMP_REP_TYPE_VAR);
        addBaseQueryComponents(builder, STAIR_STRUCTURAL_COMPONENT_VAR, STAIR_STRUCT_COMP_UID_VAR, STAIR_STRUCT_COMP_NAME_VAR, STAIR_STRUCT_COMP_PLACEMENT_VAR);
        builder.addWhere(REL_AGG_STAIR__VAR, QueryHandler.RDF_TYPE, RELAGG)
                .addWhere(REL_AGG_STAIR__VAR, IFC_PARENT_ZONE_REL, ELEMENT_VAR)
                .addWhere(REL_AGG_STAIR__VAR, IFC_CHILD_ZONE_REL, STAIR_STRUCTURAL_COMPONENT_VAR)
                .addWhere(STAIR_STRUCTURAL_COMPONENT_VAR, QueryHandler.RDF_TYPE, IFC_STAIR_MEMBER);
        addModelRepQueryComponents(builder, STAIR_STRUCTURAL_COMPONENT_VAR, STAIR_STRUCT_COMP_PRODUCT_DEFINITION_VAR, STAIR_STRUCT_COMP_SHAPE_REP_VAR,
                STAIR_STRUCT_COMP_REP_TYPE_VAR, STAIR_STRUCT_COMP_SUB_CONTEXT_VAR, STAIR_STRUCT_COMP_GEOM_VAR, STAIR_STRUCT_COMP_GEOM_TYPE_VAR);
    }

    /**
     * Add the statements for querying the stair flight and its assembly stair into the builder.
     *
     * @param builder A select builder object to append the statements to.
     */
    public static void addStairFlightQueryComponents(SelectBuilder builder) {
        builder.addVar(STAIRFLIGHT_VAR)
                .addVar(STAIR_RISER_NUM_VAR)
                .addVar(STAIR_TREAD_NUM_VAR)
                .addVar(STAIR_RISER_HEIGHT_VAR)
                .addVar(STAIR_TREAD_LENGTH_VAR)
                .addVar(STAIR_FLIGHT_SHAPE_REP_VAR)
                .addVar(STAIR_FLIGHT_SUB_CONTEXT_VAR)
                .addVar(STAIR_FLIGHT_GEOM_VAR)
                .addVar(STAIR_FLIGHT_REP_TYPE_VAR);
        addBaseQueryComponents(builder, STAIRFLIGHT_VAR, STAIR_FLIGHT_UID_VAR, STAIR_FLIGHT_NAME_VAR, STAIR_FLIGHT_PLACEMENT_VAR);
        builder.addWhere(REL_AGG_STAIR__VAR, QueryHandler.RDF_TYPE, RELAGG)
                .addWhere(REL_AGG_STAIR__VAR, IFC_PARENT_ZONE_REL, ELEMENT_VAR)
                .addWhere(REL_AGG_STAIR__VAR, IFC_CHILD_ZONE_REL, STAIRFLIGHT_VAR)
                .addWhere(STAIRFLIGHT_VAR, QueryHandler.RDF_TYPE, IFC_STAIR_FLIGHT)
                .addWhere(STAIRFLIGHT_VAR, IFC_STAIR_RISER_NUM + EXPRESS_HASINTEGER, STAIR_RISER_NUM_VAR)
                .addWhere(STAIRFLIGHT_VAR, IFC_STAIR_TREAD_NUM + EXPRESS_HASINTEGER, STAIR_TREAD_NUM_VAR)
                .addWhere(STAIRFLIGHT_VAR, IFC_STAIR_RISER_HEIGHT + EXPRESS_HASDOUBLE, STAIR_RISER_HEIGHT_VAR)
                .addWhere(STAIRFLIGHT_VAR, IFC_STAIR_TREAD_LENGTH + EXPRESS_HASDOUBLE, STAIR_TREAD_LENGTH_VAR);
        addModelRepQueryComponents(builder, STAIRFLIGHT_VAR, STAIR_FLIGHT_PRODUCT_DEFINITION_VAR, STAIR_FLIGHT_SHAPE_REP_VAR,
                STAIR_FLIGHT_REP_TYPE_VAR, STAIR_FLIGHT_SUB_CONTEXT_VAR, STAIR_FLIGHT_GEOM_VAR, STAIR_FLIGHT_GEOM_TYPE_VAR);
    }
}
