package uk.ac.cam.cares.ogm.models.geo;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;

import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;

/**
 * Model representing OntoCityGML Building objects.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
@ModelAnnotation(defaultGraphName = SchemaManagerAdapter.BUILDING_GRAPH + "/")
public class Building extends OntoCityGMLModel {

  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_FUNCTION) protected String function;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_ROOF_TYPE) protected String roofType;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_BUILDING_PARENT_ID) protected URI buildingParentId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_BUILDING_ROOT_ID) protected URI buildingRootId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_CLASS) protected String classID; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_CLASS_CODESPACE) protected String classCodespace; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_FUNCTION_CODESPACE) protected String functionCodespace; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_FOOTPRINT_ID) protected SurfaceGeometry lod0FootprintId; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_ROOFPRINT_ID) protected SurfaceGeometry lod0RoofprintId; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD1_MULTI_SURFACE_ID) protected SurfaceGeometry lod1MultiSurfaceId; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD2_MULTI_SURFACE_ID) protected SurfaceGeometry lod2MultiSurfaceId; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD3_MULTI_SURFACE_ID) protected SurfaceGeometry lod3MultiSurfaceId; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD4_MULTI_SURFACE_ID) protected SurfaceGeometry lod4MultiSurfaceId; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD1_SOLID_ID) protected SurfaceGeometry lod1SolidId; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD2_SOLID_ID) protected SurfaceGeometry lod2SolidId; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD3_SOLID_ID) protected SurfaceGeometry lod3SolidId; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD4_SOLID_ID) protected SurfaceGeometry lod4SolidId; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD1_TERRAIN_INTERSECTION) protected URI lod1TerrainIntersection; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD2_TERRAIN_INTERSECTION) protected URI lod2TerrainIntersection; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD3_TERRAIN_INTERSECTION) protected URI lod3TerrainIntersection; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD4_TERRAIN_INTERSECTION) protected URI lod4TerrainIntersection; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD2_MULTI_CURVE) protected URI lod2MultiCurve; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD3_MULTI_CURVE) protected URI lod3MultiCurve; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD4_MULTI_CURVE) protected URI lod4MultiCurve; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_MEASURED_HEIGHT) protected Double measuredHeight;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_MEASURED_HEIGHT_UNIT) protected String measuredHeightUnit; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_OBJECT_CLASS_ID) protected BigInteger objectClassId = OBJECT_CLASS_ID;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_ROOF_TYPE_CODESPACE) protected String roofTypeCodespace; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_STOREY_HEIGHTS_ABOVE_GROUND) protected String storeyHeightsAboveGround; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_STOREY_HEIGHTS_BELLOW_GROUND) protected String storeyHeightsBelowGround; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_STOREY_HEIGHTS_AG_UNIT) protected String storeyHeightsAgUnit; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_STOREY_HEIGHTS_BG_UNIT) protected String storeyHeightsBgUnit; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_STOREYS_ABOVE_GROUND) protected String storeysAboveGround; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_STOREYS_BELLOW_GROUND) protected String storeysBelowGround; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_USAGE) protected String usage; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_USAGE_CODESPACE) protected String usageCodespace; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_YEAR_CONSTRUCTION) protected String yearOfConstruction; // check-type
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_YEAR_DEMOLITION) protected String yearOfDemolition; // check-type

  @Getter @Setter @FieldAnnotation(
      value = SchemaManagerAdapter.ONTO_BUILDING_ID,
      graphName = SchemaManagerAdapter.THEMATIC_SURFACE_GRAPH + "/",
      innerType = ThematicSurface.class,
      backward = true)
  private ArrayList<ThematicSurface> thematicSurfaces;

  public static final BigInteger OBJECT_CLASS_ID = BigInteger.valueOf(26);

}
