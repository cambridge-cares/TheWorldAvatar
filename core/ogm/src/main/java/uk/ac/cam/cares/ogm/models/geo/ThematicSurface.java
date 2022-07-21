package uk.ac.cam.cares.ogm.models.geo;

import lombok.Getter;
import lombok.Setter;
import org.citydb.database.adapter.blazegraph.SchemaManagerAdapter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;

import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;

/**
 * Model representing OntoCityGML thematic surface (_BoundarySurface) objects.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
@ModelAnnotation(defaultGraphName = SchemaManagerAdapter.THEMATIC_SURFACE_GRAPH + "/")
public class ThematicSurface extends OntoCityGMLModel {

  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_BUILDING_ID)  private Building buildingId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_BUILDING_INSTALLATION_ID)  private URI buildingInstallationId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD2_MULTI_SURFACE_ID)  private SurfaceGeometry lod2MultiSurfaceId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD3_MULTI_SURFACE_ID)  private SurfaceGeometry lod3MultiSurfaceId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD4_MULTI_SURFACE_ID)  private SurfaceGeometry lod4MultiSurfaceId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_OBJECT_CLASS_ID)  private BigInteger objectClassId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_ROOM_ID)  private Room roomId;

  @Getter @Setter @FieldAnnotation(
      value = SchemaManagerAdapter.ONTO_OPENING_ID,
      graphName = SchemaManagerAdapter.OPENING_TO_THEM_SURFACE_GRAPH + "/",
      innerType = Opening.class) private ArrayList<Opening> openingId;

  public static final BigInteger CEILING_SURFACE = BigInteger.valueOf(30);
  public static final BigInteger INTERIOR_WALL_SURFACE = BigInteger.valueOf(31);
  public static final BigInteger FLOOR_SURFACE = BigInteger.valueOf(32);
  public static final BigInteger ROOF_SURFACE = BigInteger.valueOf(33);
  public static final BigInteger WALL_SURFACE = BigInteger.valueOf(34);
  public static final BigInteger GROUND_SURFACE = BigInteger.valueOf(35);
  public static final BigInteger CLOSURE_SURFACE = BigInteger.valueOf(36);
  public static final BigInteger OUTER_CEILING_SURFACE = BigInteger.valueOf(60);
  public static final BigInteger OUTER_FLOOR_SURFACE = BigInteger.valueOf(61);

}
