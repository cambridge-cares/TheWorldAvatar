package uk.ac.cam.cares.ogm.models.geo;

import lombok.Getter;
import lombok.Setter;
import org.citydb.database.adapter.blazegraph.SchemaManagerAdapter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;

import java.math.BigInteger;

/**
 * Model representing OntoCityGML Room objects.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
@ModelAnnotation(defaultGraphName = SchemaManagerAdapter.ROOM_GRAPH + "/")
public class Room extends OntoCityGMLModel {

  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_OBJECT_CLASS_ID) protected BigInteger objectClassId = OBJECT_CLASS_ID;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_USAGE) protected String usage;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_USAGE_CODESPACE) protected String usageCodespace;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_FUNCTION) protected String function;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_FUNCTION_CODESPACE) protected String functionCodespace;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_CLASS) protected String classID;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_CLASS_CODESPACE) protected String classCodespace;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_BUILDING_ID) protected Building buildingId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD4_MULTI_SURFACE_ID) protected SurfaceGeometry lod4MultiSurfaceId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LOD4_SOLID_ID) protected SurfaceGeometry lod4SolidId;

  public static final BigInteger OBJECT_CLASS_ID = BigInteger.valueOf(41);

}
