package uk.ac.cam.cares.ogm.models.geo;

import lombok.Getter;
import lombok.Setter;
import org.citydb.database.adapter.blazegraph.SchemaManagerAdapter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;

import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

/**
 * Model representing OntoCityGML SurfaceGeometry objects.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
@ModelAnnotation(defaultGraphName = SchemaManagerAdapter.SURFACE_GEOMETRY_GRAPH + "/")
public class SurfaceGeometry extends OntoCityGMLModel {

  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_CITY_OBJECT_ID) protected URI cityObjectId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_GEOMETRY_IMPLICIT) protected String implicitGeometryType;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_IS_COMPOSITE) protected BigInteger isComposite;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_IS_REVERSE) protected BigInteger isReverse;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_IS_SOLID) protected BigInteger isSolid;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_IS_TRIANGULATED) protected BigInteger isTriangulated;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_IS_XLINK) protected BigInteger isXlink;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_PARENT_ID) protected SurfaceGeometry parentId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_ROOT_ID) protected URI rootId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_GEOMETRY_SOLID) protected String solidType;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_GEOMETRY) protected GeometryType geometryType;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_GML_ID) protected String gmlId;

  @Getter @Setter @FieldAnnotation(
      value = SchemaManagerAdapter.ONTO_PARENT_ID,
      innerType = SurfaceGeometry.class,
      backward = true)
  private ArrayList<SurfaceGeometry> childGeometries;

  public List<SurfaceGeometry> getFlattenedSubtree(boolean ignoreNonGeometric) {
    List<SurfaceGeometry> outputList = new ArrayList<>();
    getFlattenedSubtree(outputList, ignoreNonGeometric);
    return outputList;
  }

  public void getFlattenedSubtree(List<SurfaceGeometry> outputList, boolean ignoreNonGeometric) {
    if (!ignoreNonGeometric || getGeometryType() != null)
      outputList.add(this);
    for (SurfaceGeometry child : getChildGeometries())
      child.getFlattenedSubtree(outputList, ignoreNonGeometric);
  }

}
