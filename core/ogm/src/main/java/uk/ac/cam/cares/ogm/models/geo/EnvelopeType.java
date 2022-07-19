package uk.ac.cam.cares.ogm.models.geo;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Polygon;
import java.util.Arrays;
import java.util.List;

/**
 * GeometryType with helper functions for obtaining the bounding envelope of geometries.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
public class EnvelopeType extends GeometryType {

  public EnvelopeType(String data, String structure) {
    super(data, structure);
  }

  public EnvelopeType(SurfaceGeometry rootSurfaceGeometry) {
    super(computeBounds(rootSurfaceGeometry.getFlattenedSubtree(true)));
  }

  public EnvelopeType(List<SurfaceGeometry> leafSurfaceGeometries) {
    super(computeBounds(leafSurfaceGeometries));
  }

  public static Polygon computeBounds(List<SurfaceGeometry> polygons) {
    // Flatten all exterior polygons into a single coordinate array
    Coordinate[] flatCoordinates = polygons.stream().flatMap(
        (SurfaceGeometry surf) -> Arrays.stream(surf.getGeometryType().getPolygon().getExteriorRing().getCoordinates())
    ).toArray(Coordinate[]::new);
    return computeBounds(flatCoordinates);
  }

  public static Polygon computeBounds(Coordinate[] coordinates) {
    // Functionally fold the array into the minimum x, y, z.
    Coordinate min = Arrays.stream(coordinates).reduce((Coordinate a, Coordinate b)
        -> new Coordinate(Math.min(a.x, b.x), Math.min(a.y, b.y), Math.min(a.getZ(), b.getZ()))).get();
    // Functionally fold the array into the maximum x, y, z.
    Coordinate max = Arrays.stream(coordinates).reduce((Coordinate a, Coordinate b)
        -> new Coordinate(Math.max(a.x, b.x), Math.max(a.y, b.y), Math.max(a.getZ(), b.getZ()))).get();
    // Construct the envelope with the min and max x, y, z
    return factory.createPolygon(new Coordinate[]{
        new Coordinate(min.x, min.y, min.getZ()), // -x -y -z (min)
        new Coordinate(max.x, min.y, min.getZ()), // +x -y -z
        new Coordinate(max.x, max.y, max.getZ()), // +x +y +z (max)
        new Coordinate(min.x, max.y, max.getZ()), // -x +y +z
        new Coordinate(min.x, min.y, min.getZ())  // -x -y -z (min)
    });
  }

  public Coordinate getLowerBound() {
    return polygon.getExteriorRing().getCoordinateN(0);
  }

  public Coordinate getUpperBound() {
    return polygon.getExteriorRing().getCoordinateN(2);
  }

}