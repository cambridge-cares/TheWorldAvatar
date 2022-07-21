package uk.ac.cam.cares.ogm.models.geo.test;

import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Polygon;
import uk.ac.cam.cares.ogm.models.geo.EnvelopeType;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class EnvelopeTypeTest {

  @Test
  public void testComputeBounds() {
    Polygon bounds = EnvelopeType.computeBounds(new Coordinate[]{
        new Coordinate(0, -2, 0),
        new Coordinate(1, 0, 0),
        new Coordinate(0, 0, 2),
        new Coordinate(0, 4, 0),
        new Coordinate(0, -2, 0),
        new Coordinate(2, 3, 1),
        new Coordinate(3, 2, 1),
        new Coordinate(1, 2, 2),
        new Coordinate(2, 7, 6),
        new Coordinate(2, 3, 1)
    });
    assertEquals(new Coordinate(0, -2, 0), bounds.getExteriorRing().getCoordinateN(0));
    assertEquals(new Coordinate(3, 7, 6), bounds.getExteriorRing().getCoordinateN(2));
  }

}