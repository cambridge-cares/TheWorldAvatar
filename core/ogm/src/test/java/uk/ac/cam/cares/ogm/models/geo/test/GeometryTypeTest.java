package uk.ac.cam.cares.ogm.models.geo.test;

import org.apache.jena.graph.Node;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.math.Vector3D;
import uk.ac.cam.cares.ogm.models.geo.GeometryType;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class GeometryTypeTest {

  @Test
  public void testRDFSerialisationDeserialisation() {
    String coords =
        "0.0#0.0#0.0#10.0#0.0#1.0#10.0#10.0#1.0#5.0#15.0#0.5#0.0#10.0#0.0#0.0#0.0#0.0#" + // outer ring
            "2.0#2.0#0.2#4.0#2.0#0.4#4.0#4.0#0.4#2.0#4.0#0.2#2.0#2.0#0.2"; // inner ring
    String structure = "http://localhost/blazegraph/literals/POLYGON-3-18-15";
    // Test deserialisation into correct structure
    GeometryType geometry = new GeometryType(coords, structure);
    assertEquals(6, geometry.getPolygon().getExteriorRing().getNumPoints());
    assertEquals(5, geometry.getPolygon().getInteriorRingN(0).getNumPoints());
    assertEquals(1, geometry.getPolygon().getNumInteriorRing());
    // Test serialisation restores original strings
    Node node = geometry.getNode();
    assertEquals(coords, node.getLiteralLexicalForm());
    assertEquals(structure, node.getLiteralDatatypeURI());
  }

  @Test
  public void testCrsMetricConversion() {
    String coords = "1#1#5#2#1#5#2#3#5#1#3#5#1#1#5";
    String structure = "http://localhost/blazegraph/literals/POLYGON-3-15";
    GeometryType.setSourceCrsName("EPSG:27700");
    GeometryType geometry = new GeometryType(coords, structure);
    assertEquals(new Coordinate(1, 1, 5), geometry.getPolygon().getExteriorRing().getCoordinateN(0));
    assertEquals(new Coordinate(603902.5252167048, 5513703.7177186, 5.0), geometry.getMetricPolygon().getExteriorRing().getCoordinateN(0));
    assertEquals(new Coordinate(2, 3, 5), geometry.getPolygon().getExteriorRing().getCoordinateN(2));
    assertEquals(new Coordinate(603903.3328829142, 5513705.798364675, 5.0), geometry.getMetricPolygon().getExteriorRing().getCoordinateN(2));
  }

  @Test
  public void testGeometricPropertyComputations() {
    GeometryType.setSourceCrsName("EPSG:27700");
    // Triangle  (1/8,0,0) (0,1/4,0) (0,0,1/2) should have
    // normal (8,4,2) => (0.87,0.44,0.22)   (note the ccw winding order)
    String coords = "0.125#0#0#0#0.25#0#0#0#0.5#0.125#0#0";
    String structure = "http://localhost/blazegraph/literals/POLYGON-3-12";
    assertEquals(new Vector3D(0.8284510730510422, 0.5159635577328808, 0.21783118842857427).toString(),
        new GeometryType(coords, structure).getNormal().toString());
    // Unit square should have unit area
    coords = "0#0#0#1#0#0#1#1#0#0#1#0#0#0#0";
    structure = "http://localhost/blazegraph/literals/POLYGON-3-15";
    GeometryType unitSquare = new GeometryType(coords, structure);
    assertEquals(0.9962825445109047, unitSquare.getArea());
    // Centroids: the native crs centroid should be at about (0.5,0.5,0.5)
    assertEquals(new Coordinate(0.5012210836284794, 0.507797131431289, 0.0), unitSquare.getCentroid());
    assertEquals(new Coordinate(603902.0748522267, 5513703.1742914090, 0.0), unitSquare.getMetricCentroid());
  }

  @Test
  public void testComputeCentroid() {
    Coordinate[] coordinates = {
        new Coordinate(0, 0, 0),
        new Coordinate(1, 0, 0),
        new Coordinate(0, 0, 2),
        new Coordinate(0, 4, 0)
    };
    assertEquals(new Coordinate(0.25, 1, 0.5), GeometryType.computeCentroid(coordinates, false));
    assertEquals(new Coordinate(1.0/3, 0, 2.0/3), GeometryType.computeCentroid(coordinates, true));
  }

  @Test
  public void testComputeVectorArea() {
    Coordinate[] coordinates = {
        new Coordinate(0, 0, 1),
        new Coordinate(1, 0, 1),
        new Coordinate(1, 1, 1),
        new Coordinate(0, 1, 1),
        new Coordinate(0, 0, 1)
    };
    assertEquals(new Vector3D(0, 0, 1).toString(),
        GeometryType.computeVectorArea(new GeometryFactory().createLinearRing(coordinates)).toString());
  }

}
