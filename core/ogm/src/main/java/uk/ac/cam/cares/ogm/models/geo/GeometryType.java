package uk.ac.cam.cares.ogm.models.geo;

import lombok.Getter;
import lombok.Setter;
import org.apache.jena.datatypes.BaseDatatype;
import org.apache.jena.datatypes.RDFDatatype;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.geotools.geometry.jts.JTS;
import org.geotools.referencing.CRS;
import org.locationtech.jts.geom.*;
import org.locationtech.jts.math.Vector3D;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.ogm.models.DatatypeModel;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * A representation of GeometryType literals in OntoCityGML. Note that to specify the source coordinate reference system
 * of a {@link GeometryType}, set the static field <code>sourceCrsName</code> before construction.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
public class GeometryType implements DatatypeModel {

  private static final String DATATYPE_IRI_HEAD = "http://localhost/blazegraph/literals/POLYGON-3";
  private static final String VALUE_DELIMITER = "#";
  private static final String STRUCTURE_DELIMITER = "-";
  private static final String STRUCTURE_PREFIX = "POLYGON-";
  private static final String UTM_FORMAT = "AUTO:42001,1,%s,%s";
  private static final String EPSG_4326 = "urn:x-ogc:def:crs:EPSG:4326";
  private static final CoordinateReferenceSystem epsg4326;

  static {
    try {
      epsg4326 = CRS.decode(EPSG_4326);
    } catch (FactoryException e) {
      throw new JPSRuntimeException(e);
    }
  }

  protected static final GeometryFactory factory = new GeometryFactory();
  private static final Map<String, BaseDatatype> datatypes = new HashMap<>();

  // Set source crs and metric crs before constructing a GeometryType
  // TODO: figure out how to make this work nicer with multithreading
  @Setter @Getter protected volatile static String sourceCrsName = EPSG_4326;

  // At construction, these are saved and initialised from the static settings.
  protected MathTransform transform = null;
  @Getter protected Coordinate utmAnchor; // EPSG:4326
  @Getter protected CoordinateReferenceSystem sourceCrs;
  @Getter protected CoordinateReferenceSystem metricCrs;

  @Getter protected Polygon polygon;
  @Getter private Coordinate centroid;
  @Getter private Polygon metricPolygon;
  @Getter private Coordinate metricCentroid;
  @Getter Vector3D normal;
  @Getter double area = -1;

  public GeometryType(Polygon polygon) {
    this.setPolygon(polygon);
  }

  public GeometryType(String data, String datatype) {
    // decode coordinates
    String[] valueStrings = data.split(VALUE_DELIMITER);
    double[] values = new double[valueStrings.length];
    for (int i = 0; i < valueStrings.length; i++) values[i] = Double.parseDouble(valueStrings[i]);
    // Deserialize coordinates into rings based on structure string, which should be e.g. [...]POLYGON-3-24-15-15.
    String[] splitDatatypeString = datatype.split(STRUCTURE_PREFIX); // ["...", "3-24-15-15"]
    String[] splitStructureString = splitDatatypeString[splitDatatypeString.length-1].split(STRUCTURE_DELIMITER); // ["3", "24", "15", "15"]
    String[] ringSizes = Arrays.copyOfRange(splitStructureString, 1, splitStructureString.length); // // ["24", "15", "15"]
    LinearRing exterior = null;
    LinearRing[] holes = new LinearRing[ringSizes.length - 1];
    int k = 0;
    try {
      for (int i = 0; i < ringSizes.length; i++) {
        int ringSize = Integer.parseInt(ringSizes[i]) / 3;
        Coordinate[] coords = new Coordinate[ringSize];
        for (int j = 0; j < ringSize; j++)
          coords[j] = new Coordinate(values[k++], values[k++], values[k++]);
        if (i == 0) exterior = factory.createLinearRing(coords);
        else holes[i - 1] = factory.createLinearRing(coords);
      }
    } catch (IndexOutOfBoundsException e) {
      throw new JPSRuntimeException(e);
    }
    polygon = factory.createPolygon(exterior, holes);
    recalculateGeometricProperties();
  }

  public void setPolygon(Polygon polygon) {
    this.polygon = polygon;
    recalculateGeometricProperties();
  }

  /**
   * Calculates centroid, metric polygon, metric centroid, normal and area.
   * Call this after modifying <code>polygon</code>. This is automatically called in the constructor.
   */
  protected void recalculateGeometricProperties() {
    // Generate transformation to metric
    try {
      // Generate transform
      sourceCrs = CRS.decode(sourceCrsName);
      Coordinate utmAnchorInSourceSrs = polygon.getExteriorRing().getCoordinateN(0);
      MathTransform transformToEpsg4326 = CRS.findMathTransform(sourceCrs, epsg4326, true);
      utmAnchor = JTS.transform(utmAnchorInSourceSrs, utmAnchor, transformToEpsg4326);
      // Provide y, x because EPSG 4326 has a NORTH_EAST (LAT_LON) axis order, but the UTM center is specified LON_LAT.
      metricCrs = CRS.decode(String.format(UTM_FORMAT, utmAnchor.y, utmAnchor.x));
      transform = CRS.findMathTransform(sourceCrs, metricCrs, true);
      // Calculate properties
      metricPolygon = (Polygon) JTS.transform(polygon, transform);
      metricCentroid = computeCentroid(metricPolygon);
      centroid = JTS.transform(metricCentroid, centroid, transform.inverse());
      centroid.setZ(metricCentroid.getZ());
      Vector3D vectorArea = computeVectorArea((LinearRing) metricPolygon.getExteriorRing());
      normal = vectorArea.normalize();
      area = vectorArea.length();
    } catch (TransformException | FactoryException e) {
      throw new JPSRuntimeException(e);
    }
  }

  /**
   * Computes the centroid of a {@link Polygon} via naive averaging of its point coordinates.
   * @param polygon the polygon whose centroid is to be compute.
   * @return the computed centroid in the same coordinate reference system as the source polygon.
   */
  public static Coordinate computeCentroid(Polygon polygon) {
    return computeCentroid(polygon.getExteriorRing().getCoordinates(), true);
  }

  /**
   * Computes the average position of an array of <code>Coordinate</code>s.
   * @param coordinates the coordinates to average.
   * @param skipLast    whether to exclude the last coordinate in the array from consideration.
   * @return the computed average position.
   */
  public static Coordinate computeCentroid(Coordinate[] coordinates, boolean skipLast) {
    double x = 0;
    double y = 0;
    double z = 0;
    int length = coordinates.length - (skipLast ? 1 : 0);
    for (int i = 0; i < length; i++) {
      x += coordinates[i].getX();
      y += coordinates[i].getY();
      z += coordinates[i].getZ();
    }
    return new Coordinate(x / length, y / length, z / length);
  }

  /**
   * Computes the vector area of a polygon described by a {@link LinearRing}, assuming the coordinates are
   * specified in counter-clockwise winding order (right-hand rule).
   * @param ring the polygon to calculate the vector area of.
   * @return the vector area.
   */
  public static Vector3D computeVectorArea(LinearRing ring) {
    CoordinateSequence coords = ring.getCoordinateSequence();
    double x = 0;
    double y = 0;
    double z = 0;
    for (int i = 0; i < coords.size() - 1; i++) {
      x += (coords.getZ(i) + coords.getZ(i + 1)) * (coords.getY(i) - coords.getY(i + 1));
      y += (coords.getX(i) + coords.getX(i + 1)) * (coords.getZ(i) - coords.getZ(i + 1));
      z += (coords.getY(i) + coords.getY(i + 1)) * (coords.getX(i) - coords.getX(i + 1));
    }
    return new Vector3D(x / 2, y / 2, z / 2);
  }

  /**
   * Fetches a datatype with the specified IRI. If one such datatype has been requested in the past, the same is
   * returned; otherwise, a new one is created. This enables correct <code>equals</code> behaviour, since
   * <code>new BaseDatatype(iri) != new BaseDatatype(iri)</code>.
   * @param datatypeIri the iri for which to get a datatype.
   * @return an RDFDatatype which has <code>getURI()==datatypeIri</code>.
   */
  private static RDFDatatype getDatatype(String datatypeIri) {
    if (!datatypes.containsKey(datatypeIri))
      datatypes.put(datatypeIri, new BaseDatatype(datatypeIri));
    return datatypes.get(datatypeIri);
  }

  /**
   * GeometryType literals have coordinates represented in the value string as {@code #}-separated numbers, and the
   * structure represented in the datatype as {@code http://localhost/blazegraph/literals/POLYGON-[N]-[E]-[I1]-[I2]...}
   * where
   * <ul>
   * <li><code>[N]</code> is the number of spatial dimensions of the coordinates.</li>
   * <li><code>[E]</code> is the number of numbers in the exterior ring (i.e. <code>number of points * N</code>).</li>
   * <li><code>[Ix]</code> is the number of numbers in the xth interior ring.</li>
   * </ul>
   * @return the {@link Node} representing the object.
   */
  @Override
  public Node getNode() {
    // Value
    Coordinate[] coordinates = polygon.getCoordinates();
    if (coordinates.length == 0) return NodeFactory.createBlankNode();
    StringBuilder value = new StringBuilder();
    for (Coordinate coordinate : coordinates)
      for (int i = 0; i < 3; i++)
        value.append(VALUE_DELIMITER).append(coordinate.getOrdinate(i));
    value.deleteCharAt(0);
    // Datatype
    StringBuilder datatype = new StringBuilder();
    datatype.append(DATATYPE_IRI_HEAD);
    datatype.append(STRUCTURE_DELIMITER).append(polygon.getExteriorRing().getNumPoints() * 3);
    for (int i = 0; i < polygon.getNumInteriorRing(); i++)
      datatype.append(STRUCTURE_DELIMITER).append(polygon.getInteriorRingN(i).getNumPoints() * 3);
    return NodeFactory.createLiteral(value.toString(), getDatatype(datatype.toString()));
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    GeometryType obj = (GeometryType) o;
    return Objects.equals(obj.getPolygon(), getPolygon()) && Objects.equals(obj.getSourceCrs(), getSourceCrs());
  }

}
