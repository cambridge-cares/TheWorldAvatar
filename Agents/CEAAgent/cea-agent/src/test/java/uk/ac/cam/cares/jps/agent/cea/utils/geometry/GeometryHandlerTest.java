package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import org.json.JSONArray;
import org.json.JSONObject;

import org.locationtech.jts.geom.*;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;

import java.util.ArrayList;
import java.util.List;

public class GeometryHandlerTest {
    @Test
    public void testGetGroundGeometry() {
        String geometry1 = "1.0#1.0#0.0#1.0#2.0#0.0#2.0#2.0#0.0#2.0#1.0#0.0#1.0#1.0#0.0";
        String geometry2 = "1.0#1.0#0.0#1.0#1.0#2.0#2.0#1.0#2.0#2.0#1.0#0.0#1.0#1.0#0.0";
        String geometry3 = "1.0#2.0#0.0#2.0#2.0#0.0#2.0#1.0#0.0#1.0#1.0#0.0#1.0#2.0#0.0";
        String geometry4 = "1.0#2.0#1.0#2.0#2.0#1.0#2.0#1.0#0.0#1.0#1.0#1.0#1.0#2.0#1.0";

        JSONArray testArray = new JSONArray();
        testArray.put(new JSONObject().put("geometry", geometry1));
        testArray.put(new JSONObject().put("geometry", geometry2));
        testArray.put(new JSONObject().put("geometry", geometry3));
        testArray.put(new JSONObject().put("geometry", geometry4));

        JSONArray expected = new JSONArray();
        expected.put(new JSONObject().put("geometry", geometry1));
        expected.put(new JSONObject().put("geometry", geometry3));

        JSONArray result = GeometryHandler.getGroundGeometry(testArray);

        assertEquals(expected.length(), result.length());

        for (int i = 0; i < expected.length(); i++){
            assertEquals(expected.getJSONObject(i).get("geometry").toString(), result.getJSONObject(i).get("geometry").toString());
        }
    }

    @Test
    public void testExtractFootprint() throws ParseException {
        String geometry1 = "1.0#1.0#0.0#2.0#1.0#0.0#2.0#2.0#0.0#1.0#1.0#0.0";
        String geometry2 = "1.0#1.0#0.0#1.0#2.0#0.0#2.0#2.0#0.0#1.0#1.0#0.0";
        String polygonType = "http://localhost/blazegraph/literals/POLYGON-3-12";

        JSONArray testArray = new JSONArray();
        testArray.put(new JSONObject().put("geometry", geometry1).put("datatype", polygonType));
        testArray.put(new JSONObject().put("geometry", geometry2).put("datatype", polygonType));

        String result = GeometryHandler.extractFootprint(testArray);

        WKTReader wktReader = new WKTReader();

        Geometry geometry = wktReader.read(result);

        Coordinate[] coordinates = geometry.getCoordinates();

        assertEquals(5, coordinates.length);
    }

    @Test
    public void testToPolygon() {
        String points = "559267.200000246#313892.7999989044#0.0#559280.5400002463#313892.7999989044#0.0#559280.5400002463#313908.7499989033#0.0#559267.200000246#313908.7499989033#0.0#559267.200000246#313892.7999989044#0.0";

        Polygon result = (Polygon) GeometryHandler.toPolygon(points);

        String expected = "POLYGON ((559267.200000246 313892.7999989044, 559280.5400002463 313892.7999989044, 559280.5400002463 313908.7499989033, 559267.200000246 313908.7499989033, 559267.200000246 313892.7999989044))";

        assertEquals(expected, result.toString());
    }

    @Test
    public void testStringToGeometries() {
        String testGeometryString = "0.0#0.0#0.0#0.0#10.0#0.0#10.0#10.0#0.0#10.0#0.0#0.0#0.0#0.0#0.0#1.0#1.0#0.0#1.0#2.0#0.0#2.0#2.0#0.0#2.0#1.0#0.0#1.0#1.0#0.0";
        String testPolygonType = "POLYGON-3-15-15";

        List<Polygon> exteriors = new ArrayList<>();
        List<LinearRing> holes = new ArrayList<>();

        GeometryHandler.stringToGeometries(testGeometryString, testPolygonType, exteriors, holes);

        assertEquals(1, exteriors.size());
        assertEquals(1, holes.size());

        String[] coordinates = testGeometryString.split("#");

        Coordinate[] exterior = exteriors.get(0).getCoordinates();
        Coordinate[] hole = holes.get(0).getCoordinates();

        int e = 0;
        int h = 0;

        for (int i = 0; i < coordinates.length; i += 3) {
            if (i < 15) {
                assertEquals(Double.valueOf(coordinates[i]), exterior[e].getX(), 0.0000001);
                assertEquals(Double.valueOf(coordinates[i+1]), exterior[e].getY(), 0.0000001);
                e++;
            }
            else {
                assertEquals(Double.valueOf(coordinates[i]), hole[h].getX(), 0.0000001);
                assertEquals(Double.valueOf(coordinates[i+1]), hole[h].getY(), 0.0000001);
                h++;
            }
        }
    }

    @Test
    public void testCoordinatesToString() {
        Coordinate[] coordinates = new Coordinate[2];

        coordinates[0] = new Coordinate(1.0, 2.0, 3.0);
        coordinates[1] = new Coordinate(4.0, 5.0, 6.0);

        String expected = "1.0#2.0#3.0#4.0#5.0#6.0";

        assertEquals(expected, GeometryHandler.coordinatesToString(coordinates));
    }

    @Test
    public void testInflatePolygon() {
                GeometryFactory gF = new GeometryFactory();
        Coordinate[] testC = new Coordinate[5];

        testC[0] = new Coordinate(1.0, 1.0, 3.01);
        testC[1] = new Coordinate(2.0, 1.0, 3.02);
        testC[2] = new Coordinate(2.0, 2.0, 3.03);
        testC[3] = new Coordinate(1.0, 2.0, 3.03);
        testC[4] = new Coordinate(1.0, 1.0, 3.01);

        Polygon testPolygon = gF.createPolygon(testC);

        Coordinate[] expectedC = new Coordinate[5];

        expectedC[0] = new Coordinate(0.9, 0.9, 3.01);
        expectedC[1] = new Coordinate(2.1, 0.9, 3.02);
        expectedC[2] = new Coordinate(2.1, 2.1, 3.03);
        expectedC[3] = new Coordinate(0.9, 2.1, 3.03);
        expectedC[4] = new Coordinate(0.9, 0.9, 3.01);

        Polygon expectedPolygon = gF.createPolygon(expectedC);

        Polygon resultPolygon = (Polygon) GeometryHandler.inflatePolygon(testPolygon, 0.1);
        assertTrue(expectedPolygon.equals(resultPolygon));
    }

    @Test
    public void testDeflatePolygon() {
        GeometryFactory gF = new GeometryFactory();
        Coordinate[] testC = new Coordinate[5];

        testC[0] = new Coordinate(1.0, 1.0, 3.01);
        testC[1] = new Coordinate(2.0, 1.0, 3.02);
        testC[2] = new Coordinate(2.0, 2.0, 3.03);
        testC[3] = new Coordinate(1.0, 2.0, 3.03);
        testC[4] = new Coordinate(1.0, 1.0, 3.01);

        Polygon testPolygon = gF.createPolygon(testC);

        Coordinate[] expectedC = new Coordinate[5];

        expectedC[0] = new Coordinate(1.1, 1.1, 3.01);
        expectedC[1] = new Coordinate(1.9, 1.1, 3.02);
        expectedC[2] = new Coordinate(1.9, 1.9, 3.03);
        expectedC[3] = new Coordinate(1.1, 1.9, 3.03);
        expectedC[4] = new Coordinate(1.1, 1.1, 3.01);

        Polygon expectedPolygon = gF.createPolygon(expectedC);

        Polygon resultPolygon = (Polygon) GeometryHandler.deflatePolygon(testPolygon, 0.1);
        assertTrue(expectedPolygon.equals(resultPolygon));
    }

    @Test
    public void testTransformCoordinate() throws Exception {
        Coordinate testCoordinate = new Coordinate(1.0, 2.0);

        String testSource = "EPSG:32633";
        String testTarget = "EPSG:4326";

        Coordinate result;

        // result should be the same if source and target CRS are the same
        result = GeometryHandler.transformCoordinate(testCoordinate, testSource, testSource);
        assertEquals(testCoordinate.getX(), result.getX(), 0.000001);
        assertEquals(testCoordinate.getY(), result.getY(), 0.000001);

        // result should be different if source and target CRS are the same
        result = GeometryHandler.transformCoordinate(testCoordinate, testSource, testTarget);
        assertNotEquals(testCoordinate.getX(), result.getX(), 0.000001);
        assertNotEquals(testCoordinate.getY(), result.getY(), 0.000001);
    }
}
