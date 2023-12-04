package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import org.json.JSONArray;
import org.json.JSONObject;

import org.locationtech.jts.geom.*;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.mockito.MockedStatic;

import java.util.List;

public class GeometryHandlerTest {
    @Test
    public void testExtractFootprint() throws ParseException {
        String geometry1 = "POLYGON((0 0, 0 4, 4 4, 4 0, 0 0))";
        String geometry2 = "POLYGON((4 0, 4 4, 8 4, 8 0, 4 0))";

        String expectedWKT = "POLYGON((0 0, 0 4, 8 4, 8 0, 0 0))";

        Geometry expectedPolygon = new WKTReader().read(expectedWKT);

        String testCRS = "4326";
        String testEPSG = "<http://www.opengis.net/def/crs/EPSG/0/" + testCRS + ">";

        JSONArray testArray = new JSONArray();
        testArray.put(new JSONObject().put("wkt", testEPSG + " " + geometry1).put("crs", testEPSG));
        testArray.put(new JSONObject().put("wkt", testEPSG + " " + geometry2).put("crs", testEPSG));

        List<Geometry> result = GeometryHandler.extractFootprint(testArray, testCRS, 10.0);

        assertEquals(1, result.size());

        Geometry geometry = result.get(0);
        Geometry intersection = expectedPolygon.intersection(geometry);

        assertEquals(expectedPolygon.getNumPoints(), geometry.getNumPoints());
        assertTrue(expectedPolygon.distance(geometry) < 0.0001);
        assertTrue((intersection.getArea() / expectedPolygon.getArea() >= 0.99) && (intersection.getArea() / geometry.getArea() >= 0.99));
    }

    @Test
    public void testBufferPolygon() {
        Coordinate[] coordinates = new Coordinate[] {
                new Coordinate(0, 0),
                new Coordinate(0, 5),
                new Coordinate(5, 5),
                new Coordinate(5, 0),
                new Coordinate(0, 0)
        };

        GeometryFactory geometryFactory = new GeometryFactory();

        Polygon polygon = geometryFactory.createPolygon(coordinates);

        try (MockedStatic<GeometryHandler> geometryHandlerMock = mockStatic(GeometryHandler.class, CALLS_REAL_METHODS)) {
            geometryHandlerMock.when(() -> GeometryHandler.isCRSUnitMeter(anyString())).thenReturn(true);
            geometryHandlerMock.when(() -> GeometryHandler.transformGeometry(any(), anyString(), anyString())).thenReturn(polygon);
            geometryHandlerMock.when(() -> GeometryHandler.buffer(any(), anyDouble())).thenReturn(polygon);

            // check when CRS is Cartesian
            GeometryHandler.bufferPolygon(polygon, "123", 1.0);

            geometryHandlerMock.verify(times(2), () -> GeometryHandler.isCRSUnitMeter(anyString()));
            geometryHandlerMock.verify(times(0), () -> GeometryHandler.transformGeometry(any(), anyString(), anyString()));
            geometryHandlerMock.verify(times(1), () -> GeometryHandler.buffer(any(), anyDouble()));

            geometryHandlerMock.when(() -> GeometryHandler.isCRSUnitMeter(anyString())).thenReturn(false);

            // check when CRS is non-Cartesian
            GeometryHandler.bufferPolygon(polygon, "123", 1.0);

            geometryHandlerMock.verify(times(4), () -> GeometryHandler.isCRSUnitMeter(anyString()));
            geometryHandlerMock.verify(times(2), () -> GeometryHandler.transformGeometry(any(), anyString(), anyString()));
            geometryHandlerMock.verify(times(2), () -> GeometryHandler.buffer(any(), anyDouble()));
        }
    }

    @Test
    public void testBuffer() {
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

        Polygon resultPolygon = (Polygon) GeometryHandler.buffer(testPolygon, 0.1);
        assertTrue(expectedPolygon.equals(resultPolygon));
    }

    @Test
    public void testTransformGeometry() throws Exception {
        Coordinate[] coordinates = new Coordinate[] {
                new Coordinate(0, 0),
                new Coordinate(0, 5),
                new Coordinate(5, 5),
                new Coordinate(5, 0),
                new Coordinate(0, 0)
        };

        GeometryFactory geometryFactory = new GeometryFactory();

        Polygon polygon = geometryFactory.createPolygon(coordinates);

        try (MockedStatic<GeometryHandler> geometryHandlerMock = mockStatic(GeometryHandler.class, CALLS_REAL_METHODS)) {
            geometryHandlerMock.when(() -> GeometryHandler.transformCoordinate(any(), anyString(), anyString())).thenReturn(coordinates[0]);

            // check when CRS is Cartesian
            GeometryHandler.transformGeometry(polygon, "123", "321");

            geometryHandlerMock.verify(times(coordinates.length), () -> GeometryHandler.transformCoordinate(any(), anyString(), anyString()));
        }
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
