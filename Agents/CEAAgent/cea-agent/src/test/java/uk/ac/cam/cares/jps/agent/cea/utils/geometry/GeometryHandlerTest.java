package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.cts.op.CoordinateOperation;
import org.junit.jupiter.api.Test;

import org.locationtech.jts.geom.*;
import org.mockito.MockedStatic;

import java.util.Collections;
import java.util.Set;

public class GeometryHandlerTest {
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
        Set<CoordinateOperation> operations = Collections.emptySet();

        try (MockedStatic<GeometryHandler> geometryHandlerMock = mockStatic(GeometryHandler.class, CALLS_REAL_METHODS)) {
            geometryHandlerMock.when(() -> GeometryHandler.getTransformOperations(anyString(), anyString()))
                    .thenReturn(operations);
            geometryHandlerMock.when(() -> GeometryHandler.transformCoordinate(any(Coordinate.class), same(operations)))
                    .thenAnswer(invocation -> invocation.getArgument(0));

            assertSame(polygon, GeometryHandler.transformGeometry(polygon, "123", "123"));

            // check when CRS is Cartesian
            GeometryHandler.transformGeometry(polygon, "123", "321");

            geometryHandlerMock.verify(times(1), () -> GeometryHandler.getTransformOperations("123", "321"));
            geometryHandlerMock.verify(times(coordinates.length),
                    () -> GeometryHandler.transformCoordinate(any(Coordinate.class), same(operations)));
        }
    }

    @Test
    public void testTransformCoordinate() throws Exception {
        Coordinate testCoordinate = new Coordinate(1.0, 2.0);
        CoordinateOperation operation = mock(CoordinateOperation.class);
        when(operation.transform(any(double[].class))).thenReturn(new double[] { 3.0, 4.0, 5.0 });

        Coordinate result = GeometryHandler.transformCoordinate(testCoordinate, Collections.singleton(operation));

        assertEquals(3.0, result.getX(), 0.000001);
        assertEquals(4.0, result.getY(), 0.000001);
        assertEquals(5.0, result.getZ(), 0.000001);
        verify(operation, times(1)).transform(any(double[].class));
    }
}
