package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import org.cts.crs.CRSException;
import org.json.JSONArray;

import org.cts.CRSFactory;
import org.cts.crs.CoordinateReferenceSystem;
import org.cts.units.Unit;
import org.cts.crs.GeodeticCRS;
import org.cts.op.CoordinateOperation;
import org.cts.op.CoordinateOperationFactory;
import org.cts.registry.EPSGRegistry;
import org.cts.registry.RegistryManager;

import org.locationtech.jts.geom.*;
import org.locationtech.jts.geom.util.GeometryFixer;
import org.locationtech.jts.io.ParseException;
import org.apache.jena.geosparql.implementation.parsers.wkt.WKTReader;
import org.locationtech.jts.operation.buffer.BufferOp;
import org.locationtech.jts.operation.buffer.BufferParameters;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class GeometryHandler {
    private static final Integer METER_EPSG = 3395;
    private static final String METER_EPSG_STRING = "EPSG:" + METER_EPSG;

    public static Geometry toGeometry(String geometryString) {
        return WKTReader.extract(geometryString).getGeometry();
    }

    public static Geometry bufferPolygon(Geometry geom, String sourceCRS, Double distance) throws Exception {
        Geometry transformed;
        Integer source = Integer.parseInt(sourceCRS.replaceAll("[^0-9]", ""));

        geom.setSRID(source);

        if (!isCRSUnitMeter(sourceCRS)) {
            transformed = transformGeometry(geom, sourceCRS, METER_EPSG_STRING);
            transformed.setSRID(METER_EPSG);
        }
        else {
            transformed = geom;
        }

        Geometry result = buffer(transformed, distance);

        if (!isCRSUnitMeter(sourceCRS)) {
            result = transformGeometry(result, METER_EPSG_STRING, sourceCRS);
        }

        result.setSRID(source);

        return result;
    }

    /**
     * Inflates a polygon
     * @param geom polygon geometry
     * @param distance buffer distance
     * @return inflated polygon
     */
    public static Geometry buffer(Geometry geom, Double distance) {
        BufferParameters bufferParameters = new BufferParameters();
        bufferParameters.setEndCapStyle(BufferParameters.CAP_ROUND);
        bufferParameters.setJoinStyle(BufferParameters.JOIN_MITRE);
        Geometry buffered = BufferOp.bufferOp(geom, distance, bufferParameters);
        buffered.setUserData(geom.getUserData());
        return buffered;
    }

    /**
     * Transforms a geometry from sourceCRS to targetCRS
     * @param geometry geometry object
     * @param sourceCRS source CRS of geometry
     * @param targetCRS target CRS for geometry transformation
     * @return the transformed geometry in targetCRS
     */
    public static Geometry transformGeometry(Geometry geometry, String sourceCRS, String targetCRS) throws Exception {
        Coordinate[] coordinates = geometry.getCoordinates();
        Coordinate[] transformed = new Coordinate[coordinates.length];

        for (int i = 0; i < coordinates.length; i++) {
            transformed[i] = transformCoordinate(coordinates[i], sourceCRS, targetCRS);
        }

        GeometryFactory geometryFactory = new GeometryFactory();

        return geometryFactory.createPolygon(transformed);
    }

    /**
     * Transform a coordinate from sourceCRS to targetCRS
     * @param coordinate coordinate to be transformed
     * @param sourceCRS source CRS of coordinate
     * @param targetCRS target CRS for coordinate transformation
     * @return the transformed coordinate in targetCRS
     */
    public static Coordinate transformCoordinate(Coordinate coordinate, String sourceCRS, String targetCRS) throws Exception {
        CRSFactory crsFactory = new CRSFactory();
        RegistryManager registryManager = crsFactory.getRegistryManager();
        registryManager.addRegistry(new EPSGRegistry());

        CoordinateReferenceSystem source = crsFactory.getCRS(sourceCRS);
        CoordinateReferenceSystem target = crsFactory.getCRS(targetCRS);

        Set<CoordinateOperation> operations = CoordinateOperationFactory
                .createCoordinateOperations((GeodeticCRS) source, (GeodeticCRS) target);

        double[] transform = new double[3];
        if (operations.size() != 0) {
            // Test each transformation method (generally, only one method is available)
            for (CoordinateOperation op : operations) {
                // Transform coord using the op CoordinateOperation from sourceCRS to targetCRS
                transform  = op.transform(new double[] {coordinate.getX(), coordinate.getY(), coordinate.getZ()});
            }
        }

        return new Coordinate(transform[0], transform[1], transform[2]);
    }

    /**
     * Checks whether a CRS has meter as the unit of measurement
     * @param crs CRS string
     * @return true if crs has meter as the unit of measurement, false otherwise
     */
    public static boolean isCRSUnitMeter(String crs) throws CRSException {
        CRSFactory crsFactory = new CRSFactory();
        RegistryManager registryManager = crsFactory.getRegistryManager();
        registryManager.addRegistry(new EPSGRegistry());

        Unit unit = crsFactory.getCRS(crs).getCoordinateSystem().getUnit(0);

        if (unit.getName().equals(Unit.METER)) {
            return true;
        }
        else {
            return false;
        }
    }
}
