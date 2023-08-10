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
import org.locationtech.jts.io.WKTReader;
import org.locationtech.jts.operation.buffer.BufferOp;
import org.locationtech.jts.operation.buffer.BufferParameters;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class GeometryHandler {
    private static final Integer METER_EPSG = 3395;
    private static final String METER_EPSG_STRING = "EPSG:" + METER_EPSG;

    public static Geometry toGeometry(String geometryString) throws ParseException {
        WKTReader wktReader = new WKTReader();

        return wktReader.read(geometryString);
    }
    /**
     * Extracts the footprint of the building from its ground surface geometries
     * @param surfaceArray JSONArray of the query results for ground surface geometries
     * @return footprint as a string
     */
    public static String extractFootprint(JSONArray surfaceArray) {
        double distance = 0.00001;
        double increment = 0.00001;

        Polygon exteriorPolygon;
        List<Polygon> exteriors = new ArrayList<>();
        List<LinearRing> holes = new ArrayList<>();
        LinearRing exteriorRing;
        GeometryFactory geometryFactory = new GeometryFactory();
        String geoType;
        String datatype = surfaceArray.getJSONObject(0).get("datatype").toString();

        if (surfaceArray.length() == 1) {
            if (datatype.contains("wktLiteral")) {
                return surfaceArray.getJSONObject(0).get("geometry").toString();
            }
            else{
                throw new JPSRuntimeException("Geometry is not stored as geosparql:wktLiteral");
            }
        }
        else {
            for (int i = 0; i < surfaceArray.length(); i++) {
                if (datatype.contains("wktLiteral")) {
                    try {
                        Polygon polygon = (Polygon) toGeometry(surfaceArray.getJSONObject(i).get("geometry").toString());
                        exteriors.add(geometryFactory.createPolygon(polygon.getExteriorRing()));
                        for (int j = 0; j < polygon.getNumInteriorRing(); j++) {
                            holes.add(polygon.getInteriorRingN(j));
                        }
                    }
                    catch (ParseException e) {
                        e.printStackTrace();
                        throw new JPSRuntimeException(e);
                    }
                }
            }

            GeometryCollection geoCol = (GeometryCollection) geometryFactory.buildGeometry(exteriors);

            Geometry merged = geoCol.union();

            geoType = merged.getGeometryType();

            while (!geoType.equals("Polygon") || !buffer(merged, distance).getGeometryType().equals("Polygon")) {
                distance += increment;

                for (int i = 0; i < exteriors.size(); i++) {
                    Polygon temp = (Polygon) buffer(exteriors.get(i), distance);
                    if (!temp.isValid()) {
                        temp = (Polygon) GeometryFixer.fix(temp);
                    }
                    exteriors.set(i, temp);
                }

                geoCol = (GeometryCollection) geometryFactory.buildGeometry(exteriors);
                merged = geoCol.union();
                geoType = merged.getGeometryType();
            }

            exteriorPolygon = (Polygon) buffer(merged, distance*-1);

            if (!exteriorPolygon.isValid()) {
                exteriorPolygon = (Polygon) GeometryFixer.fix(exteriorPolygon);
            }

            exteriorRing = exteriorPolygon.getExteriorRing();
        }

        if (holes.size() == 0) {
            return exteriorPolygon.toString();
        }
        else{
            LinearRing[] holeRings = holes.toArray(new LinearRing[holes.size()]);
            return geometryFactory.createPolygon(exteriorRing, holeRings).toString();
        }
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
