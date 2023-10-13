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
import org.apache.jena.geosparql.implementation.parsers.wkt.WKTReader;
import org.locationtech.jts.operation.buffer.BufferOp;
import org.locationtech.jts.operation.buffer.BufferParameters;
import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class GeometryHandler {
    private static final String EPSG_4326 = "EPSG:4326";
    private static final Integer METER_EPSG = 3395;
    private static final String METER_EPSG_STRING = "EPSG:" + METER_EPSG;
    private static final Double FLOOR_HEIGHT = 3.2;

    public static Geometry toGeometry(String geometryString) {
        return WKTReader.extract(geometryString).getGeometry();
    }

    /**
     *
     * @param geom
     * @param sourceCRS source CRS of geometry with EPSG prefix
     * @param distance
     * @return
     */
    public static Geometry bufferPolygon(Geometry geom, String sourceCRS, Double distance)  {
        if (distance == 0) {
            return geom;
        }

        Geometry transformed;
        Integer source = Integer.parseInt(sourceCRS.replaceAll("[^0-9]", ""));

        geom.setSRID(source);

        try {
            if (!isCRSUnitMeter(sourceCRS)) {
                transformed = transformGeometry(geom, sourceCRS, METER_EPSG_STRING);
                transformed.setSRID(METER_EPSG);
            } else {
                transformed = geom;
            }

            Geometry result = buffer(transformed, distance);

            if (!isCRSUnitMeter(sourceCRS)) {
                result = transformGeometry(result, METER_EPSG_STRING, sourceCRS);
            }

            result.setSRID(source);

            return result;
        }
        catch (Exception e) {
            return geom;
        }
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
     * @param sourceCRS source CRS of geometry with EPSG prefix
     * @param targetCRS target CRS for geometry transformation with EPSG prefix
     * @return the transformed geometry in targetCRS
     */
    public static Geometry transformGeometry(Geometry geometry, String sourceCRS, String targetCRS) throws Exception {
        Coordinate[] coordinates = geometry.getCoordinates();
        Coordinate[] transformed = new Coordinate[coordinates.length];

        for (int i = 0; i < coordinates.length; i++) {
            transformed[i] = transformCoordinate(coordinates[i], sourceCRS, targetCRS);
        }

        GeometryFactory geometryFactory = new GeometryFactory();

        Geometry result = geometryFactory.createPolygon(transformed);

        if (!result.isValid()) {
            result = GeometryFixer.fix(result);
        }

        return result;
    }

    /**
     * Transform a coordinate from sourceCRS to targetCRS
     * @param coordinate coordinate to be transformed
     * @param sourceCRS source CRS of coordinate with EPSG prefix
     * @param targetCRS target CRS for coordinate transformation with EPSG prefix
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

        if (unit.getName().contains(Unit.METER.getName())) {
            return true;
        }
        else {
            return false;
        }
    }

    /**
     *
     * @param surfaceArray
     * @param crs CRS of surfaceArray as String
     * @param height
     * @return
     */
    public static List<Geometry> extractFootprint(JSONArray surfaceArray, String crs, Double height) {
        double distance = 0.0;
        double increment = 0.00001;
        double limit = 0.0001;

        List<Polygon> exteriors = new ArrayList<>();
//        List<LinearRing> holes = new ArrayList<>();
        GeometryFactory geometryFactory = new GeometryFactory();
        String geoType;

        List<Geometry> result = new ArrayList<>();

        crs = "EPSG:" + crs;

        if (surfaceArray.length() == 1) {
            result.add(toGeometry(surfaceArray.getJSONObject(0).getString("wkt").split("> ")[1]));
            return result;
        }
        else {
            for (int i = 0; i < surfaceArray.length(); i++) {
                // create Polygon object from WKT string
                Polygon polygon = (Polygon) toGeometry(surfaceArray.getJSONObject(i).getString("wkt").split("> ")[1]);
                LinearRing exterior = polygon.getExteriorRing();

                // get exterior ring for buffering
                exteriors.add(geometryFactory.createPolygon(exterior));

                // store holes to add back in later
//                for (int j = 0; j < polygon.getNumInteriorRing(); j++) {
//                    holes.add(polygon.getInteriorRingN(j));
//                }
            }

            // create GeometryCollection and perform union
            GeometryCollection geoCol = geometryFactory.createGeometryCollection(exteriors.toArray(new Polygon[0]));

            Geometry merged = geoCol.union();

            geoType = merged.getGeometryType();

            distance += increment;

            // keep on inflating, merging until either the limit is reached or a single polygon is formed
            while (!geoType.equals("Polygon") && distance < limit) {
                distance += increment;

                for (int i = 0; i < exteriors.size(); i++) {
                    Polygon temp = (Polygon) bufferPolygon(exteriors.get(i), crs, distance);
                    if (!temp.isValid()) {
                        temp = (Polygon) GeometryFixer.fix(temp);
                    }
                    exteriors.set(i, temp);
                }

                geoCol = (GeometryCollection) geometryFactory.buildGeometry(exteriors);
                merged = geoCol.union();
                geoType = merged.getGeometryType();
            }

            List<Geometry> geometries = new ArrayList<>();

            if (geoType.equals("Polygon")) {
                geometries.add(bufferPolygon(merged, crs, -1 * distance));
            }
            else {
                for (int i = 0; i < merged.getNumGeometries(); i++) {
                    Geometry geometry = merged.getGeometryN(i);

                    // calculate number of floors
                    int floor = (int) Math.round(height / FLOOR_HEIGHT);

                    // at least one floor
                    if (floor == 0) {
                        floor = 1;
                    }

                    double area = 0;

                    try {
                        area = transformGeometry(geometry, crs, METER_EPSG_STRING).getArea();
                    }
                    catch (Exception e) {
                    }

                    // calculate gross floor area for the geometry, since CEA treats each geometry object as a building
                    double gfa = floor * area;

                    // CEA cannot work with geometry whose gross floor area is smaller than or equal to 100 m2
                    if (gfa > 100) {
                        // deflate geometries back
                        geometries.add(bufferPolygon(geometry, crs, -1 * distance));
                    }
                }
            }

            return geometries;
        }
    }

    public static void ensureSameCRS(List<CEAGeometryData> ceaGeometries) throws Exception {
        if (!checkSameCRS(ceaGeometries)) {
            for (int i = 0; i < ceaGeometries.size(); i++) {
                CEAGeometryData ceaGeometryData = ceaGeometries.get(i);
                List<Geometry> geometries = ceaGeometryData.getFootprint();
                String crs = ceaGeometryData.getCrs();

                for (int j = 0; j < geometries.size(); j++) {
                    Geometry geometry = transformGeometry(geometries.get(j), "EPSG:" + crs, EPSG_4326);
                    geometries.set(j, geometry);
                }

                ceaGeometryData.setFootprint(geometries);

                // store only the integer part
                ceaGeometryData.setCrs(EPSG_4326.replaceAll("[^0-9]", ""));

                ceaGeometries.set(i, ceaGeometryData);
            }
        }
    }

    private static boolean checkSameCRS(List<CEAGeometryData> ceaGeometries) {
        List<String> crsList = ceaGeometries.stream()
                .map(CEAGeometryData::getCrs)
                .collect(Collectors.toList());
        return crsList.stream().allMatch(s -> s.equals(crsList.get(0)));
    }

    private static List<Geometry> insertHoles(List<Geometry>  geometries, List<LinearRing> holes) {
        List<Geometry> result = new ArrayList<>();

        for (LinearRing hole: holes) {
            int i = 0;
            while (i < geometries.size()) {
                Geometry geometry = geometries.get(i);
                if (geometry.contains(hole)) {
                    result.add(geometry.difference(hole));
                    geometries.remove(i);
                    break;
                }
                else {
                    i++;
                }
            }
        }

        result.addAll(geometries);

        return result;
    }
}
