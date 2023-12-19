package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;

import org.cts.crs.CRSException;
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

import org.json.JSONArray;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class GeometryHandler {
    public static final String EPSG_4326 = "EPSG:4326";
    public static final Integer METER_EPSG = 3395;
    public static final String METER_EPSG_STRING = "EPSG:" + METER_EPSG;
    public static final Double FLOOR_HEIGHT = 3.2;

    /**
     * Parses a WKT string to a geometry object
     * @param geometryString WKT string
     * @return geometryString as a geometry object
     */
    public static Geometry toGeometry(String geometryString) {
        return WKTReader.extract(geometryString).getGeometry();
    }

    /**
     * Buffers a geometry object
     * @param geom geometry object
     * @param sourceCRS source CRS of geometry with EPSG prefix
     * @param distance buffer distance
     * @return buffered geometry object
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
     * Inflates a geometry object
     * @param geom geometry object
     * @param distance buffer distance
     * @return inflated geometry object
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
     * Transforms a geometry object from sourceCRS to targetCRS
     * @param geometry geometry object
     * @param sourceCRS source CRS of geometry with EPSG prefix
     * @param targetCRS target CRS for geometry transformation with EPSG prefix
     * @return the transformed geometry in targetCRS
     */
    public static Geometry transformGeometry(Geometry geometry, String sourceCRS, String targetCRS) throws Exception {
        Coordinate[] coordinates = geometry.getCoordinates();
        Coordinate[] transformed = new Coordinate[coordinates.length];

        if (sourceCRS.equals(targetCRS)) {
            return geometry;
        }

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
     * Extracts the footprint given a JSONArray of building surface geometries as WKT strings
     * @param surfaceArray JSONArray of building surface geometries as WKT strings
     * @param originalCRS CRS of surfaceArray as String
     * @param height building height
     * @return list of geometry objects representing the building footprint
     */
    public static List<Geometry> extractFootprint(JSONArray surfaceArray, String originalCRS, Double height) {
        double distance = 0.0;
        double increment = 0.00005;
        double limit = 0.001;

        List<Polygon> exteriors = new ArrayList<>();
//        List<LinearRing> holes = new ArrayList<>();
        GeometryFactory geometryFactory = new GeometryFactory();
        String geoType;

        List<Geometry> result = new ArrayList<>();

        originalCRS = "EPSG:" + originalCRS;

        String crs = EPSG_4326;

        if (surfaceArray.length() == 1) {
            String wkt = surfaceArray.getJSONObject(0).getString("wkt");

            Geometry temp = toGeometry(wkt);

            try {
                result.add(transformGeometry(temp, originalCRS, EPSG_4326));

                return result;
            }
            catch (Exception e) {
                e.printStackTrace();
                throw new JPSRuntimeException(e);
            }
        }
        else {
            for (int i = 0; i < surfaceArray.length(); i++) {
                String wkt = surfaceArray.getJSONObject(i).getString("wkt");

                // create Polygon object from WKT string
                Polygon polygon = (Polygon) toGeometry(wkt);

                try {
                    polygon = (Polygon) transformGeometry(polygon, originalCRS, EPSG_4326);
                }
                catch (Exception e) {
                    e.printStackTrace();
                    throw new JPSRuntimeException(e);
                }

                // get exterior ring for buffering
                exteriors.add(extractExterior(polygon));

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
                geometries.add(extractExterior((Polygon) bufferPolygon(merged, crs, -1 * distance)));
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
                        geometries.add(extractExterior((Polygon) bufferPolygon(geometry, crs, -1 * distance)));
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

    /**
     * Extracts and returns the exterior ring of a polygon object
     * @param polygon polygon object
     * @return polygon's exterior ring as a polygon object
     */
    public static Polygon extractExterior(Polygon polygon) {
        return new GeometryFactory().createPolygon(polygon.getExteriorRing());
    }

    /**
     * Swaps the coordinates of a polygon object
     * @param polygon polygon object
     * @return polygon with its coordinates swapped
     */
    public static Polygon swapCoordinates(Geometry polygon) {
        Coordinate[] coordinates = polygon.getCoordinates();

        for (Coordinate coordinate : coordinates) {
            double temp = coordinate.getX();
            coordinate.setX(coordinate.getY());
            coordinate.setY(temp);
        }

        coordinates[0] = coordinates[coordinates.length-1];

        return new GeometryFactory().createPolygon(coordinates);
    }

    /**
     * Check if the CEAGeometryDatas in ceaGeometries have the same CRS
     * @param ceaGeometries list of CEAGeometryDatas
     * @return true if CEAGeometryDatas in ceaGeometries have the same CRS, false otherwise
     */
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
