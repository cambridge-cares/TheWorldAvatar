package uk.ac.cam.cares.jps.agent.cea;

import org.cts.CRSFactory;
import org.cts.crs.CoordinateReferenceSystem;
import org.cts.crs.GeodeticCRS;
import org.cts.op.CoordinateOperation;
import org.cts.op.CoordinateOperationFactory;
import org.cts.registry.EPSGRegistry;
import org.cts.registry.RegistryManager;
import org.json.JSONArray;
import org.locationtech.jts.geom.*;
import org.locationtech.jts.geom.util.GeometryFixer;
import org.locationtech.jts.operation.buffer.BufferOp;
import org.locationtech.jts.operation.buffer.BufferParameters;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class GeometryHelper {

    /**
     * Finds footprint of building from array of building surfaces by searching for constant minimum z in geometries
     * NB. On data TSDA has run on, the thematic surface is labelled with a ground surface id so this is not required
     * @param surfaceArray array of building surfaces
     * @return ground geometry in a JSONArray
     */
    public static JSONArray getGroundGeometry(JSONArray surfaceArray) {
        ArrayList<Integer> ind = new ArrayList<>();
        ArrayList<Double> z;
        Double minZ = Double.MAX_VALUE;
        double eps = 0.5;
        boolean flag;
        String geom;
        String[] split;

        for (int i = 0; i < surfaceArray.length(); i++){
            geom = surfaceArray.getJSONObject(i).get("geometry").toString();
            split = geom.split("#");

            z = new ArrayList<>();
            z.add(Double.parseDouble(split[2]));

            flag = true;

            for (int j = 5; j < split.length; j += 3) {
                for (int ji = 0;  ji < z.size(); ji++) {
                    if (Math.abs(Double.parseDouble(split[j]) - z.get(ji)) > eps){
                        flag = false;
                        break;
                    }
                }

                if (flag){
                    z.add(Double.parseDouble(split[j]));
                }
                else{
                    break;
                }
            }

            if (!flag){
                ind.add(i);
            }
            else{
                if (z.get(0) < minZ){minZ = z.get(0);}
            }
        }

        // gets rid of geometry without constant z (up to eps tolerance)
        for (int i = ind.size() - 1; i >= 0; i--){
            surfaceArray.remove(ind.get(i));
        }

        int i = 0;

        // gets rid of geometry without minimum z value (up to eps tolerance)
        while (i < surfaceArray.length()){
            geom = surfaceArray.getJSONObject(i).get("geometry").toString();
            split = geom.split("#");

            if (Double.parseDouble(split[2]) > minZ + eps){
                surfaceArray.remove(i);
            }
            else{
                i++;
            }
        }

        return surfaceArray;
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

        if (surfaceArray.length() == 1) {
            stringToGeometries(surfaceArray.getJSONObject(0).get("geometry").toString(), surfaceArray.getJSONObject(0).get("datatype").toString(), exteriors, holes);
            exteriorPolygon = exteriors.get(0);
            exteriorRing = exteriorPolygon.getExteriorRing();
        }
        else {
            for (int i = 0; i < surfaceArray.length(); i++) {
                stringToGeometries(surfaceArray.getJSONObject(i).get("geometry").toString(), surfaceArray.getJSONObject(i).get("datatype").toString(), exteriors, holes);
            }

            GeometryCollection geoCol = (GeometryCollection) geometryFactory.buildGeometry(exteriors);

            Geometry merged = geoCol.union();

            geoType = merged.getGeometryType();

            while (!geoType.equals("Polygon") || !deflatePolygon(merged, distance).getGeometryType().equals("Polygon")) {
                distance += increment;

                for (int i = 0; i < exteriors.size(); i++) {
                    Polygon temp = (Polygon) inflatePolygon(exteriors.get(i), distance);
                    if (!temp.isValid()) {
                        temp = (Polygon) GeometryFixer.fix(temp);
                    }
                    exteriors.set(i, temp);
                }

                geoCol = (GeometryCollection) geometryFactory.buildGeometry(exteriors);
                merged = geoCol.union();
                geoType = merged.getGeometryType();
            }

            exteriorPolygon = (Polygon) deflatePolygon(merged, distance);

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

    /**
     * Creates a polygon with the given points
     * @param points points of the polygon as a string
     * @return a polygon
     */
    public static Geometry toPolygon(String points) {
        int ind = 0;
        GeometryFactory geometryFactory = new GeometryFactory();

        String[] arr = points.split("#");

        Coordinate[] coordinates = new Coordinate[(arr.length) / 3];

        for (int i = 0; i < arr.length; i += 3){
            coordinates[ind] = new Coordinate(Double.valueOf(arr[i]), Double.valueOf(arr[i+1]), Double.valueOf(arr[i+2]));
            ind++;
        }

        return geometryFactory.createPolygon(coordinates);
    }

    /**
     * Converts geometry strings to geometry objects
     * @param geometry geometry string
     * @param polygonType string describing the polygon type of geometry, in the format of POLYGON-3-15
     * @param exteriors list to store the exterior geometry
     * @param holes list to store the hole geometries
     */
    public static void stringToGeometries(String geometry, String polygonType, List<Polygon> exteriors, List<LinearRing> holes) {
        GeometryFactory geometryFactory = new GeometryFactory();
        int ind = 0;
        int k = 0;

        String[] points = geometry.split("#");
        String[] split = polygonType.split("-");

        int num = Integer.parseInt(split[1]);

        // exterior ring
        Coordinate[] exterior = new Coordinate[Integer.parseInt(split[2])/num];

        while (ind < Integer.parseInt(split[2])) {
            exterior[k] = new Coordinate(Double.parseDouble(points[ind]), Double.parseDouble(points[ind+1]));
            ind += 3;
            k++;
        }
        exteriors.add(geometryFactory.createPolygon(exterior));

        int sum = Integer.parseInt(split[2]);
        // holes
        for (int i = 3; i < split.length; i++){
            Coordinate[] hole = new Coordinate[Integer.parseInt(split[i])/num];
            k = 0;
            sum += Integer.parseInt(split[i]);
            while (ind < sum) {
                hole[k] = new Coordinate(Double.parseDouble(points[ind]), Double.parseDouble(points[ind+1]));
                ind += 3;
                k++;
            }
            holes.add(geometryFactory.createLinearRing(hole));
        }
    }

    /**
     * Converts an array of coordinates into a string
     * @param coordinates array of footprint coordinates
     * @return coordinates as a string
     */
    public static String coordinatesToString(Coordinate[] coordinates) {
        String output = "";

        for (int i = 0; i < coordinates.length; i++){
            output = output + "#" + Double.toString(coordinates[i].getX()) + "#" + Double.toString(coordinates[i].getY()) + "#" + Double.toString(coordinates[i].getZ());
        }

        return output.substring(1, output.length());
    }

    /**
     * Inflates a polygon
     * @param geom polygon geometry
     * @param distance buffer distance
     * @return inflated polygon
     */
    public static Geometry inflatePolygon(Geometry geom, Double distance) {
        BufferParameters bufferParameters = new BufferParameters();
        bufferParameters.setEndCapStyle(BufferParameters.CAP_ROUND);
        bufferParameters.setJoinStyle(BufferParameters.JOIN_MITRE);
        Geometry buffered = BufferOp.bufferOp(geom, distance, bufferParameters);
        buffered.setUserData(geom.getUserData());
        return buffered;
    }

    /**
     * Deflates a polygon
     * @param geom polygon geometry
     * @param distance buffer distance
     * @return deflated polygon
     */
    public static Geometry deflatePolygon(Geometry geom, Double distance) {
        BufferParameters bufferParameters = new BufferParameters();
        bufferParameters.setEndCapStyle(BufferParameters.CAP_ROUND);
        bufferParameters.setJoinStyle(BufferParameters.JOIN_MITRE);
        Geometry buffered = BufferOp.bufferOp(geom, distance * -1, bufferParameters);
        buffered.setUserData(geom.getUserData());
        return buffered;
    }

    /**
     * Transform a coordinate from sourceCRS to targetCRS
     * @param coordinate coordinate to be transformed
     * @param sourceCRS source crs of coordinate
     * @param targetCRS target crs for coordinate transformation
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
}
