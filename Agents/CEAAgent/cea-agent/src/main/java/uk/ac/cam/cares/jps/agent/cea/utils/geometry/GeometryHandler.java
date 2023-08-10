package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import org.json.JSONArray;

import org.cts.CRSFactory;
import org.cts.crs.CoordinateReferenceSystem;
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
        WKTReader wktReader = new WKTReader();
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
                        Polygon polygon = (Polygon) wktReader.read(surfaceArray.getJSONObject(i).get("geometry").toString());
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
