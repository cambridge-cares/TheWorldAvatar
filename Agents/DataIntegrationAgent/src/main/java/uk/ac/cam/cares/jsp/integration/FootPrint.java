package uk.ac.cam.cares.jsp.integration;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.CoordinateSequenceFilter;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryCollection;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.util.GeometryFixer;
import org.locationtech.jts.operation.buffer.BufferOp;
import org.locationtech.jts.operation.buffer.BufferParameters;

/**
 * Workflow of footprint extraction:
 * 1. thematic-- true: query ground surface and extract footprint
 * 2. thematic-- false: identify surface types, then extract footprint
 */
public class FootPrint {

    List<GeoObject3D> allObject3D = new ArrayList<>();
    boolean thematic = false;
    String surfaceType = null;

    protected void proFootPrint(String[] config, String thematicParams, String surfaceType) throws SQLException {

        GeoObject3D object3D = new GeoObject3D();
        object3D.setConfig(config);
        this.allObject3D = object3D.getObject3D();   
        this.surfaceType = surfaceType;     
        if(thematicParams.equals("true")){
            this.thematic = true;
        }else{
            this.thematic = false;
            object3D.identifySurface(allObject3D, this.surfaceType);
        }
        
        object3D.extractPrint_thematic(allObject3D, this.surfaceType);
    }
    

    /**
     * Extracts the footprint of the building from its ground surface geometries
     * 
     * @param results JSONArray of the query results for ground surface geometries
     * @return footprint as a string
     */
    private LinearRing extractFootprint(List<Polygon> polygonList) {

        if (polygonList.size() == 1)
            return polygonList.get(0).getExteriorRing();

        double distance = 0.00001;
        double increment = 0.00001;

        Polygon footprintPolygon;
        LinearRing footprintRing;
        ArrayList<Geometry> geometries = new ArrayList<>();
        GeometryFactory geoFac = new GeometryFactory();
        GeometryCollection geoCol;
        Geometry merged;
        Geometry temp;

        for (int i = 0; i < polygonList.size(); i++) {
            temp = polygonList.get(i);
            if (!temp.isValid()) {
                temp = GeometryFixer.fix(temp);
            }
            geometries.add(temp);
        }

        geoCol = (GeometryCollection) geoFac.buildGeometry(geometries);

        merged = geoCol.union();

        while ((merged.getClass() != Polygon.class)
                || (deflatePolygon(merged, distance).getClass() != Polygon.class)) {
            distance += increment;

            for (int i = 0; i < geometries.size(); i++) {
                temp = inflatePolygon(geometries.get(i), distance);
                if (!temp.isValid()) {
                    temp = GeometryFixer.fix(temp);
                }
                geometries.set(i, temp);
            }

            geoCol = (GeometryCollection) geoFac.buildGeometry(geometries);
            merged = geoCol.union();
        }

        footprintPolygon = (Polygon) deflatePolygon(merged, distance);

        if (!footprintPolygon.isValid()) {
            footprintPolygon = (Polygon) GeometryFixer.fix(footprintPolygon);
        }

        footprintRing = footprintPolygon.getExteriorRing();

        return footprintRing;

    }


        /**
     * Inflates a polygon
     * 
     * @param geom     polygon geometry
     * @param distance buffer distance
     * @return inflated polygon
     */
    private Geometry inflatePolygon(Geometry geom, Double distance) {
        ArrayList<Double> zCoordinate = getPolygonZ(geom);
        BufferParameters bufferParameters = new BufferParameters();
        bufferParameters.setEndCapStyle(BufferParameters.CAP_ROUND);
        bufferParameters.setJoinStyle(BufferParameters.JOIN_MITRE);
        Geometry buffered = BufferOp.bufferOp(geom, distance, bufferParameters);
        buffered.setUserData(geom.getUserData());
        setPolygonZ(buffered, zCoordinate);
        return buffered;
    }

    /**
     * Deflates a polygon
     * 
     * @param geom     polygon geometry
     * @param distance buffer distance
     * @return deflated polygon
     */
    private Geometry deflatePolygon(Geometry geom, Double distance) {
        ArrayList<Double> zCoordinate = getPolygonZ(geom);
        BufferParameters bufferParameters = new BufferParameters();
        bufferParameters.setEndCapStyle(BufferParameters.CAP_ROUND);
        bufferParameters.setJoinStyle(BufferParameters.JOIN_MITRE);
        Geometry buffered = BufferOp.bufferOp(geom, distance * -1, bufferParameters);
        buffered.setUserData(geom.getUserData());
        setPolygonZ(buffered, zCoordinate);
        return buffered;
    }

     /**
     * Extract the z coordinates of the polygon vertices
     * 
     * @param geom polygon geometry
     * @return the z coordinates of the polygon vertices
     */
    private static ArrayList<Double> getPolygonZ(Geometry geom) {
        Coordinate[] coordinates = geom.getCoordinates();
        ArrayList<Double> output = new ArrayList<>();

        for (int i = 0; i < coordinates.length; i++) {
            output.add(coordinates[i].getZ());
        }

        return output;
    }

    /**
     * Sets a polygon's z coordinates to the values from zInput
     * 
     * @param geom   polygon geometry
     * @param zInput ArrayList of values representing z coordinates
     */
    private void setPolygonZ(Geometry geom, ArrayList<Double> zInput) {
        Double newZ = Double.NaN;

        for (int i = 0; i < zInput.size(); i++) {
            if (!zInput.get(i).isNaN()) {
                newZ = zInput.get(i);
                break;
            }
        }

        if (newZ.isNaN()) {
            newZ = 10.0;
        }

        if (geom.getNumPoints() < zInput.size()) {
            while (geom.getNumPoints() != zInput.size()) {
                zInput.remove(zInput.size() - 1);
            }
        } else {
            while (geom.getNumPoints() != zInput.size()) {
                zInput.add(1, newZ);
            }
        }

        Collections.replaceAll(zInput, Double.NaN, newZ);
        geom.apply(new CoordinateSequenceFilter() {
            @Override
            public void filter(CoordinateSequence cSeq, int i) {
                cSeq.getCoordinate(i).setZ(zInput.get(i));
            }

            @Override
            public boolean isDone() {
                return false;
            }

            @Override
            public boolean isGeometryChanged() {
                return false;
            }
        });
    }
}
