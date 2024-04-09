package com.cmclinnovations.aermod.objects;

import java.util.ArrayList;
import java.util.List;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryCollection;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.util.GeometryFixer;
import org.locationtech.jts.operation.buffer.BufferOp;
import org.locationtech.jts.operation.buffer.BufferParameters;

import uk.ac.cam.cares.jps.base.util.CRSTransformer;

public class Building {
    private String iri;
    private LinearRing footPrint;
    private double height;
    private Point location;
    private double baseElevation = 0.0;

    public Building(LinearRing footPrint, double height) {
        this.footPrint = footPrint;
        this.height = height;
        this.location = footPrint.getCentroid();
    }

    public Building(List<Polygon> footPrintPolygons, double height) {
        footPrint = extractFootprint(footPrintPolygons);
        this.height = height;
        this.location = footPrint.getCentroid();
    }

    public LinearRing getFootprint() {
        return footPrint;
    }

    public double getHeight() {
        return height;
    }

    public String getSrid() {
        return "EPSG:" + footPrint.getSRID();
    }

    public void setElevation(double elevation) {
        this.baseElevation = elevation;
    }

    public double getElevation() {
        return baseElevation;
    }

    public Point getLocation() {
        return location;
    }

    public void setIri(String iri) {
        this.iri = iri;
    }

    public String getIri() {
        return iri;
    }

    // for StaticPointSource
    public double getRadius() {
        Coordinate[] baseCoords = footPrint.getCoordinates();

        // ensure coordinates are in meters
        List<Coordinate> coordinatesInMeter = new ArrayList<>();
        for (Coordinate coordinate : baseCoords) {
            double[] xyOriginal = { coordinate.getX(), coordinate.getY() };
            double[] xyTransformed = CRSTransformer.transform(getSrid(), "EPSG:3857", xyOriginal);
            coordinatesInMeter.add(new Coordinate(xyTransformed[0], xyTransformed[1]));
        }

        double[] centroidOriginal = { footPrint.getCentroid().getX(), footPrint.getCentroid().getY() };
        double[] centroidTransformed = CRSTransformer.transform(getSrid(), "EPSG:3857", centroidOriginal);
        Point centroidAsPoint = new GeometryFactory()
                .createPoint(new Coordinate(centroidTransformed[0], centroidTransformed[1]));

        return coordinatesInMeter.stream()
                .mapToDouble(d -> centroidAsPoint.distance(new GeometryFactory().createPoint(d))).average().orElse(0.0);
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
     * Deflates a polygon
     * 
     * @param geom     polygon geometry
     * @param distance buffer distance
     * @return deflated polygon
     */
    private Geometry deflatePolygon(Geometry geom, Double distance) {
        BufferParameters bufferParameters = new BufferParameters();
        bufferParameters.setEndCapStyle(BufferParameters.CAP_ROUND);
        bufferParameters.setJoinStyle(BufferParameters.JOIN_MITRE);
        Geometry buffered = BufferOp.bufferOp(geom, distance * -1, bufferParameters);
        buffered.setUserData(geom.getUserData());
        return buffered;
    }

    /**
     * Inflates a polygon
     * 
     * @param geom     polygon geometry
     * @param distance buffer distance
     * @return inflated polygon
     */
    private Geometry inflatePolygon(Geometry geom, Double distance) {
        BufferParameters bufferParameters = new BufferParameters();
        bufferParameters.setEndCapStyle(BufferParameters.CAP_ROUND);
        bufferParameters.setJoinStyle(BufferParameters.JOIN_MITRE);
        Geometry buffered = BufferOp.bufferOp(geom, distance, bufferParameters);
        buffered.setUserData(geom.getUserData());
        return buffered;
    }
}
