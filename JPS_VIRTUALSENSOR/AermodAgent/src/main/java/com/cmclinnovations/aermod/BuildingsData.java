package com.cmclinnovations.aermod;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.CoordinateSequenceFilter;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryCollection;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.geom.util.GeometryFixer;
import org.locationtech.jts.operation.buffer.BufferOp;
import org.locationtech.jts.operation.buffer.BufferParameters;

import com.cmclinnovations.aermod.objects.Building;
import com.cmclinnovations.aermod.objects.PointSource;
import com.cmclinnovations.aermod.objects.StaticPointSource;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class BuildingsData {

    private static final Logger LOGGER = LogManager.getLogger(BuildingsData.class);
    private int srid;
    private QueryClient queryClient;

    public BuildingsData(String namespaceCRS, QueryClient queryClient) {
        this.queryClient = queryClient;
        this.srid = Integer.parseInt(namespaceCRS.substring(namespaceCRS.indexOf("EPSG:") + 5));
    }

    public void setStaticPointSourceProperties(
            List<StaticPointSource> staticPointSources) {

        Map<String, StaticPointSource> iriToPointSourceMap = new HashMap<>();

        staticPointSources.stream()
                .filter(s -> s.getCityObjectType() == StaticPointSource.CityObjectType.CITY_FURNITURE)
                .forEach(s -> iriToPointSourceMap.put(s.getOcgmlIri(), s));

        setCityFurnitureProperties(iriToPointSourceMap);
        iriToPointSourceMap.clear();
        staticPointSources.stream()
                .filter(s -> s.getCityObjectType() == StaticPointSource.CityObjectType.BUILDING)
                .forEach(s -> iriToPointSourceMap.put(s.getOcgmlIri(), s));
        setBuildingProperties(iriToPointSourceMap);
    }

    // This method is called for static point sources whose OCGML representation is
    // a cityfurniture object.

    private void setCityFurnitureProperties(Map<String, StaticPointSource> iriToPointSourceMap) {

        List<String> ocgmlIRIList = new ArrayList<>(iriToPointSourceMap.keySet());
        Map<String, List<Polygon>> iriToPolygonMap = queryClient.cityFurnitureQuery(ocgmlIRIList);

        for (String ocgmlIRI : ocgmlIRIList) {
            List<Polygon> polygonData = iriToPolygonMap.get(ocgmlIRI);
            // Go through the set of polygons to retrieve the following:
            // 1. maximum and minimum z-coordinates of all vertices across all polygons
            // (minZ & maxZ).
            // 2. base polygon

            Polygon basePolygon = null;
            final double minZ = polygonData.stream().flatMap(polygon -> Arrays.stream(polygon.getCoordinates()))
                    .mapToDouble(Coordinate::getZ).min().orElse(0.0);
            final double maxZ = polygonData.stream().flatMap(polygon -> Arrays.stream(polygon.getCoordinates()))
                    .mapToDouble(Coordinate::getZ).max().orElse(0.0);

            List<Polygon> basePolygons = polygonData.stream().filter(polygon -> {
                double polyMinZ = Arrays.stream(polygon.getCoordinates()).mapToDouble(Coordinate::getZ).min()
                        .orElse(minZ);
                double polyMaxZ = Arrays.stream(polygon.getCoordinates()).mapToDouble(Coordinate::getZ).max()
                        .orElse(maxZ);
                return (polyMinZ == polyMaxZ) && (minZ == polyMinZ);
            }).collect(Collectors.toList());

            if (basePolygons.size() == 1)
                basePolygon = basePolygons.get(0);
            else {
                LOGGER.error("Could not identify base polygon for city furniture object with OCGML IRI {}", ocgmlIRI);
                throw new JPSRuntimeException("Error in BuildingsData class.");
            }

            double height = maxZ - minZ;

            basePolygon.setSRID(srid);
            Point centre = basePolygon.getCentroid();
            centre.setSRID(srid);
            Coordinate[] baseCoords = basePolygon.getCoordinates();

            double radius = Arrays.stream(baseCoords)
                    .mapToDouble(d -> centre.distance(new GeometryFactory().createPoint(d))).average().orElse(0.0);

            StaticPointSource ps = iriToPointSourceMap.get(ocgmlIRI);
            ps.setLocation(centre);
            ps.setHeight(height);
            ps.setDiameter(2.0 * radius);

        }

    }

    // This method is called for static point sources whose OCGML representation is
    // a building.
    private void setBuildingProperties(Map<String, StaticPointSource> iriToPointSourceMap) {

        List<String> ocgmlIRIList = new ArrayList<>(iriToPointSourceMap.keySet());
        Map<String, List<List<Polygon>>> iriToPolygonMap = queryClient.buildingsQuery(ocgmlIRIList);

        for (String ocgmlIRI : ocgmlIRIList) {
            List<Polygon> groundPolygons = iriToPolygonMap.get(ocgmlIRI).get(0);
            List<Polygon> roofPolygons = iriToPolygonMap.get(ocgmlIRI).get(1);

            StaticPointSource ps = iriToPointSourceMap.get(ocgmlIRI);

            LinearRing basePolygon = extractFootprint(groundPolygons);
            basePolygon.setSRID(srid);
            Coordinate[] baseCoords = basePolygon.getCoordinates();
            Point centre = basePolygon.getCentroid();
            centre.setSRID(srid);

            double radius = Arrays.stream(baseCoords)
                    .mapToDouble(d -> centre.distance(new GeometryFactory().createPoint(d))).average().orElse(0.0);

            double aveGroundZ = groundPolygons.stream()
                    .flatMap(polygon -> Arrays.stream(polygon.getCoordinates()))
                    .mapToDouble(Coordinate::getZ).average().orElse(0.0);

            double aveRoofZ = roofPolygons.stream()
                    .flatMap(polygon -> Arrays.stream(polygon.getCoordinates()))
                    .mapToDouble(Coordinate::getZ).average().orElse(0.0);
            double height = aveRoofZ - aveGroundZ;

            ps.setLocation(centre);
            ps.setDiameter(2.0 * radius);
            ps.setHeight(height);

        }

    }

    // This method is called to set properties of buildings that are near static
    // point sources but are not emitting
    // points.
    public List<Building> getBuildings(List<PointSource> allSources) throws ParseException {

        Map<String, List<List<Polygon>>> iriToPolygonMap = queryClient
                .getBuildingsNearPollutantSources(allSources);

        List<String> ocgmlIRList = new ArrayList<>(iriToPolygonMap.keySet());

        List<Building> buildings = new ArrayList<>();

        for (String ocgmlIRI : ocgmlIRList) {
            List<Polygon> groundPolygons = iriToPolygonMap.get(ocgmlIRI).get(0);
            List<Polygon> roofPolygons = iriToPolygonMap.get(ocgmlIRI).get(1);
            LinearRing basePolygon = extractFootprint(groundPolygons);
            basePolygon.setSRID(srid);

            double aveGroundZ = groundPolygons.stream()
                    .flatMap(polygon -> Arrays.stream(polygon.getCoordinates()))
                    .mapToDouble(Coordinate::getZ).average().orElse(0.0);

            double aveRoofZ = roofPolygons.stream()
                    .flatMap(polygon -> Arrays.stream(polygon.getCoordinates()))
                    .mapToDouble(Coordinate::getZ).average().orElse(0.0);

            double height = aveRoofZ - aveGroundZ;

            Building bd = new Building(basePolygon, height);
            bd.setIri(ocgmlIRI);
            buildings.add(bd);

        }

        return buildings;

    }

    // helper methods

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

}
