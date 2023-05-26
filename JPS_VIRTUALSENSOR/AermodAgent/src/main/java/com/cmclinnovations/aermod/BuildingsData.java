package com.cmclinnovations.aermod;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
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
import com.cmclinnovations.aermod.objects.StaticPointSource;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class BuildingsData {

    private static final Logger LOGGER = LogManager.getLogger(BuildingsData.class);
    private String citiesNamespace;
    private String namespaceCRS;
    private QueryClient queryClient;

    public BuildingsData(String citiesNamespace, String namespaceCRS, QueryClient queryClient) {
        this.citiesNamespace = citiesNamespace;
        this.namespaceCRS = namespaceCRS;
        this.queryClient = queryClient;
    }

    public void setStaticPointSourceProperties(
            List<StaticPointSource> staticPointSources) {
        Map<String, StaticPointSource> iriToPointSourceMap = new HashMap<>();

        String objectType = null;

        StaticPointSource firstSource = staticPointSources.get(0);

        if (firstSource.getOcgmlIri().contains("cityfurniture")) {
            objectType = "cityfurniture";
        } else if (firstSource.getOcgmlIri().contains("building")) {
            objectType = "building";
        }
        iriToPointSourceMap.put(firstSource.getOcgmlIri(), firstSource);

        for (int i = 1; i < staticPointSources.size(); i++) {
            StaticPointSource ps = staticPointSources.get(i);
            String ontoCityGMLIRI = ps.getOcgmlIri();
            if (ontoCityGMLIRI.contains(objectType)) {
                iriToPointSourceMap.put(ontoCityGMLIRI, ps);
            } else {
                LOGGER.error("Static point source with IRI " + staticPointSources.get(i).getIri()
                        + " has invalid OntoCityGML IRI. The OntoCityGML IRI is " + ontoCityGMLIRI);
                throw new JPSRuntimeException(
                        "Invalid OCGML IRI encountered when calculating static point source prperties.");
            }
        }

        if (objectType.equals("cityfurniture")) {
            setCityFurnitureProperties(iriToPointSourceMap);
        } else {
            setBuildingProperties(iriToPointSourceMap);
        }

    }

    private void setCityFurnitureProperties(Map<String, StaticPointSource> iriToPointSourceMap) {

        List<String> ocgmlIRIList = new ArrayList<>(iriToPointSourceMap.keySet());
        Map<String, List<Polygon>> iriToPolygonMap = queryClient.cityFurnitureQuery(ocgmlIRIList);

        for (String ocgmlIRI : ocgmlIRIList) {
            List<Polygon> polygonData = iriToPolygonMap.get(ocgmlIRI);
            // Go through the set of polygons to retrieve the following:
            // 1. maximum and minimum z-coordinates of all vertices across all polygons
            // (minZ & maxZ).
            // 2. base polygon
            double minZ = 0.0;
            double maxZ = 0.0;
            double minAveZ = 0.0;
            Polygon basePolygon = null;

            for (int i = 0; i < polygonData.size(); i++) {
                Polygon poly = polygonData.get(i);
                Coordinate[] coord = poly.getCoordinates();

                double polyMinZ = coord[0].z;
                double polyMaxZ = coord[0].z;
                double polyAveZ = 0;

                for (int j = 0; j < coord.length; j++) {
                    double zc = coord[j].z;
                    polyMinZ = Math.min(polyMinZ, zc);
                    polyMaxZ = Math.max(polyMaxZ, zc);
                    polyAveZ += zc;
                }

                polyAveZ /= coord.length;

                if (i == 0) {
                    minZ = polyMinZ;
                    maxZ = polyMaxZ;
                    minAveZ = polyAveZ;
                } else {
                    minZ = Math.min(minZ, polyMinZ);
                    maxZ = Math.max(maxZ, polyMaxZ);
                    minAveZ = Math.min(minAveZ, polyAveZ);
                }

                if (polyMinZ == polyMaxZ && minZ == polyMinZ) {
                    basePolygon = poly;
                }

            }

            if (basePolygon == null) {
                LOGGER.error("Could not identify base polygon for city furniture object with OCGML IRI " + ocgmlIRI);
                throw new JPSRuntimeException("Error in BuildingsData class.");
            }

            double height = maxZ - minZ;
            Point centre = basePolygon.getCentroid();
            Coordinate[] baseCoords = basePolygon.getCoordinates();

            double radius = 0.0;

            for (int i = 0; i < baseCoords.length; i++) {
                double dx = baseCoords[i].getX() - centre.getX();
                double dy = baseCoords[i].getX() - centre.getY();
                radius += Math.sqrt(dx * dx + dy * dy);
            }

            radius /= baseCoords.length;

            StaticPointSource ps = iriToPointSourceMap.get(ocgmlIRI);
            ps.setLocation(basePolygon.getCentroid());
            ps.setHeight(height);
            ps.setDiameter(2.0 * radius);

        }

    }

    // This method is to be called for

    private void setBuildingProperties(Map<String, StaticPointSource> iriToPointSourceMap) {

        List<String> ocgmlIRIList = new ArrayList<>(iriToPointSourceMap.keySet());
        Map<String, List<List<Polygon>>> iriToPolygonMap = queryClient.buildingsQuery(ocgmlIRIList);

        for (String ocgmlIRI : ocgmlIRIList) {
            List<Polygon> groundPolygons = iriToPolygonMap.get(ocgmlIRI).get(0);
            List<Polygon> roofPolygons = iriToPolygonMap.get(ocgmlIRI).get(1);

            StaticPointSource ps = iriToPointSourceMap.get(ocgmlIRI);

            if (groundPolygons.size() == 1) {
                Polygon basePolygon = groundPolygons.get(0);
                Coordinate[] baseCoords = basePolygon.getCoordinates();
                Point centre = basePolygon.getCentroid();
                double radius = 0.0;
                double aveGroundZ = 0.0;

                for (int i = 0; i < baseCoords.length; i++) {
                    double dx = baseCoords[i].getX() - centre.getX();
                    double dy = baseCoords[i].getX() - centre.getY();
                    radius += Math.sqrt(dx * dx + dy * dy);
                    aveGroundZ += baseCoords[i].getZ();
                }

                radius /= baseCoords.length;
                aveGroundZ /= baseCoords.length;

   

                if (roofPolygons.size() == 1) {
                    Polygon roofPolygon = roofPolygons.get(0);
                    Coordinate[] roofCoords = roofPolygon.getCoordinates();
                    double aveRoofZ = 0.0;
                    for (int i = 0; i < roofCoords.length; i++) {
                        aveRoofZ += roofCoords[i].getZ();
                    }
                    aveRoofZ /= roofCoords.length;
                    double height = aveRoofZ - aveGroundZ;
                    ps.setLocation(basePolygon.getCentroid());
                    ps.setDiameter(2.0 * radius);
                    ps.setHeight(height);
                } else {
                    // Not sure if this scenario may arise. Not allowing it for now. 
                    LOGGER.error("Building with OCGML IRI" + ocgmlIRI + " has a single ground surface but multiple roof surfaces. ");
                    throw new JPSRuntimeException("Error in BuildingsData class. Found building with single ground surface but multiple roof surfaces.")
                }

            } else {
                // Multiple roof and ground polygons.

                LinearRing basePolygon = extractFootprint(groundPolygons);
                Coordinate[] baseCoords = basePolygon.getCoordinates();
                Point centre = basePolygon.getCentroid();
                double radius = 0.0;
                double aveGroundZ = 0.0;

                for (int i = 0; i < baseCoords.length; i++) {
                    double dx = baseCoords[i].getX() - centre.getX();
                    double dy = baseCoords[i].getX() - centre.getY();
                    radius += Math.sqrt(dx * dx + dy * dy);
                    aveGroundZ += baseCoords[i].getZ();
                }

                radius /= baseCoords.length;
                aveGroundZ /= baseCoords.length;
                

                double aveRoofZ = 0.0;
                for (int i = 0; i < roofPolygons.size(); i++) {
                    Coordinate[] roofCoordinates = roofPolygons.get(i).getCoordinates();
                    double polyAveZ = 0.0;
                    for (int j = 0; j < roofCoordinates.length; j++) {
                        polyAveZ += roofCoordinates[j].getZ();
                    }
                    polyAveZ /= roofCoordinates.length;
                    aveRoofZ += polyAveZ;
                }
                aveRoofZ /= roofPolygons.size();

                double height = aveRoofZ - aveGroundZ;

                ps.setLocation(basePolygon.getCentroid());
                ps.setDiameter(2.0 * radius);
                ps.setHeight(height);                

            }
        }

    }

    private void setBuildingData(Map<String, Building> iriToBuildingsMap) {

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
     * Returns the ground geometry's exterior ring
     * 
     * @param geometry    ground geometry
     * @param polygonType polygon datatype, such as "<...\POLYGON-3-45-15>"
     * @return ground geometry with no holes
     */
    private String ignoreHole(String geometry, String polygonType) {
        int num;
        int ind;
        int count = 1;

        String[] split = polygonType.split("-");

        if (split.length < 4) {
            return geometry;
        }

        num = Integer.parseInt(split[2]);

        ind = geometry.indexOf("#");

        while (count != num) {
            ind = geometry.indexOf("#", ind + 1);
            count++;
        }
        return geometry.substring(0, ind);
    }

    /**
     * Extracts the footprint of the building from its ground surface geometries
     * 
     * @param results JSONArray of the query results for ground surface geometries
     * @return footprint as a string
     */
    private LinearRing extractFootprint(List<Polygon> polygonList) {
        double distance = 0.00001;
        double increment = 0.00001;

        Polygon footprintPolygon;
        LinearRing footprintRing;
        Coordinate[] footprintCoordinates;
        ArrayList<Geometry> geometries = new ArrayList<>();
        GeometryFactory geoFac = new GeometryFactory();
        GeometryCollection geoCol;
        Geometry merged;
        Geometry temp;
        String geoType;

        for (int i = 0; i < polygonList.size(); i++) {
            temp = polygonList.get(i);
            if (!temp.isValid()) {
                temp = GeometryFixer.fix(temp);
            }
            geometries.add(temp);
        }

        geoCol = (GeometryCollection) geoFac.buildGeometry(geometries);

        merged = geoCol.union();

        geoType = merged.getGeometryType();

        while (geoType != "Polygon" || deflatePolygon(merged, distance).getGeometryType() != "Polygon") {
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
            geoType = merged.getGeometryType();
        }

        footprintPolygon = (Polygon) deflatePolygon(merged, distance);

        if (!footprintPolygon.isValid()) {
            footprintPolygon = (Polygon) GeometryFixer.fix(footprintPolygon);
        }
    

        footprintRing = footprintPolygon.getExteriorRing();

        return footprintRing;

  
}
