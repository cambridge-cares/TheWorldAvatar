package uk.ac.cam.cares.jsp.integration;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.geotools.geometry.jts.JTSFactoryFinder;
import org.geotools.referencing.CRS;
import org.geotools.geometry.jts.JTS;
import org.locationtech.jts.geom.*;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.locationtech.jts.operation.buffer.BufferOp;
import org.locationtech.jts.operation.buffer.BufferParameters;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import java.util.*;

public class SpatialLink {

    private static final Logger LOGGER = LogManager.getLogger(SpatialLink.class);
    private PostgresClient postgresClient;

    List<GeoObject3D> allObject3D = new ArrayList<>();
    List<GeoObject2D> allObject2D = new ArrayList<>();

    public SpatialLink() {}

    public void findMatchedObjects() throws ParseException, FactoryException, TransformException {

        GeometryFactory geometryFactory = JTSFactoryFinder.getGeometryFactory();
        WKTReader reader = new WKTReader( geometryFactory );

        for (int i = 0; i < this.allObject3D.size(); i++) {
            GeoObject3D object3D = this.allObject3D.get(i);
            int srid3D = object3D.getEnvelope().getGeometry().getSrid();
            String coord3D = object3D.getEnvelope().getGeometry().getValue();
            Geometry envelope = createGeometry(coord3D);
            double refAreaRation = 0;
            for (int j = 0; j < this.allObject2D.size(); j++) {
                GeoObject2D object2D = this.allObject2D.get(j);
                int srid2D = object2D.getGeometry2D().getGeometry().getSrid();
                String geom2D = object2D.getGeometry2D().toString();
                geom2D = geom2D.split(";")[1];
                MultiPolygon polys2D = (MultiPolygon) reader.read(geom2D);

                if(srid3D != srid2D){
                    Geometry transGeom3D = Transform(envelope, srid3D, srid2D);
                    srid3D = transGeom3D.getSRID();
                    Coordinate[] reversedCoordinates = getReversedCoordinates(transGeom3D);
                    envelope = geometryFactory.createPolygon(reversedCoordinates);
                }

                if ((!polys2D.within(envelope)) || (!envelope.within(polys2D))){
                    if(envelope.intersects(polys2D)){
                        Geometry intersect = envelope.intersection(polys2D);
                        double areaRatio = 100.0*intersect.getArea() / polys2D.getArea();
                        System.out.println("ratio: "+areaRatio + "%");
                        if(areaRatio>60){
                            if((refAreaRation !=0 && refAreaRation<areaRatio) || refAreaRation==0){
                                object3D.setName(object2D.getName());
                                refAreaRation = areaRatio;
                            }
                        }
                    }
                }else{
                    object3D.setName(object2D.getName());
                }
            }
            if (object3D.getName() != null){
                object3D.updateName(object3D);
            }
        }

    }

    public List<Coordinate> str2coords(String st_geometry) {
        String[] pointXYZList = null;
        List<Coordinate> coords = new LinkedList<Coordinate>();
        st_geometry = st_geometry.replace("(", "");
        st_geometry = st_geometry.replace(")", "");

        if (st_geometry.contains(",")) {
            //System.out.println("====================== InputString is from POSTGIS");
            pointXYZList = st_geometry.split(",");

            for (int i = 0; i < pointXYZList.length; ++i) {
                String[] pointXYZ = pointXYZList[i].split(" ");
                List<String> coordinates = new LinkedList<String>(Arrays.asList(pointXYZ));
                coordinates.removeIf(String::isEmpty);
                //coordinates.removeAll(Arrays.asList(null, ""));
                pointXYZ = coordinates.toArray(new String[0]);
                if (pointXYZ.length == 2) {
                    coords.add(new Coordinate(Double.valueOf(pointXYZ[0]), Double.valueOf(pointXYZ[1])));
                } else if (pointXYZ.length == 3) {
                    coords.add(new Coordinate(Double.valueOf(pointXYZ[0]), Double.valueOf(pointXYZ[1]), Double.valueOf(pointXYZ[2])));
                } else {
                    System.out.println("InputString has no valid format");
                    return null;
                }
            }
        } else if (st_geometry.contains("#")) {
            //System.out.println("====================== InputString is from Blazegraph");
            pointXYZList = st_geometry.split("#");
            if (pointXYZList.length % 3 == 0) {
                // 3d coordinates
                for (int i = 0; i < pointXYZList.length; i = i + 3) {
                    coords.add(new Coordinate(Double.valueOf(pointXYZList[i]), Double.valueOf(pointXYZList[i + 1]), Double.valueOf(pointXYZList[i + 2])));
                }
            } else if (pointXYZList.length % 2 == 0) {
                // 2d coordinates
                for (int i = 0; i < pointXYZList.length; i = i + 2) {
                    coords.add(new Coordinate(Double.valueOf(pointXYZList[i]), Double.valueOf(pointXYZList[i + 1])));
                }
            }
        } else {
            System.out.println("InputString has no valid format");
            return null;
        }
        return coords;

    }

    public Geometry createGeometry(String coordlist, String geomtype, int dimension, int[] dimOfRings) {
        GeometryFactory fac = new GeometryFactory();
        Coordinate[] coordinates = str2coords(coordlist).toArray(new Coordinate[0]);

        Geometry geom = null;
        if (geomtype.equals("POLYGON")){
            if (dimOfRings.length >= 2){ // Polygon with Holes : LinearRing shell and LinearRing[] holes
                Coordinate[] shell_coords = Arrays.copyOfRange(coordinates, 0, dimOfRings[0] / dimension);
                LinearRing shell = fac.createLinearRing(shell_coords);
                ArrayList<LinearRing> holeslist = new ArrayList<>();
                int start = dimOfRings[0] / dimension;
                for (int k = 1; k < dimOfRings.length; ++k){
                    Coordinate[] holes_coords = Arrays.copyOfRange(coordinates, start, start + dimOfRings[k] /dimension);
                    holeslist.add(fac.createLinearRing(holes_coords));
                    start = start + dimOfRings[k] / dimension;
                }
                LinearRing[] holes = holeslist.toArray(new LinearRing[0]);
                geom = fac.createPolygon(shell, holes);
            } else if (dimOfRings.length == 1) {
                // Polygon without holes
                LinearRing shell = fac.createLinearRing(coordinates);
                geom = fac.createPolygon(shell);
            }
        }
        return geom;
    }

    public Geometry createGeometry(String coordlist) {
        GeometryFactory fac = new GeometryFactory();
        Geometry geom = fac.createPolygon(str2coords(coordlist).toArray(new Coordinate[0]));

        return geom;
    }

    public Geometry Transform(Geometry geom, int srcSRID, int dstSRID) {

        GeometryFactory fac = new GeometryFactory();
        Geometry sourceGeometry = fac.createGeometry(geom);

        Geometry targetGeometry = null;
        try {
            CoordinateReferenceSystem sourceCRS = CRS.decode("EPSG:" + srcSRID);
            CoordinateReferenceSystem targetCRS = CRS.decode("EPSG:" + dstSRID);
            MathTransform transform = CRS.findMathTransform(sourceCRS, targetCRS);
            targetGeometry = JTS.transform(sourceGeometry, transform);
            targetGeometry.setSRID(dstSRID);

        } catch (FactoryException | TransformException e) {
            e.printStackTrace();
        }
        return targetGeometry;
    }

    public Coordinate[] getReversedCoordinates(Geometry geometry) {

        Coordinate[] original = geometry.getCoordinates();
        Coordinate[] reversed = new Coordinate[original.length];

        for (int i = 0; i < original.length; i++) {
            reversed[i] = new Coordinate(original[i].y, original[i].x);
        }

        return reversed;
    }
}
