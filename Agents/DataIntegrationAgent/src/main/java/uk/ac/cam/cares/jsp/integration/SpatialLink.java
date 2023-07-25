package uk.ac.cam.cares.jsp.integration;
/**
 * Set name of geoobject (3D model) based on reference data (2D geospatial data with metatdata)
 * All data in different database of PostgreSQL
 * Parameter:
 * 1. database names of 3D model and reference data
 * 2. database name of 2D data
 * 3. table name of 2D data
 * @author Jingya yan
 *
 */

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.geotools.geometry.jts.JTS;
import org.geotools.geometry.jts.JTSFactoryFinder;
import org.geotools.referencing.CRS;
import org.locationtech.jts.geom.*;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;
import org.postgis.PGgeometry;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.*;


public class SpatialLink{

    private static final Logger LOGGER = LogManager.getLogger(SpatialLink.class);
    // private PostgresClient postgresClient2d;
    // private PostgresClient postgresClient3d;
    private static final String INVALID_CONNECTION_MESSAGE = "Connection is invalid...";
    private SqlConnectionPool pool;
    private String[] config;

    List<GeoObject3D> allObject3D = new ArrayList<>();
    List<GeoObject2D> allObject2D = new ArrayList<>();

    protected void SpatialLink(String[] config) throws SQLException {
        this.config = config;

        GeoObject3D object3D = new GeoObject3D();
        GeoObject2D object2D = new GeoObject2D();

        this.allObject2D = object2D.getObject2D(config);
        this.allObject3D = object3D.getObject3D(config);
        try {
            findMatchedObjects(config);
        } catch (ParseException | FactoryException | TransformException | SQLException e) {
            throw new RuntimeException(e);
        }


    }
    public void findMatchedObjects(String[] config) throws ParseException, FactoryException, TransformException, SQLException {

        // GeometryFactory geometryFactory = JTSFactoryFinder.getGeometryFactory();
        GeometryFactory fac = new GeometryFactory();
        WKTReader reader = new WKTReader( fac );
        ObjectAddress address = new ObjectAddress();

        int srid2D = this.allObject2D.get(0).getGeometry2D().getGeometry().getSrid();
        int srid3D = this.allObject3D.get(0).getSrid(this.allObject3D.get(0).getGeometry3D());
        if(srid3D != srid2D){
            reprojectPost(this.allObject3D, srid2D);

        }

        for (int i = 0; i < this.allObject3D.size(); i++) {
            GeoObject3D object3D = this.allObject3D.get(i);    
            String geom3D = object3D.getGeometry3D();
            // int srid3D = object3D.getSrid(object3D.getGeometry3D());       
            // String coord3D = object3D.getEnvelope().getGeometry().getValue();
            // Geometry envelope = createGeometry(coord3D);
            double refAreaRation = 0;           

            for (int j = 0; j < this.allObject2D.size(); j++) {
                GeoObject2D object2D = this.allObject2D.get(j);
                String geom2D = object2D.getGeometry2D().toString();
                try (Connection srcConn = this.pool.get3DConnection()) {
                    if (!srcConn.isValid(60)) {
                        LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                        throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
                    }else{
                        try (Statement stmt = srcConn.createStatement()) {
                            String transformSql = "SELECT public.ST_AREA(public.ST_INTERSECTION(public.ST_GeomFromEWKT('" + geom3D + "'), public.ST_GeomFromEWKT('" + geom2D + "'))) AS intersect";
                            ResultSet result = stmt.executeQuery(transformSql);
                        if (result.next()) {
                            float area = result.getFloat("intersect");
                            geom2D = geom2D.split(";")[1];
                            MultiPolygon polys2D = (MultiPolygon) reader.read(geom2D);
                            double areaRatio = 100.0*(area / polys2D.getArea());
//                        System.out.println("ratio: "+areaRatio + "%");
                            if(areaRatio>60){
                                if((refAreaRation !=0 && refAreaRation<areaRatio) || refAreaRation==0){

                                    object3D.setName(object2D.getName());
                                    address.setStreet(object2D.getStreet());
                                    address.setHouse(object2D.getHouse());
                                    address.setZipCode(object2D.getPostcode());
                                    address.setCity(object2D.getCity());
                                    address.setCountry(object2D.getCountry());
                                    address.setGmlid(object3D.getGmlId());
                                    object3D.setAddress(address);
                                    refAreaRation = areaRatio;
                                }
                            }
                        }
                                                       
                        }
                    } 
                }catch (SQLException e) {
                    LOGGER.fatal("Error connecting to source database: " + e);
                    throw new JPSRuntimeException("Error connecting to source database: " + e);
            }

//                 if ((!polys2D.within(envelope)) || (!envelope.within(polys2D))){
//                     if(envelope.intersects(polys2D)){
//                         Geometry intersect = envelope.intersection(polys2D);
//                         
//                     }
//                 }else{
//                     object3D.setName(object2D.getName());
//                     address = new ObjectAddress(object3D.getGmlId(), object2D.getStreet(), object2D.getHouse(), object2D.getPostcode(),object2D.getCountry(),object2D.getCity());
//                     object3D.setAddress(address);
//                 }
            }
            if (object3D.getName() != null){
                object3D.updateName(object3D,config);
            }
            if (object3D.getAddress().getStreet()!= null){
                object3D.updateAddress(object3D.getAddress(),config);
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
        Coordinate[] coord = str2coords(coordlist).toArray(new Coordinate[0]);
        LinearRing lr = fac.createLinearRing(coord);
        Polygon poly = new Polygon(lr, null, fac);
        return lr;
        // return fac.createPolygon(lr);       
    }

    public Geometry reproject(Geometry geom, int srcSRID, int dstSRID) {

        GeometryFactory fac = new GeometryFactory();
        // Geometry sourceGeometry = fac.createGeometry(geom);

        Geometry targetGeometry = null;
        try {
            CoordinateReferenceSystem sourceCRS = CRS.decode("EPSG:" + srcSRID);
            CoordinateReferenceSystem targetCRS = CRS.decode("EPSG:" + dstSRID);
            MathTransform transform = CRS.findMathTransform(sourceCRS, targetCRS);
            targetGeometry = JTS.transform(geom, transform);
            targetGeometry.setSRID(dstSRID);

        } catch (FactoryException | TransformException e) {
            e.printStackTrace();
        }
        return targetGeometry;
    }

    public List<GeoObject3D> reprojectPost(List<GeoObject3D> allObject3D, int dstSRID){
         if(this.pool == null){
            this.pool = new SqlConnectionPool(this.config);
        }

        try (Connection srcConn = this.pool.get3DConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }else{
                try (Statement stmt = srcConn.createStatement()) {
                    for (int i = 0; i < allObject3D.size(); i++) {
                        String targetGeom = null;
                        String srcGeom = allObject3D.get(i).getGeometry3D();
                        String transformSql = "SELECT public.ST_AsEWKT(public.ST_Transform(public.ST_GeomFromEWKT('" + srcGeom + "')," + dstSRID +")) AS targetGeom";
                        ResultSet result = stmt.executeQuery(transformSql);
                        if (result.next()) {
                            targetGeom = result.getString("targetGeom");
                            allObject3D.get(i).setGeometry(targetGeom);
                        }
                    }                           
                }
            } 
        }catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        } 
        return allObject3D;
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
