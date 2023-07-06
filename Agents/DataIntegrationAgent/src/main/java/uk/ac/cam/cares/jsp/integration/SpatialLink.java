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
import org.json.JSONObject;
import org.locationtech.jts.geom.*;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.*;
import java.util.stream.Stream;


public class SpatialLink extends HttpServlet {

    private static final Logger LOGGER = LogManager.getLogger(SpatialLink.class);
    private static String file ="/inputs/config/endpoint.properties";
    private PostgresClient postgresClient2d;
    private PostgresClient postgresClient3d;
    private static final String INVALID_CONNECTION_MESSAGE = "Connection is invalid...";
    private SqlConnectionPool pool;

    List<GeoObject3D> allObject3D = new ArrayList<>();
    List<GeoObject2D> allObject2D = new ArrayList<>();

//    public SpatialLink() {}
//    void setPostGISClient2d(PostgresClient postgisClient) {
//        this.postgresClient2d = postgisClient;
//    }
//    void setPostGISClient3d(PostgresClient postgisClient) {
//        this.postgresClient3d = postgisClient;
//    }
    protected void SpatialLink(String[] config) {
//        new Config().initProperties();
        this.pool = new SqlConnectionPool(config);
        LOGGER.info("Pinging source database for availability...");
        try (Connection srcConn = this.pool.getSourceConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }

        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
//        Map<String, String> parameters = aggregateByKeys();
//        String db3d = parameters.get("db3d");
//        String db2d = parameters.get("db2d");
//        String db2d_table = parameters.get("db2d_table");

//        String db3d;
//        String db2d;
//
        GeoObject3D object3D = new GeoObject3D();
        GeoObject2D object2D = new GeoObject2D();

//        try {
//            db3d = req.getParameter("db3d");
//            db2d = req.getParameter("db2d");
//            db2d_table = req.getParameter("db2d_table");
//        } catch (Exception e) {
//            LOGGER.error("Error parsing input, make sure database names are specified as parameters");
//            LOGGER.error(e.getMessage());
//            throw new RuntimeException(e);
//        }

//        if (postgresClient2d == null) {
//            postgresClient2d = new PostgresClient(Config.dburl + "/" + db2d, Config.dbuser, Config.dbpassword);
////            PostgresClient postgresClient3d = new PostgresClient(Config.dburl + "/" + db3d, Config.dbuser, Config.dbpassword);
//        }
//        if (postgresClient3d == null) {
//            postgresClient3d = new PostgresClient(Config.dburl + "/" + db3d, Config.dbuser, Config.dbpassword);
//        }

//        object2D.setPostGISClient(postgresClient2d);
        this.allObject2D = object2D.getObject2D(config);
//        object3D.setPostGISClient(postgresClient3d);
        this.allObject3D = object3D.getObject3D(config);
        try {
            findMatchedObjects();
        } catch (ParseException | FactoryException | TransformException | SQLException e) {
            throw new RuntimeException(e);
        }


    }
    public void findMatchedObjects() throws ParseException, FactoryException, TransformException, SQLException {

        GeometryFactory geometryFactory = JTSFactoryFinder.getGeometryFactory();
        WKTReader reader = new WKTReader( geometryFactory );
        ObjectAddress address = new ObjectAddress();

        for (int i = 0; i < this.allObject3D.size(); i++) {
            GeoObject3D object3D = this.allObject3D.get(i);
            int srid3D = object3D.getEnvelope().getGeometry().getSrid();
            String coord3D = object3D.getEnvelope().getGeometry().getValue();
            Geometry envelope = createGeometry(coord3D);
            double refAreaRation = 0;
            int srid2D = this.allObject2D.get(0).getGeometry2D().getGeometry().getSrid();
            if(srid3D != srid2D){
                Geometry transGeom3D = Transform(envelope, srid3D, srid2D);
//                srid3D = transGeom3D.getSRID();
                Coordinate[] reversedCoordinates = getReversedCoordinates(transGeom3D);
                envelope = geometryFactory.createPolygon(reversedCoordinates);
            }

            for (int j = 0; j < this.allObject2D.size(); j++) {
                GeoObject2D object2D = this.allObject2D.get(j);

                String geom2D = object2D.getGeometry2D().toString();
                geom2D = geom2D.split(";")[1];
                MultiPolygon polys2D = (MultiPolygon) reader.read(geom2D);
                if ((!polys2D.within(envelope)) || (!envelope.within(polys2D))){
                    if(envelope.intersects(polys2D)){
                        Geometry intersect = envelope.intersection(polys2D);
                        double areaRatio = 100.0*intersect.getArea() / polys2D.getArea();
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
                }else{
                    object3D.setName(object2D.getName());
                    address = new ObjectAddress(object3D.getGmlId(), object2D.getStreet(), object2D.getHouse(), object2D.getPostcode(),object2D.getCountry(),object2D.getCity());
                    object3D.setAddress(address);
                }
            }
            if (object3D.getName() != null){
                object3D.updateName(object3D);
            }
            if (object3D.getAddress().getStreet()!= null){
                object3D.updateAddress(object3D.getAddress());
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

    public static Map<String, String> aggregateByKeys() {
        Map<String, String> map = new HashMap<>();
        try (Stream<String> lines = Files.lines(Paths.get(file))) {
            lines.filter(line -> line.contains(":"))
                    .forEach(line -> {
                        String[] keyValuePair = line.split(":", 2);
                        String key = keyValuePair[0];
                        String value = keyValuePair[1];
                        map.put(key, value);

                    });
        } catch (IOException e) {
            e.printStackTrace();
        }
        return map;
    }
}
