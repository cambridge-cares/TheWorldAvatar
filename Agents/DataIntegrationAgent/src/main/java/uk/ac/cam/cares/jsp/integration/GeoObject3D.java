package uk.ac.cam.cares.jsp.integration;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.checkerframework.checker.units.qual.g;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.CoordinateSequence;
import org.locationtech.jts.geom.CoordinateSequenceFilter;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryCollection;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.operation.buffer.BufferOp;
import org.locationtech.jts.operation.buffer.BufferParameters;
import org.locationtech.jts.geom.util.GeometryFixer;
import org.postgis.PGgeometry;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
//cityobject table in postgresql
public class GeoObject3D {
    private PGgeometry geometry;
    private String name;
    private int objectClassid;
    private String gmlid;
    private int cityobjectid;
    private ObjectAddress address = new ObjectAddress();

    private static final Logger LOGGER = LogManager.getLogger(SpatialLink.class);
    private PostgresClient postgresClient;
    private static final String INVALID_CONNECTION_MESSAGE = "Connection is invalid...";
    private SqlConnectionPool pool;

    public GeoObject3D () {}
    public GeoObject3D(String name, int cityobjectid, int objectClassid, PGgeometry geometry){
        this.name = name;
        this.cityobjectid = cityobjectid;
        this.objectClassid = objectClassid;
        this.geometry = geometry;
    }

    public String getName(){
        return this.name;
    }

    public int getId() {return this.cityobjectid;}

    public String getGmlId() {return this.gmlid;}

    public PGgeometry getEnvelope(){
        return this.geometry;
    }

    public void setId(int cityobjectid) {
        this.cityobjectid = cityobjectid;
    }
    public void setName(String name){
        this.name = name;
    }

    public void setGmlid (String gmlid){
        this.gmlid = gmlid;
    }

    public void setObjectClassid (int objectClassid) {
        this.objectClassid = objectClassid;
    }

    public void setGeometry (PGgeometry geometry) {
        this.geometry = geometry;
    }
    public void setAddress(ObjectAddress address){
        this.address = address;
    }

    public ObjectAddress getAddress(){ return this.address;}

    public List<GeoObject3D> getObject3D (String[] config){

        List<GeoObject3D> allObject3D = new ArrayList<>();
        this.pool = new SqlConnectionPool(config);
        LOGGER.info("Pinging source database for availability...");
        try (Connection srcConn = this.pool.getSourceConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }

            String sql = "SELECT id, gmlid, objectclass_id, name, envelope FROM cityobject";
            try (Statement stmt = srcConn.createStatement()) {
                ResultSet result = stmt.executeQuery(sql);
                while (result.next()) {
                    GeoObject3D object3D = new GeoObject3D();
                    object3D.setId(result.getInt("id"));
                    object3D.setGmlid(result.getString("gmlid"));
                    object3D.setObjectClassid(result.getInt("objectclass_id"));
                    object3D.setName(result.getString("name"));
                    object3D.setGeometry((PGgeometry)result.getObject("envelope"));
                    object3D.setPostGISClient(postgresClient);
                    object3D.setAddress(this.address.queryAddress(result.getInt("id"), srcConn));
                    allObject3D.add(object3D);
                }
                return allObject3D;
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
    }

    void setPostGISClient(PostgresClient postgresClient) {
        this.postgresClient = postgresClient;
    }

    public void updateName(GeoObject3D object3D, String[] config){
        String upSql = "UPDATE cityobject SET ";
        if(object3D.name != null){
            upSql = upSql + "name = '" + object3D.name + "' WHERE gmlid = '" + object3D.gmlid + "';";
            this.pool = new SqlConnectionPool(config);
            LOGGER.info("Pinging source database for availability...");
            try (Connection srcConn = this.pool.getSourceConnection()) {
                if (!srcConn.isValid(60)) {
                    LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                    throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
                }else{
                    try (Statement stmt = srcConn.createStatement()) {
                    stmt.executeUpdate(upSql);
                    }
                }
            } catch (SQLException e) {
                LOGGER.fatal("Error connecting to source database: " + e);
                throw new JPSRuntimeException("Error connecting to source database: " + e);
            }    
        }else{
            System.out.println("No Update");
        }

    }

    public void updateAddress(ObjectAddress address, String[] config) throws SQLException {
        this.address.setPostGISClient(this.postgresClient);
        this.address.updateAddress(address,config);
    }

    public List<Polygon> queryBuildingSurfaces(int cityobjectid){
        List<Polygon> gourndSurface = new ArrayList<>();
        try (Connection srcConn = this.pool.getSourceConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }else{
                String sql = "SELECT geometry, cityobject_id FROM surface_geometry WHERE cityobject_id = " + cityobjectid;
                try (Statement stmt = srcConn.createStatement()) {
                    ResultSet result = stmt.executeQuery(sql);
                    while (result.next()) {
                        SpatialLink sp = new SpatialLink();
                        PGgeometry geom = (PGgeometry)result.getObject("geometry");
                        String coordString = geom.getValue();
                        Geometry polygon = sp.createGeometry(coordString);
                        gourndSurface.add((Polygon)polygon);
                    }                  
                }
                return gourndSurface;
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }

    }
}
