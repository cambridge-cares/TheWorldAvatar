package uk.ac.cam.cares.jsp.integration;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Polygon;
import org.postgis.PGgeometry;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
//cityobject table in postgresql
public class GeoObject3D {
    private String geometry;
    private String name;
    private int objectClassid;
    private String gmlid;
    private int cityobjectid;
    private ObjectAddress address = new ObjectAddress();

    private static final Logger LOGGER = LogManager.getLogger(GeoObject3D.class);
    private String[] config;
    private static final String INVALID_CONNECTION_MESSAGE = "Connection is invalid...";
    private SqlConnectionPool pool;

    public GeoObject3D () {}
    public GeoObject3D(String name, int cityobjectid, int objectClassid, String geometry){
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

    public String getGeometry3D(){
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

    public void setGeometry (String geometry) {
        this.geometry = geometry;
    }
    public void setAddress(ObjectAddress address){
        this.address = address;
    }

    public int getSrid(String geom){
        String[] srid = geom.split("[= ;]");
        return Integer.parseInt(srid[1]);
    }

    public ObjectAddress getAddress(){ return this.address;}

    public List<GeoObject3D> getObject3D (String[] config) throws SQLException {

        List<GeoObject3D> allObject3D = new ArrayList<>();
        this.config = config;
        this.pool = new SqlConnectionPool(config);
        LOGGER.info("Pinging source database for availability...");
        try (Connection srcConn = this.pool.get3DConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }

            String sql = "SELECT id, gmlid, objectclass_id, name, public.ST_AsEWKT(envelope) AS geom FROM cityobject";
            try (Statement stmt = srcConn.createStatement()) {
                ResultSet result = stmt.executeQuery(sql);
                while (result.next()) {
                    GeoObject3D object3D = new GeoObject3D();
                    object3D.setId(result.getInt("id"));
                    object3D.setGmlid(result.getString("gmlid"));
                    object3D.setObjectClassid(result.getInt("objectclass_id"));
                    object3D.setName(result.getString("name"));
                    object3D.setGeometry(result.getString("geom"));
                    object3D.setSqlConnectionPool(this.pool);
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

    void setSqlConnectionPool(SqlConnectionPool pool) {
        this.pool = pool;
    }

    public void updateName(GeoObject3D object3D, String[] config){
        String upSql = "UPDATE cityobject SET ";
        if(object3D.name != null){
            upSql = upSql + "name = '" + object3D.name + "' WHERE gmlid = '" + object3D.gmlid + "';";
            this.pool = new SqlConnectionPool(config);
            LOGGER.info("Pinging source database for availability...");
            try (Connection srcConn = this.pool.get3DConnection()) {
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
        this.address.updateAddress(address,config);
    }

    public Map<Integer, Polygon> queryBuildingSurfaces(int cityobjectid){
        Map<Integer, Polygon> gourndSurface = new java.util.HashMap<>();
        try (Connection srcConn = this.pool.get3DConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }else{
                String sql = "SELECT geometry, id FROM surface_geometry WHERE cityobject_id = " + cityobjectid;
                try (Statement stmt = srcConn.createStatement()) {
                    ResultSet result = stmt.executeQuery(sql);
                    while (result.next()) {
                        SpatialLink sp = new SpatialLink();
                        org.postgis.Geometry geom = (org.postgis.Geometry)result.getObject("geometry");
                        if(geom != null){
                            String coordString = geom.getValue();
                            Geometry polygon = sp.createGeometry(coordString);
                            gourndSurface.put(Integer.valueOf(result.getInt("id")) ,(Polygon)polygon);
                        }                        
                    }                  
                }
                return gourndSurface;
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }

    }

    public void updateGroundSurface(int surfaceid, int buildingid){
        String querySql = "SELECT id, objectclass_id FROM thematic_surface WHERE building_id = " + buildingid + " AND lod2_multi_surface_id = " + surfaceid;
        String upSql;
        LOGGER.info("Pinging source database for availability...");
        if(this.pool == null){
            this.pool = new SqlConnectionPool(this.config);
        }
        try (Connection srcConn = this.pool.get3DConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }else{
                try (Statement stmt = srcConn.createStatement()) {
                    ResultSet result = stmt.executeQuery(querySql);
                    if (result.next()) {
                        upSql = "UPDATE thematic_surface SET objectclass_id = 35 WHERE id = " + result.getInt("id") +";";
                    }else{
                        upSql = "INSERT INTO thematic_surface (objectclass_id, building_id, lod2_multi_surface_id) VALUE (" + 35 + "," + buildingid + "," + surfaceid +");";
                    }
                    stmt.executeUpdate(upSql);
                }
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }    
    }

    //1. insert data in surface_geometry 2. update data in building
    public void updateFootprint(int buildingid, Polygon polygon){
        String insertSql = "INSERT INTO surface_geometry (geometry, cityobject_id) VALUE (" + polygon + "," + buildingid + ");";
        int surfaceid = 0;
        if(this.pool == null){
            this.pool = new SqlConnectionPool(this.config);
        }
        try (Connection srcConn = this.pool.get3DConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }else{
                try (Statement stmt = srcConn.createStatement()) {
                    stmt.executeUpdate(insertSql);
                    
                    ResultSet rs =  stmt.getGeneratedKeys();
                    if(rs.next()){
                        surfaceid = rs.getInt(1);
                    }
                    if(surfaceid != 0){
                        String upSql = "UPDATE building SET lod0_footprint_id = " + surfaceid + " WHERE id = " + buildingid + ";";
                        try (Statement upStmt = srcConn.createStatement()) {
                            upStmt.executeUpdate(upSql);
                        }
                    }
                    
                }
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }    
    }

    
    
}
