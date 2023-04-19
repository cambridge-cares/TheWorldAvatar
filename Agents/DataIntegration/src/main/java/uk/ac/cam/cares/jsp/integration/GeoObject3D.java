package uk.ac.cam.cares.jsp.integration;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.postgis.PGgeometry;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

public class GeoObject3D {
    private PGgeometry geometry;
    private String name;
    private int objectClassid;
    private int id;
    private String address;

    private static final Logger LOGGER = LogManager.getLogger(SpatialLink.class);
    private PostgresClient postgresClient;

    public GeoObject3D () {}
    public GeoObject3D(String name, int id, int objectClassid, PGgeometry geometry){
        this.name = name;
        this.id = id;
        this.objectClassid = objectClassid;
        this.geometry = geometry;
    }

    public String getName(){
        return this.name;
    }

    public int getId() {return this.id;}

    public PGgeometry getEnvelope(){
        return this.geometry;
    }

    public void setName(String name){
        this.name = name;
    }

    public void setId (int id){
        this.id = id;
    }

    public void setObjectClassid (int objectClassid) {
        this.objectClassid = objectClassid;
    }

    public void setGeometry (PGgeometry geometry) {
        this.geometry = geometry;
    }
    public void setAddress(String address){
        this.address = address;
    }

    public List<GeoObject3D> getObject3D (){

        List<GeoObject3D> allObject3D = new ArrayList<>();

        try (Connection conn = postgresClient.getConnection()) {
            String sql = "SELECT id, objectclass_id, name, envelope FROM cityobject";
            try (Statement stmt = conn.createStatement()) {
                ResultSet result = stmt.executeQuery(sql);
                while (result.next()) {
                    GeoObject3D object3D = new GeoObject3D();
                    object3D.setId(result.getInt("id"));
                    object3D.setObjectClassid(result.getInt("objectclass_id"));
                    object3D.setName(result.getString("name"));
                    object3D.setGeometry((PGgeometry)result.getObject("envelope"));
                    object3D.setPostGISClient(postgresClient);
                    allObject3D.add(object3D);
                }
                return allObject3D;
            }
        } catch (SQLException e) {
            LOGGER.error("Probably failed to disconnect");
            LOGGER.error(e.getMessage());
        }
        return null;
    }

    void setPostGISClient(PostgresClient postgresClient) {
        this.postgresClient = postgresClient;
    }

    public void updateName(GeoObject3D object3D){
        String upSql = "UPDATE cityobject SET ";
        if(object3D.name != null){
            upSql = upSql + "name = '" + object3D.name + "' WHERE id = " + object3D.id + ";";
            try (Connection conn = postgresClient.getConnection()) {
                try (Statement stmt = conn.createStatement()) {
                    stmt.executeUpdate(upSql);
                }
            }catch (SQLException e) {
                LOGGER.error("Probably failed to disconnect");
                LOGGER.error(e.getMessage());
            }
        }else{
            System.out.println("No Update");
        }

    }
}
