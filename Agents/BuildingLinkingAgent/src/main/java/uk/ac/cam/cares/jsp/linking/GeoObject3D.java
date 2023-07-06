package uk.ac.cam.cares.jsp.linking;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.postgis.PGgeometry;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
//cityobject table in postgresql
public class GeoObject3D {
    private PGgeometry geometry;
    private String name;
    private int objectClassid;
    private String gmlid;
    private ObjectAddress address = new ObjectAddress();

    private static final Logger LOGGER = LogManager.getLogger(BuildingLink.class);
    private PostgresClient postgresClient;

    public GeoObject3D () {}
    public GeoObject3D(String name, String gmlid, int objectClassid, PGgeometry geometry){
        this.name = name;
        this.gmlid = gmlid;
        this.objectClassid = objectClassid;
        this.geometry = geometry;
    }

    public String getName(){
        return this.name;
    }

    public String getGmlId() {return this.gmlid;}

    public PGgeometry getEnvelope(){
        return this.geometry;
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

    public List<GeoObject3D> getObject3D (){

        List<GeoObject3D> allObject3D = new ArrayList<>();

        try (Connection conn = postgresClient.getConnection()) {
            String sql = "SELECT id, gmlid, objectclass_id, name, envelope FROM cityobject";
            try (Statement stmt = conn.createStatement()) {
                ResultSet result = stmt.executeQuery(sql);
                while (result.next()) {
                    GeoObject3D object3D = new GeoObject3D();
                    object3D.setGmlid(result.getString("gmlid"));
                    object3D.setObjectClassid(result.getInt("objectclass_id"));
                    object3D.setName(result.getString("name"));
                    object3D.setGeometry((PGgeometry)result.getObject("envelope"));
                    object3D.setPostGISClient(postgresClient);
                    object3D.setAddress(this.address.queryAddress(result.getInt("id"), conn));
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
            upSql = upSql + "name = '" + object3D.name + "' WHERE gmlid = '" + object3D.gmlid + "';";
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

    public void updateAddress(ObjectAddress address) throws SQLException {
        this.address.setPostGISClient(this.postgresClient);
        this.address.updateAddress(address);
    }
}
