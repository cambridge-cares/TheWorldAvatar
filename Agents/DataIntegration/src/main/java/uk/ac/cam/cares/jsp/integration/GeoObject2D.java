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

public class GeoObject2D {
    private PGgeometry geometry2D;
    private String name;
    private String address;
    private String postcode;

    private static final Logger LOGGER = LogManager.getLogger(SpatialLink.class);
    private PostgresClient postgresClient;

    public GeoObject2D () {}
    public GeoObject2D(String name, String address, String postcode, PGgeometry geometry){
        this.name = name;
        this.address = address;
        this.postcode = postcode;
        this.geometry2D = geometry;
    }

    public String getName(){
        return this.name;
    }

    public PGgeometry getGeometry2D(){
        return this.geometry2D;
    }

    public String getAddress(){
        return this.address;
    }

    public void setName(String name){
        this.name = name;
    }

    public void setGeometry2D(PGgeometry object2D){
        this.geometry2D = object2D;
    }

    public void setAddress(String address){
        this.address =  address;
    }

    public void setPostcode(String postcode){
        this.postcode =  postcode;
    }

    void setPostGISClient(PostgresClient postgresClient) {
        this.postgresClient = postgresClient;
    }

    public List<GeoObject2D> getObject2D (){

        List<GeoObject2D> allObject2D = new ArrayList<>();

        try (Connection conn = postgresClient.getConnection()) {
            String sql = "SELECT name, addr_postc, addr_stree, wkb_geometry FROM ntu2d";
            try (Statement stmt = conn.createStatement()) {
                ResultSet result = stmt.executeQuery(sql);
                while (result.next()) {
                    GeoObject2D object2D = new GeoObject2D();
                    object2D.setName(result.getString("name"));
                    object2D.setGeometry2D((PGgeometry)result.getObject("wkb_geometry"));
                    object2D.setAddress(result.getString("addr_stree"));
                    object2D.setPostcode(result.getString("addr_postc"));
                    object2D.setPostGISClient(postgresClient);
                    allObject2D.add(object2D);
                }
                return allObject2D;
            }
        } catch (SQLException e) {
            LOGGER.error("Probably failed to disconnect");
            LOGGER.error(e.getMessage());
        }
        return null;
    }


}
