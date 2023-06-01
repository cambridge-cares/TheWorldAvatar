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
    private String street;
    private String postcode;
    private String country;
    private String city;
    private String house;

    private static final Logger LOGGER = LogManager.getLogger(SpatialLink.class);
    private PostgresClient postgresClient;

    public GeoObject2D () {}
    public GeoObject2D(String name, String street, String house, String postcode, String country, String city, PGgeometry geometry){
        this.name = name;
        this.street = street;
        this.house = house;
        this.postcode = postcode;
        this.geometry2D = geometry;
        this.country = country;
        this.city = city;
    }

    public String getName(){
        return this.name;
    }

    public PGgeometry getGeometry2D(){
        return this.geometry2D;
    }

    public String getStreet(){
        return this.street;
    }
    public String getHouse() {return this.house;}
    public String getPostcode() {return  this.postcode; }
    public String getCountry() {return this.country;}
    public String getCity() { return this.city; }
    public void setName(String name){
        this.name = name;
    }

    public void setGeometry2D(PGgeometry object2D){
        this.geometry2D = object2D;
    }

    public void setAddress(String street){
        this.street =  street;
    }
    public void setHouse(String house) {this.house = house;}

    public void setPostcode(String postcode){
        this.postcode =  postcode;
    }
    public void setCountry(String country) {this.country = country;}
    public void setCity(String city) {this.city = city;}

    void setPostGISClient(PostgresClient postgresClient) {
        this.postgresClient = postgresClient;
    }

    public List<GeoObject2D> getObject2D (String tableName){

        List<GeoObject2D> allObject2D = new ArrayList<>();

        try (Connection conn = postgresClient.getConnection()) {
            String sql = "SELECT name, addr_postc, addr_stree, addr_hou_1, addr_count, addr_city, wkb_geometry FROM " + tableName + " WHERE name is not null or addr_stree is not null";
            try (Statement stmt = conn.createStatement()) {
                ResultSet result = stmt.executeQuery(sql);
                while (result.next()) {
                    GeoObject2D object2D = new GeoObject2D();
                    object2D.setName(result.getString("name"));
                    object2D.setGeometry2D((PGgeometry)result.getObject("wkb_geometry"));
                    object2D.setAddress(result.getString("addr_stree"));
                    object2D.setHouse(result.getString("addr_hou_1"));
                    object2D.setPostcode(result.getString("addr_postc"));
                    object2D.setCity(result.getString("addr_city"));
                    object2D.setCountry(result.getString("addr_count"));
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
