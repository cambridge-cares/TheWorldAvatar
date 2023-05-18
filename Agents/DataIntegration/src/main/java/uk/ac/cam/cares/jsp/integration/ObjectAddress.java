package uk.ac.cam.cares.jsp.integration;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

//address address_to_building table in postgresql
public class ObjectAddress {
    private static final Logger LOGGER = LogManager.getLogger(KGObjects.class);
    private String gmlid;
    private String street;
    private String zip_code;
    private String country;
    private String city;
    private PostgresClient postgresClient;

    ObjectAddress(String gmlid, String street, String zip_code, String country, String city) {
        this.gmlid = gmlid;
        this.street = street;
        this.zip_code = zip_code;
        this.country = country;
        this.city = city;
    }

    public String getGmlId() {return this.gmlid;}
    public void setPostGISClient(PostgresClient postgresClient) {
        this.postgresClient = postgresClient;
    }

    public void updateAddress (ObjectAddress address) throws SQLException { //check address exiting firstly
        String gmlid = address.getGmlId();
        try (Connection conn = postgresClient.getConnection()) {
            String sql = "SELECT id FROM address WHERE address.gmlid = " + gmlid;
            try (Statement stmt = conn.createStatement()) {
                ResultSet result = stmt.executeQuery(sql);
                if (!result.next()) {

                }
            }
        }


    }

}
