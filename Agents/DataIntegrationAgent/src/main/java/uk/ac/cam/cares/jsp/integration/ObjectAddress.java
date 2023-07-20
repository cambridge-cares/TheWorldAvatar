package uk.ac.cam.cares.jsp.integration;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

//address address_to_building table in postgresql
public class ObjectAddress {
    private static final Logger LOGGER = LogManager.getLogger(ObjectAddress.class);
    private String gmlid;
    private String street;
    private String house;
    private String zip_code;
    private String country;
    private String city;
    private PostgresClient postgresClient;
    private static final String INVALID_CONNECTION_MESSAGE = "Connection is invalid...";
    private SqlConnectionPool pool;

    ObjectAddress() {}
    ObjectAddress(String gmlid, String street, String house, String zip_code, String country, String city) {
        this.gmlid = gmlid;
        this.street = street;
        this.house = house;
        this.zip_code = zip_code;
        this.country = country;
        this.city = city;
    }

    public String getGmlId() {return this.gmlid;}
    public String getStreet() {return this.street;}
    public String getHouse() {return this.house;}
    public String getZipCode() {return this.zip_code;}
    public String getCountry() {return this.country;}
    public String getCity() {return this.city;}
    public void setStreet(String street) {this.street = street;}
    public void setHouse(String house) {this.house = house;}
    public void setGmlid (String gmlid) {this.gmlid = gmlid;}
    public void setCity (String city) {this.city = city;}
    public void setCountry (String country) {this.country = country;}
    public void setZipCode (String zipCode) {this.zip_code = zipCode;}
    public void setPostGISClient(PostgresClient postgresClient) {
        this.postgresClient = postgresClient;
    }

    public void updateAddress (ObjectAddress address, String[] config) throws SQLException {
        String gmlid = address.getGmlId();
        this.pool = new SqlConnectionPool(config);
        LOGGER.info("Pinging source database for availability...");
        try (Connection srcConn = this.pool.get3DConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }else{
            String sql = "SELECT id FROM address WHERE address.gmlid = " + "'" + gmlid + "'";//check address existing firstly
            try (Statement stmt = srcConn.createStatement()) {
                ResultSet result = stmt.executeQuery(sql);
                if (!result.next()) {
                    String upSql1 = "INSERT INTO address (gmlid, street, house_number, zip_code, city, country) VALUES (";
                    if(gmlid != null) {
                        upSql1 = upSql1 + "'" + gmlid + "', ";
                    }else{
                        upSql1 = upSql1 + "null, ";
                    }
                    if(address.street != null) {
                        upSql1 = upSql1  + "'" + address.street + "', ";
                    }else{
                        upSql1 = upSql1 + "null, ";
                    }
                    if(address.house != null) {
                        upSql1 = upSql1  + "'" + address.house + "', ";
                    }else{
                        upSql1 = upSql1 + "null, ";
                    }
                    if(address.zip_code != null) {
                        upSql1 = upSql1  + "'" + address.zip_code + "', ";
                    }else{
                        upSql1 = upSql1 + "null, ";
                    }
                    if(address.city != null) {
                        upSql1 = upSql1  + "'" + address.city + "', ";
                    }else{
                        upSql1 = upSql1 + "null, ";
                    }
                    if(address.country != null) {
                        upSql1 = upSql1  + "'" + address.country + "')";
                    }else{
                        upSql1 = upSql1 + "null); ";
                    }
                    stmt.executeUpdate(upSql1);//insert data to address table

                    insertAtoB(gmlid,config);
                }
            
                }
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }    
        
    }

    public void insertAtoB(String gmlid, String[] config) throws SQLException {
        String sqlCityObject = "SELECT id FROM cityobject WHERE cityobject.gmlid = " + "'" + gmlid + "'";
        String sqlAddress = "SELECT id FROM address WHERE address.gmlid = " + "'" + gmlid + "'";
        this.pool = new SqlConnectionPool(config);
        LOGGER.info("Pinging source database for availability...");
        try (Connection srcConn = this.pool.get3DConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }else{
                Statement stmtC = srcConn.createStatement();
                Statement stmtA = srcConn.createStatement();
                ResultSet resultC = stmtC.executeQuery(sqlCityObject);
                ResultSet resultA = stmtA.executeQuery(sqlAddress);
                if (resultA.next() && resultC.next()) {
                    int building_id = resultC.getInt("id");
                    int address_id = resultA.getInt("id");
                    String sqlBuilding = "SELECT id FROM building WHERE building.id = " + building_id;
                    Statement stmtB = srcConn.createStatement();
                    ResultSet resultB = stmtB.executeQuery(sqlBuilding);
                    if (resultB.next()){
                        String insertSql = "INSERT INTO address_to_building VALUES (" + building_id + ", " + address_id + ");";
                        Statement stmt = srcConn.createStatement();
                        stmt.executeUpdate(insertSql);
                        System.out.println("Insert linking of building" + building_id);
                    }
                }
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
    }

    public ObjectAddress queryAddress(int objectId, Connection conn) {
        ObjectAddress objAddress = new ObjectAddress();
            String sql = "SELECT address_id FROM address_to_building WHERE address_to_building.building_id = " + objectId;//check address existing
            try (Statement stmt = conn.createStatement()) {
                ResultSet resultLink = stmt.executeQuery(sql);
                if (resultLink.next()) {
                    int addressId = resultLink.getInt("address_id");
                    String addressSql = "SELECT * FROM address WHERE address.id = " + addressId;
                    ResultSet resAddress = stmt.executeQuery(addressSql);
                    while (resAddress.next()){
                        objAddress.gmlid = resAddress.getString("gmlid");
                        objAddress.street = resAddress.getString("street");
                        objAddress.house = resAddress.getString("house_number");
                        objAddress.zip_code = resAddress.getString("zip_code");
                        objAddress.country = resAddress.getString("country");
                        objAddress.city = resAddress.getString("city");
                    }
                }
                return objAddress;

        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

}
