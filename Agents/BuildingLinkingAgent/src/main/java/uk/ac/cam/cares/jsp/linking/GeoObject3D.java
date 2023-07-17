package uk.ac.cam.cares.jsp.linking;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.postgis.PGgeometry;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

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
    private String cityobjectid;
    private String objectIri;
    private ObjectAddress address = new ObjectAddress();

    private static final Logger LOGGER = LogManager.getLogger(GeoObject3D.class);
    private PostgresClient postgresClient;
    private static final String INVALID_CONNECTION_MESSAGE = "Connection is invalid...";
    private SqlConnectionPool pool;

    public GeoObject3D () {}
    public GeoObject3D(String name, String cityobjectid, int objectClassid, PGgeometry geometry, String Iri){
        this.name = name;
        this.cityobjectid = cityobjectid;
        this.objectClassid = objectClassid;
        this.geometry = geometry;
        this.objectIri = Iri;
    }

    public String getName(){
        return this.name;
    }

    public String getCityobjectId() {return this.cityobjectid;}

    public PGgeometry getEnvelope(){
        return this.geometry;
    }

    public String getIRI() {return this.objectIri;}

    public void setName(String name){
        this.name = name;
    }

    public void setCityObjectId (String cityobjectid){
        this.cityobjectid = cityobjectid;
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

    public void setIri (String objectIri){
        this.objectIri = objectIri;
    }
    public ObjectAddress getAddress(){ return this.address;}


// 1. get cityobject 2. get generic attribute (IRI) of cityobject 3. get address of cityobject
    public List<GeoObject3D> getObject3D (String[] config){

        List<GeoObject3D> allObject3D = new ArrayList<>();
        this.pool = new SqlConnectionPool(config);
        LOGGER.info("Pinging source database for availability...");
        try (Connection srcConn = this.pool.getSourceConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }else{
                String sql = "SELECT id, objectclass_id, name, envelope FROM cityobject";
                try (Statement stmt = srcConn.createStatement()) {
                    ResultSet result = stmt.executeQuery(sql);
                    while (result.next()) {
                        GeoObject3D object3D = new GeoObject3D();
                        object3D.setCityObjectId(result.getString("id"));
                        object3D.setObjectClassid(result.getInt("objectclass_id"));
                        object3D.setName(result.getString("name"));
                        object3D.setGeometry((PGgeometry)result.getObject("envelope"));
                        object3D.setPostGISClient(postgresClient);
                        object3D.setAddress(this.address.queryAddress(result.getInt("id"), srcConn));
                        object3D.setIri(this.queryIRI(result.getString("id"), srcConn));
                        allObject3D.add(object3D);
                    }
                    return allObject3D;
                }
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }

        // try (Connection conn = postgresClient.getConnection()) {
        //     String sql = "SELECT id, gmlid, objectclass_id, name, envelope FROM cityobject";
        //     try (Statement stmt = conn.createStatement()) {
        //         ResultSet result = stmt.executeQuery(sql);
        //         while (result.next()) {
        //             GeoObject3D object3D = new GeoObject3D();
        //             object3D.setGmlid(result.getString("gmlid"));
        //             object3D.setObjectClassid(result.getInt("objectclass_id"));
        //             object3D.setName(result.getString("name"));
        //             object3D.setGeometry((PGgeometry)result.getObject("envelope"));
        //             object3D.setPostGISClient(postgresClient);
        //             object3D.setAddress(this.address.queryAddress(result.getInt("id"), conn));
        //             allObject3D.add(object3D);
        //         }
        //         return allObject3D;
        //     }
        // } catch (SQLException e) {
        //     LOGGER.error("Probably failed to disconnect");
        //     LOGGER.error(e.getMessage());
        // }
        // return null;
    }

    public String queryIRI(String cityobjectid, Connection srcConn){
        this.objectIri = null;
        try {
            if (!srcConn.isValid(60)) {
                    LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                    throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
                }else{
                    String sql = "SELECT urival FROM cityobject_genericattrib WHERE cityobject_id = " + cityobjectid + "AND urival IS NOT NULL";
                    try (Statement stmt = srcConn.createStatement()) {
                        ResultSet result = stmt.executeQuery(sql);
                        while (result.next()) {
                            this.objectIri = result.getString("urival");
                        }
                        
                    }
                }
        } catch (SQLException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return this.objectIri;

    }

    void setPostGISClient(PostgresClient postgresClient) {
        this.postgresClient = postgresClient;
    }

}
