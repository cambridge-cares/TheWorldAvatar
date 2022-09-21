package uk.ac.cam.cares.jps.base.query;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.sql.*;

/**
 * This class allows to establish connection with postgresql relational databases (RDBs)
 * to perform query and update operations.
 *
 * It is also used to obtain the connection object after the connection to the RDB
 * has been established.
 *
 * It requires to set the URL and credentials for the postgresql database.
 * Example URL: jdbc:postgresql://host.docker.internal:5432/timeseries
 *
 * @author Mehal Agarwal (ma988@cam.ac.uk)
 *
 */

public class RemoteRDBStoreClient {

    //RDB Connection object
    private Connection conn;
    Statement stmt;
    // URL and credentials for the relational database
    private String rdbURL;
    private String rdbUser;
    private String rdbPassword;
    //Driver string
    static final String driver = "org.postgresql.Driver";

    /**
     * A constructor defined to initialise the URL, username and password to connect to the RDB
     * @param rdbURL
     * @param user
     * @param password
     */
    public RemoteRDBStoreClient(String rdbURL, String user, String password) {
        this.rdbURL = rdbURL;
        this.rdbUser = user;
        this.rdbPassword = password;
    }

    /**
     * Establish connection to RDB and set Statement object
     * @return connection object to the RDB
     */
    public Connection getConnection(){
        try {
            Class.forName(driver);
            conn = DriverManager.getConnection(this.rdbURL, this.rdbUser, this.rdbPassword);
            stmt = conn.createStatement(java.sql.ResultSet.TYPE_FORWARD_ONLY, java.sql.ResultSet.CONCUR_READ_ONLY);
        } catch (SQLException e) {
            throw new JPSRuntimeException("The connection attempt failed", e);
        } catch (ClassNotFoundException e){
            throw new JPSRuntimeException("Failed to load postgresql", e);
        }
        return conn;
    }

    /**
     * Executes the query supplied by the calling method and returns results
     * as a ResultSet
     * @param query
     * @return the query result as a ResultSet
     */
    public ResultSet executeQuery(String query) {
        ResultSet rs;
        try {
            if(this.conn == null || stmt==null){
                this.conn = getConnection();
            }
            rs = this.stmt.executeQuery(query);
        } catch (SQLException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
        return rs;
    }

    /**
     * Executes the update operation supplied by the calling method.
     * @param update
     * @return
     */
    public int executeUpdate(String update) {
        try {
            return this.stmt.executeUpdate(update);
        } catch (SQLException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    /**
     * Get and set methods for private relational database properties (e.g. PostgreSQL)
     */
    public void setRdbURL(String rdbURL) {
        this.rdbURL = rdbURL;
    }

    public String getRdbURL() {
        return rdbURL;
    }

    public void setUser(String user) {
        this.rdbUser = user;
    }

    public String getUser() {
        return rdbUser;
    }

    public void setPassword(String password) {
        this.rdbPassword = password;
    }
}



