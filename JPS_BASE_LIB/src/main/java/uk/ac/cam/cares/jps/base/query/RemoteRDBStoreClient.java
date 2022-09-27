package uk.ac.cam.cares.jps.base.query;

import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.util.StoreClientHelper;

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

public class RemoteRDBStoreClient implements StoreClientInterface {

    //RDB Connection object
    private Connection conn;
    Statement stmt;
    // URL and credentials for the relational database
    private String rdbURL;
    private String rdbUser;
    private String rdbPassword;

    private String query;
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
     * A constructor defined to initialise the URL, username and password to connect to the RDB
     * and a data retrieval or update query
     * @param rdbURL
     * @param user
     * @param password
     * @param query
     */
    public RemoteRDBStoreClient(String rdbURL, String user, String password, String query){
        this.rdbURL = rdbURL;
        this.rdbUser = user;
        this.rdbPassword = password;
        this.query = query;
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
     * @return query result as a ResultSet
     */
    public ResultSet executeQuerytoResultSet(String query) {
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
     * Executes the query supplied by the calling method and returns results
     * as a JSONArray.
     * @param query
     * @return query result as a JSONArray
     */
    @Override
    public JSONArray executeQuery(String query){
        JSONArray results;
        try {
            if(this.conn == null || stmt==null){
                this.conn = getConnection();
            }
            java.sql.ResultSet rs = this.stmt.executeQuery(query);
            results = StoreClientHelper.convert(rs);
        } catch (SQLException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
        return results;
    }

    /**
     * Executes the query that is provided through the constructors or setter method.
     * @return query result as a JSONArray
     */
    @Override
    public JSONArray executeQuery() {
        return executeQuery(this.query);
    }

    /**
     * Executes query using the query variable
     * @return JSONArray as String
     */
    @Override
    public String execute() {
        return execute(this.query);
    }

    /**
     * Executes query supplied by the calling method.
     * @param query
     * @return JSONArray as String
     */
    @Override
    public String execute(String query){
        JSONArray result = executeQuery(query);
        if (result == null) {
            throw new JPSRuntimeException("Query result is null.");
        } else {
            return result.toString();
        }
    }

    /**
     * Executes the update operation supplied by the calling method.
     * @param update
     * @return
     */
    @Override
    public int executeUpdate(String update) {
        try {
            if(this.conn == null || stmt==null){
                this.conn = getConnection();
            }
            return this.stmt.executeUpdate(update);
        } catch (SQLException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    /**
     * Executes the update request supplied by the calling method.
     * @param update as UpdateRequest
     */
    @Override
    public int executeUpdate(UpdateRequest update){
        return executeUpdate(update.toString());
    }

    /**
     * Executes the update operation that is provided through the constructor or setter method.
     * @return
     */
    @Override
    public int executeUpdate() {
        return executeUpdate(this.query);
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

    @Override
    public void setUser(String user) {
        this.rdbUser = user;
    }

    @Override
    public String getUser() {
        return rdbUser;
    }

    @Override
    public void setPassword(String password) {
        this.rdbPassword = password;
    }

    @Override
    public String getPassword() {
        return rdbPassword;
    }

    /**
     * Returns the available query.
     * @return
     */
    @Override
    public String getQuery() {
        return query;
    }

    /**
     * Sets a query if provided.
     * @param query
     * @return
     */
    @Override
    public String setQuery(String query) {
        this.query = query;
        return this.query;
    }
}



