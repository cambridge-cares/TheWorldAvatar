package uk.ac.cam.cares.jps.base.query;

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
    // URL and credentials for the relational database
    private String rdbURL;
    private String rdbUser;
    private String rdbPassword;

    private static final String ERR_PREFIX = "RemoteRDBStoreClient: ";

    private String query;

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
     * @throws SQLException
     */
    public Connection getConnection() throws SQLException{
        try {
            Class.forName("org.postgresql.Driver");
            return DriverManager.getConnection(this.rdbURL, this.rdbUser, this.rdbPassword);
        } catch (ClassNotFoundException e) {
            throw new JPSRuntimeException("Failed to load driver for postgresql", e);
        }
    }

    /**
     * Executes the query supplied by the calling method and returns results
     * as a ResultSet
     * @param query
     * @return query result as a ResultSet
     */
    public ResultSet executeQuerytoResultSet(String query) {
        try (Connection conn = getConnection(); 
            Statement stmt = conn.createStatement(java.sql.ResultSet.TYPE_FORWARD_ONLY, java.sql.ResultSet.CONCUR_READ_ONLY)) {
            return stmt.executeQuery(query);
        } catch (SQLException e) {
            throw new JPSRuntimeException(ERR_PREFIX + "executeQuerytoResultSet failed", e);
        }
    }

    /**
     * Executes the query supplied by the calling method and returns results
     * as a JSONArray.
     * @param query
     * @return query result as a JSONArray
     */
    @Override
    public JSONArray executeQuery(String query){
        try (Connection conn = getConnection();
            Statement stmt = conn.createStatement(java.sql.ResultSet.TYPE_FORWARD_ONLY, java.sql.ResultSet.CONCUR_READ_ONLY);
            java.sql.ResultSet rs = stmt.executeQuery(query)) {
                return StoreClientHelper.convert(rs);
        } catch (SQLException e) {
            throw new JPSRuntimeException(ERR_PREFIX + "Failure at closing statement or connection", e);
        }
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
            throw new JPSRuntimeException(ERR_PREFIX + "Query result is null.");
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
        try (Connection conn = getConnection(); 
            Statement stmt = conn.createStatement(java.sql.ResultSet.TYPE_FORWARD_ONLY, java.sql.ResultSet.CONCUR_READ_ONLY)) {
            return stmt.executeUpdate(update);
        } catch (SQLException e) {
            throw new JPSRuntimeException(ERR_PREFIX + "Failed at closing connection/statement while executing update", e);
        }
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



