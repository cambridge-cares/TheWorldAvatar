package uk.ac.cam.cares.jps.base.query;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.sql.*;

public class RemoteRDBStoreClient {

    private Connection conn;
    Statement stmt;
    // URL and credentials for the relational database
    private String rdbURL;
    private String rdbUser;
    private String rdbPassword;

    public RemoteRDBStoreClient(String rdbURL, String user, String password) {
        this.rdbURL = rdbURL;
        this.rdbUser = user;
        this.rdbPassword = password;
    }

    public Connection getConnection(){
        try {
            Class.forName("org.postgresql.Driver");
            conn = DriverManager.getConnection(this.rdbURL, this.rdbUser, this.rdbPassword);
            stmt = conn.createStatement(java.sql.ResultSet.TYPE_FORWARD_ONLY, java.sql.ResultSet.CONCUR_READ_ONLY);
        } catch (SQLException | ClassNotFoundException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
        return conn;
    }

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



