package uk.ac.cam.cares.jps.base.query;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class RDBStoreClient {

    private Connection conn;
    // URL and credentials for the relational database
    private String rdbURL;
    private String rdbUser;
    private String rdbPassword;

    public RDBStoreClient(String rdbURL, String user, String password) {
        this.rdbURL = rdbURL;
        this.rdbUser = user;
        this.rdbPassword = password;
    }

    public Connection getConnection(){
        try {
            Class.forName("org.postgresql.Driver");
            conn = DriverManager.getConnection(this.rdbURL, this.rdbUser, this.rdbPassword);
        } catch (SQLException | ClassNotFoundException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
        return conn;
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



