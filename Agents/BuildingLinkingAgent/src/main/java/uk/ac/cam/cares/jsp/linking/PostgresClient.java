package uk.ac.cam.cares.jsp.linking;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class PostgresClient {
    private static final Logger LOGGER = LogManager.getLogger(PostgresClient.class);
    private final String dbrul;
    private final String dbuser;
    private final String dbpassword;

    PostgresClient(String dbrul, String dbuser, String dbpassword) {
        this.dbrul = dbrul;
        this.dbuser = dbuser;
        this.dbpassword = dbpassword;
    }

    Connection getConnection() throws SQLException {
        try {
            Class.forName("org.postgresql.Driver");
        } catch (ClassNotFoundException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to find postgre driver");
        }
        return DriverManager.getConnection(this.dbrul, this.dbuser, this.dbpassword);
    }
}
